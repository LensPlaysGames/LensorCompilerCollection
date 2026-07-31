#include <lcc/codegen/aarch64/aarch64.hh>

#include <lcc/codegen/mir.hh>
#include <lcc/codegen/mir_utils.hh>
#include <lcc/codegen/register_allocation.hh>
#include <lcc/ir/core.hh>
#include <lcc/target.hh>
#include <lcc/version.hh>

#include <lccbase/context.hh>
#include <lccbase/file.hh>

#include <filesystem>
#include <vector>

namespace lcc::aarch64 {

namespace {

static constexpr auto comment_begin = ";#";

auto comment(std::string_view in) {
    while (in.ends_with('\n'))
        in.remove_suffix(1);
    return fmt::format("{} {}\n", comment_begin, in);
}

// NOTE: Does not handle empty string (because we want every input of this
// function to produce the same output)
auto safe_name(std::string in) -> std::string {
    LCC_ASSERT(
        not in.empty(),
        "safe_name does not handle empty string input"
    );
    // . in the middle of an identifier is not allowed
    std::replace(in.begin(), in.end(), '.', '_');
    return fmt::format("{}", in);
}

auto block_name(const std::string& in) -> std::string {
    LCC_ASSERT(not in.empty(), "Cannot emit empty block name!");
    // ".L" at the beginning tells the assembler it's a local label and not a
    // function, which helps objdump and things like that don't get confused.
    return fmt::format(".L{}", safe_name(in));
}

constexpr auto gnu_mnemonic(Opcode opcode) -> std::string {
    return StringifyEnum(opcode);
}

auto emit_operand(MFunction& function, MOperand op) -> std::string {
    static_assert(
        std::variant_size_v<MOperand> == 6,
        "Exhaustive handling of MOperand alternatives in aarch64 GNU Assembly backend"
    );
    if (std::holds_alternative<MOperandRegister>(op)) {
        // TODO: Assert that register id is one of the x86_64 register ids...
        MOperandRegister reg = std::get<MOperandRegister>(op);
        return ToString(RegisterId(reg.value), reg.size);
    }

    if (std::holds_alternative<MOperandImmediate>(op)) {
        return fmt::format(
            "#{}",
            std::get<MOperandImmediate>(op).value
        );
    }

    if (std::holds_alternative<MOperandLocal>(op)) {
        return fmt::format(
            "[x29, #{}]",
            function.local_offset(std::get<MOperandLocal>(op))
        );
    }

    if (std::holds_alternative<MOperandGlobal>(op))
        Diag::ICE("Cannot reference global directly in aarch64, must use adrp + add :lo12:. Likely a bug in IR/MIR lowering");

    if (std::holds_alternative<MOperandFunction>(op))
        return std::get<MOperandFunction>(op)->names().at(0).name;

    if (std::holds_alternative<MOperandBlock>(op))
        return block_name(std::get<MOperandBlock>(op)->name());

    Diag::ICE("Unhandled MOperand kind (index {})", op.index());
}

void emit_context_files_info(std::string& out, Context& context) {
    for (const auto& f : context.files()) {
        out += fmt::format(
            "    .file {} \"{}\"\n",
            f->file_id(),
            fs::absolute(f->path()).string()
        );
    }
}

void emit_globals_info(std::string& out, Module& mod) {
    // Exported globals need to be visible to external programs; we use the
    // .globl (global) directive for this.
    // Imported globals /could/ be declared with .extern, but GNU as ignores
    // these directives anyway, so we just don't emit them.
    for (auto& var : mod.vars()) {
        for (auto n : var->names()) {
            if (IsExportedLinkage(n.linkage)) {
                out += fmt::format("    .globl {}\n", n.name);
            }
        }
    }
}

// Function Header
void emit_stack_frame_entry(std::string& out, usz stack_frame_size) {
    // Align stack to 16 bytes
    {
        constexpr usz alignment = 16;
        stack_frame_size = utils::AlignTo(stack_frame_size, alignment);
    }

    out += fmt::format(
        "    sub sp, sp, #{}\n"
        "    stp x29, x30, [sp]\n"
        "    mov x29, sp\n",
        stack_frame_size
    );
}

// Function Footer
void emit_stack_frame_exit(std::string& out, usz stack_frame_size) {
    // mov base pointer to stack pointer
    // pop base pointer
    out += fmt::format(
        "    ldp x29, x30, [sp]\n"
        "    add sp, sp, #{}\n",
        stack_frame_size
    );
}

void emit_function_bodies(
    std::string& out,
    Module& mod,
    std::vector<MFunction>& mir
) {
    for (auto& function : mir) {
        bool imported{false};

        for (auto n : function.names()) {
            // From GNU as manual: `.extern` is accepted in the source program--for
            // compatibility with other assemblers--but it is ignored. `as` treats all
            // undefined symbols as external.
            if (IsExportedLinkage(n.linkage))
                out += fmt::format("    .globl {}\n", n.name);

            if (IsImportedLinkage(n.linkage))
                imported = true;
        }
        if (imported) continue;

        // Emit labels for all function names.
        for (auto n : function.names())
            out += fmt::format("{}:\n", safe_name(n.name));

        // TODO: Comments to denote locals and their offsets.

        // Location information, if applicable
        // Keep in mind that debug lines are 1-indexed.
        //   .loc <file-id> <line-number> [ <column-number> ]
        if (function.location().seekable(mod.context())) {
            auto l = function.location().seek_line_column(mod.context());
            out += fmt::format(
                "    .loc {} {} {}\n",
                function.location().file_id,
                l.line,
                l.col
            );
        }

        // Calculate stack frame size; this is the sum of the size of all locals
        // and the size of all spilled registers.

        // Sum locals sizes
        usz stack_frame_size = rgs::fold_left(
            vws::transform(function.locals(), [](AllocaInst* l) {
                return l->allocated_type()->bytes();
            }),
            0,
            std::plus{}
        );

        if (stack_frame_size) {
            constexpr usz alignment = 16;
            stack_frame_size = utils::AlignTo(stack_frame_size, alignment);
        }

        // Sum spilled registers' sizes, keeping track of their frame offsets.
        // TODO: These lists are so small, vectors would improve performance
        // simply due to cache locality and memory footprint.
        std::unordered_map<usz, usz> spill_offsets{};
        std::unordered_map<usz, MOperandRegister> spill_id_to_register{};
        for (auto& block : function.blocks()) {
            for (auto& instruction : block.instructions()) {
                if (instruction.opcode() == +MInst::Kind::Spill) {
                    auto& r = std::get<MOperandRegister>(
                        instruction.all_operands().at(0)
                    );
                    auto i = std::get<MOperandImmediate>(
                        instruction.all_operands().at(1)
                    );

                    // Unique spills only
                    if (spill_offsets.contains(i.value))
                        continue;

                    LCC_ASSERT(
                        r.size % 8 == 0,
                        "Invalid spilled register size"
                    );
                    stack_frame_size += r.size / 8;
                    spill_offsets[i.value] = stack_frame_size;

                    spill_id_to_register[i.value] = r;
                }
            }
        }

        // TODO: Stack frame kind (inherit vs generate)

        // TODO: Comments to denote spilled registers and their offsets.

        emit_stack_frame_entry(out, stack_frame_size);

        Location last_location{};
        for (auto [block_index, block] : vws::enumerate(function.blocks())) {
            out += fmt::format("{}:\n", block_name(block.name()));

            for (auto& instruction : block.instructions()) {
                // ================================
                // QUICK PATH OPTIMISATION (don't move a register into itself)
                // ================================
                // Sometimes, the compiler can seem kind of dumb, as it produces
                // instructions that actually don't do anything. In this case, the
                // compiler converts virtual registers to hardware ones, sometimes the
                // same hardware ones, and the moves between virtual registers become
                // moves between the same register, effectively doing nothing.
                // Since these instructions do nothing, we just don't emit them.
                if (
                    instruction.opcode() == +Opcode::Move
                    and is_reg_reg(instruction)
                ) {
                    auto [lhs, rhs] = extract_reg_reg(instruction);
                    if (lhs.value == rhs.value)
                        continue;
                }

                // ================================
                // QUICK PATH OPTIMISATION (simple jump threading)
                // ================================
                if (
                    &block != &function.blocks().back()
                    and instruction.opcode() == +Opcode::Branch
                    and is_block(instruction)
                ) {
                    auto* target_block = extract_block(instruction);
                    auto next_block = function.blocks().at(usz(block_index + 1));
                    if (target_block->name() == next_block.name())
                        continue;
                }

                // ================================
                // COPY
                // ================================
                if (instruction.opcode() == +MInst::Kind::Copy) {
                    // Move register operand into result register.
                    auto& src = std::get<MOperandRegister>(
                        instruction.all_operands().at(0)
                    );
                    LCC_ASSERT(src.size % 8 == 0, "Invalid copied source register size");

                    auto dst = Register{
                        instruction.reg(),
                        (uint) instruction.regsize(),
                        (Register::Category) instruction.regcategory(),
                        instruction.is_defining()
                    };
                    LCC_ASSERT(dst.size % 8 == 0, "Invalid copied destination register size");

                    auto mnemonic = gnu_mnemonic(Opcode(+Opcode::Move));
                    out += fmt::format(
                        "    {} {}, {}  {} COPY\n",
                        mnemonic,
                        ToString((RegisterId) dst.value, dst.size),
                        ToString((RegisterId) src.value, src.size),
                        comment_begin
                    );
                    continue;
                }
                // ================================
                // SPILL
                // ================================
                if (instruction.opcode() == +MInst::Kind::Spill) {
                    // Store register operand onto the stack at "spill offset - reg.size"
                    auto& r = std::get<MOperandRegister>(
                        instruction.all_operands().at(0)
                    );
                    // slot
                    auto i = std::get<MOperandImmediate>(
                        instruction.all_operands().at(1)
                    );
                    LCC_ASSERT(r.size % 8 == 0, "Invalid spilled register size");
                    auto mnemonic = gnu_mnemonic(Opcode(+Opcode::Store));
                    out += fmt::format(
                        "    {} {}, [{}, #-{}]  {} SPILL (slot {})\n",
                        mnemonic,
                        ToString((RegisterId) r.value, (uint) r.size),
                        // x29 -> frame pointer
                        ToString(
                            RegisterId::R29,
                            64
                        ),
                        spill_offsets.at(i.value),
                        comment_begin,
                        i.value
                    );
                    continue;
                }
                // ================================
                // UNSPILL
                // ================================
                if (instruction.opcode() == +MInst::Kind::Unspill) {
                    // slot
                    auto i = std::get<MOperandImmediate>(
                        instruction.all_operands().at(0)
                    );
                    auto mnemonic = gnu_mnemonic(Opcode(+Opcode::Load));
                    out += fmt::format(
                        "    {} {}, [{}, #-{}]  {} UNSPILL (slot {})\n",
                        mnemonic,
                        ToString(
                            (RegisterId) instruction.reg(),
                            (uint) instruction.regsize()
                        ),
                        // x29 -> frame pointer
                        ToString(
                            RegisterId::R29,
                            64
                        ),
                        spill_offsets.at(i.value),
                        comment_begin,
                        i.value
                    );
                    continue;
                }

                // ================================
                // CONFIDENCE CHECK (moves between registers must match sizes)
                // ================================
                if (
                    instruction.opcode() == +Opcode::Move
                    and is_reg_reg(instruction)
                ) {
                    auto [lhs, rhs] = extract_reg_reg(instruction);
                    if (lhs.size != rhs.size) {
                        Diag::ICE(
                            "Move from register to register has mismatched sizes in basic block {} in function {}",
                            block.name(),
                            function.names().at(0).name
                        );
                    }
                }

                // Update 1-bit operations (boolean) to the minimum addressable: a byte.
                if (instruction.regsize() == 1)
                    instruction.regsize(8);

                // ================================
                // INSTRUCTION PROLOGUE (some insts have preceding instructions)
                // ================================
                if (instruction.opcode() == +Opcode::Return)
                    emit_stack_frame_exit(out, stack_frame_size);

                // ================================
                // INSTRUCTION DEBUG LOCATION
                // ================================
                {
                    auto loc = instruction.location();
                    if (loc.seekable(mod.context()) and not loc.equal_position(last_location)) {
                        auto l = loc.seek_line_column(mod.context());
                        out += comment(
                            fmt::format(
                                "FILE {} ({}), LINE {}, COLUMN {}",
                                loc.file_id,
                                fs::absolute(mod.context()->files().at(loc.file_id)->path())
                                    .string(),
                                l.line,
                                l.col
                            )
                        );
                        out += fmt::format(
                            "    .loc {} {} {}\n",
                            loc.file_id,
                            l.line,
                            l.col
                        );
                        last_location = loc;
                    }
                }

                // ================================
                // INSTRUCTION MNEMONIC
                // ================================
                out += "    ";
                out += gnu_mnemonic(Opcode(instruction.opcode()));

                // ================================
                // INSTRUCTION OPERANDS
                // ================================
                usz i = 0;
                for (auto& operand : instruction.all_operands()) {
                    if (i == 0) out += ' ';
                    else out += ", ";
                    // Update 1-bit operations (boolean) to the minimum addressable on x86_64: a byte.
                    if (
                        std::holds_alternative<MOperandRegister>(operand)
                        and std::get<MOperandRegister>(operand).size == 1
                    ) {
                        auto tmp = std::get<MOperandRegister>(operand);
                        tmp.size = 8;
                        operand = tmp;
                    }
                    out += emit_operand(function, operand);
                    ++i;
                }
                out += '\n';
            }
        }
    }
}

void emit_init_vars(std::string& out, Module& mod) {
    bool init_vars_present{false};
    for (auto& var : mod.vars()) {
        if (not var->init())
            continue;

        bool defines{false};
        for (auto n : var->names()) {
            if (not IsImportedLinkage(n.linkage)) {
                if (not init_vars_present) {
                    out += "    .section .data\n";
                    init_vars_present = true;
                }
                out += fmt::format("{}:\n", safe_name(n.name));
                defines = true;
            }
        }
        if (not defines) continue;

        LCC_ASSERT(var->init());
        LCC_ASSERT(defines);

        switch (var->init()->kind()) {
            default:
                Diag::ICE(
                    "The aarch64 Assembly Backend does not handle global variable initialisation from value kind {}\n"
                    "It seems the global variable {} has an unsupported initialiser\n"
                    "{}",
                    Value::ToString(var->init()->kind()),
                    vws::transform(var->names(), [](const auto& n) {
                        return n.name;
                    }),
                    var->init()->string()
                );

            case Value::Kind::ArrayConstant: {
                auto* array_constant = as<ArrayConstant>(var->init());
                out += fmt::format(
                    "    .byte {}\n",
                    fmt::join(
                        vws::transform(*array_constant, [&](char c) {
                            return fmt::format("0x{:x}", int(c));
                        }),
                        ","
                    )
                );
            } break;

            case Value::Kind::IntegerConstant: {
                auto* integer_constant = as<IntegerConstant>(var->init());
                LCC_ASSERT(
                    integer_constant->type()->bytes() <= aarch64::GeneralPurposeBytewidth,
                    "Oversized integer constant"
                );
                // Represent bytes literally
                out += "    .byte ";
                u64 value = integer_constant->value().value();
                for (usz i = 0; i < integer_constant->type()->bytes(); ++i) {
                    int byte = (value >> (i * 8)) & 0xff;
                    out += fmt::format("0x{:x}", byte);
                    if (i + 1 < integer_constant->type()->bytes())
                        out += ", ";
                }
            } break;
        }
        out += '\n';
    }
}

void emit_uninit_vars(std::string& out, Module& mod) {
    bool uninit_vars_present{false};
    for (auto& var : mod.vars()) {
        if (var->init()) continue;

        bool defines{false};
        for (auto n : var->names()) {
            if (n.linkage == Linkage::Exported) {
                if (not uninit_vars_present) {
                    out += ".section .bss\n";
                    uninit_vars_present = true;
                }
                // Only emit align directive once, even if there are multiple exported
                // names.
                if (not defines) {
                    out += fmt::format(
                        ".align {}\n",
                        var->allocated_type()->align_bytes()
                    );
                }

                // If safe_name breaks the identifier, well, there's not much we can do,
                // since the identifier cannot be represented in the output format...
                out += fmt::format("{}:\n", safe_name(n.name));
                defines = true;
            }
        }
        if (not defines) continue;

        LCC_ASSERT(uninit_vars_present);
        LCC_ASSERT(not var->init());
        LCC_ASSERT(defines);

        out += fmt::format(
            ".zero {}\n",
            var->allocated_type()->bytes()
        );
    }
}

void emit_extra_sections(std::string& out, Module& mod) {
    for (auto& section : mod.extra_sections()) {
        out += fmt::format(".section {}\n", section.name);
        LCC_ASSERT(
            not section.is_fill,
            "Sorry, haven't handled fill extra sections"
        );
        if (section.contents().empty())
            continue;

        const auto write_byte = [&](u8 byte) {
            return fmt::format("0x{:x}", byte);
        };
        out += fmt::format(
            ".byte {}\n",
            fmt::join(vws::transform(section.contents(), write_byte), ",")
        );
    }
}

} // namespace

void emit_gnu_assembly(
    const fs::path& output_path,
    lcc::Module* mod,
    const MachineDescription& machine_desc,
    std::vector<MFunction>& mir
) {
    LCC_ASSERT(mod and mod->context());

    std::string out{};

    // File information for source location directives
    emit_context_files_info(out, *mod->context());

    // Exported globals need to be visible to external programs
    emit_globals_info(out, *mod);

    emit_function_bodies(out, *mod, mir);

    emit_init_vars(out, *mod);
    emit_uninit_vars(out, *mod);

    emit_extra_sections(out, *mod);

    out += ".ident \"" LCC_IDENT "\"\n";

    if (output_path.empty() or output_path == "-")
        fmt::print("{}", out);
    else {
        File::WriteOrTerminate(
            out.data(),
            out.size(),
            output_path
        );
    }
}

} // namespace lcc::aarch64
