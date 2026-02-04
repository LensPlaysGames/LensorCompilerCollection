#include <lcc/codegen/mir.hh>
#include <lcc/codegen/mir_utils.hh>
#include <lcc/codegen/register_allocation.hh>
#include <lcc/codegen/x86_64/assembly.hh>
#include <lcc/codegen/x86_64/x86_64.hh>
#include <lcc/context.hh>
#include <lcc/core.hh>
#include <lcc/ir/core.hh>
#include <lcc/utils.hh>
#include <lcc/utils/fractionals.hh>

#include <fmt/format.h>
#include <fmt/ranges.h>

#include <climits>
#include <filesystem>
#include <functional>
#include <ranges>
#include <string>
#include <type_traits>
#include <variant>
#include <vector>

namespace lcc::x86_64 {

namespace {

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

} // namespace

// FIXME: ToString is a bad name for this!
auto ToString(MFunction& function, MOperand op) -> std::string {
    static_assert(
        std::variant_size_v<MOperand> == 6,
        "Exhaustive handling of MOperand alternatives in x86_64 GNU Assembly backend"
    );
    if (std::holds_alternative<MOperandRegister>(op)) {
        // TODO: Assert that register id is one of the x86_64 register ids...
        MOperandRegister reg = std::get<MOperandRegister>(op);
        return fmt::format(
            "%{}",
            ToString(RegisterId(reg.value), reg.size)
        );
    }
    if (std::holds_alternative<MOperandImmediate>(op)) {
        return fmt::format(
            "${}",
            std::get<MOperandImmediate>(op).value
        );
    }
    if (std::holds_alternative<MOperandLocal>(op)) {
        return fmt::format(
            "{}(%rbp)",
            function.local_offset(std::get<MOperandLocal>(op))
        );
    }
    if (std::holds_alternative<MOperandGlobal>(op)) {
        return fmt::format(
            "{}(%rip)",
            safe_name(std::get<MOperandGlobal>(op)->names().at(0).name)
        );
    }
    if (std::holds_alternative<MOperandFunction>(op)) {
        return fmt::format(
            "{}",
            std::get<MOperandFunction>(op)->names().at(0).name
        );
    }
    if (std::holds_alternative<MOperandBlock>(op)) {
        return fmt::format(
            "{}",
            block_name(std::get<MOperandBlock>(op)->name())
        );
    }
    LCC_ASSERT(false, "Unhandled MOperand kind (index {})", op.index());
}

enum class StackFrameKind {
    Generate,
    Inherit,

    COUNT
};

// Function Header
void emit_stack_frame_entry(std::string& out, StackFrameKind k) {
    switch (k) {
        // push base pointer
        // mov stack pointer to base pointer
        case StackFrameKind::Generate:
            out += "    push %rbp\n";

            // Update CFA offset, as we now have changed the stack pointer (by 8).
            // `.cfi_def_cfa_offset` updates CFA offset to new expression, but not register.
            // `.cfi_offset` notifies saved register rbp location from CFA.
            out +=
                "    .cfi_def_cfa_offset 16\n"
                "    .cfi_offset %rbp, -16\n";

            out += "    mov %rsp, %rbp\n";

            // Update CFA register, as we now have stored the value of RSP in RBP.
            out += "    .cfi_def_cfa_register %rbp\n";

            return;

        case StackFrameKind::Inherit:
            return;

        case StackFrameKind::COUNT:
            LCC_UNREACHABLE();
    }
}

// Function Footer
void emit_stack_frame_exit(std::string& out, StackFrameKind k) {
    switch (k) {
        // mov base pointer to stack pointer
        // pop base pointer
        case StackFrameKind::Generate:
            out +=
                "    mov %rbp, %rsp\n"
                "    pop %rbp\n";

            // Update CFA expression since 16(%rbp) is no longer accurate.
            out += "    .cfi_def_cfa %rsp, 8\n";
            return;

        case StackFrameKind::Inherit:
            return;

        case StackFrameKind::COUNT:
            LCC_UNREACHABLE();
    }
}

bool operand_is_float(MOperand& op) {
    return std::holds_alternative<MOperandRegister>(op)
       and std::get<MOperandRegister>(op).category
               == Register::Category::FLOAT;
}

void emit_gnu_att_assembly(
    const fs::path& output_path,
    Module* module,
    const MachineDescription& desc,
    std::vector<MFunction>& mir
) {
    LCC_ASSERT(module and module->context());

    std::string out{};

    // If we ever add optional location information to the MIR (and some
    // eventually trickles through), this would allow somebody to step through
    // the source in a debugger like gdb.
    for (const auto& f : module->context()->files()) {
        out += fmt::format(
            "    .file {} \"{}\"\n",
            f->file_id(),
            fs::absolute(f->path()).string()
        );
    }

    // Exported globals need to be visible to external programs; we use the
    // .globl (global) directive for this.
    // Imported globals /could/ be declared with .extern, but GNU as ignores
    // these directives anyway, so we just don't emit them.
    for (auto* var : module->vars()) {
        for (auto n : var->names()) {
            if (IsExportedLinkage(n.linkage))
                out += fmt::format(
                    "    .globl {}\n"
                    "    .type {},@object\n"
                    "    .size {},{}\n",
                    n.name,
                    n.name,
                    n.name,
                    var->allocated_type()->bytes()
                );
        }
    }

    for (auto& function : mir) {
        bool imported{false};
        for (auto n : function.names()) {
            // From GNU as manual: `.extern` is accepted in the source program--for
            // compatibility with other assemblers--but it is ignored. `as` treats all
            // undefined symbols as external.
            if (IsExportedLinkage(n.linkage)) {
                out += fmt::format(
                    "    .globl {}\n"
                    "    .type {},@function\n",
                    n.name,
                    n.name
                );
            }

            if (IsImportedLinkage(n.linkage))
                imported = true;
        }
        if (imported) continue;

        for (auto n : function.names())
            out += fmt::format("{}:\n", safe_name(n.name));

        // READABILITY: Comments to denote locals and their offsets.
        if (function.locals().size()) {
            out += "# Locals:\n";
            for (auto [i, l] : vws::enumerate(function.locals())) {
                out += fmt::format(
                    "# {}, {}(%rbp): {} ({} size, {} align)\n",
                    i,
                    function.local_offset(MOperandLocal{(u32) i, 0}),
                    l->allocated_type()->string(false),
                    l->allocated_type()->bytes(),
                    l->allocated_type()->align_bytes()
                );
            }
        }

        // Keep in mind that debug lines are 1-indexed.
        //   .loc <file-id> <line-number> [ <column-number> ]
        if (function.location().seekable(module->context())) {
            auto l = function.location().seek_line_column(module->context());
            out += fmt::format(
                "    .loc {} {} {}\n",
                function.location().file_id,
                l.line,
                l.col
            );
        }

        // CFA (CIE starts it as %rsp+8)
        out += "    .cfi_startproc\n";

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

        auto frame_kind = StackFrameKind::Inherit;
        if (stack_frame_size)
            frame_kind = StackFrameKind::Generate;

        // READABILITY: Comments to denote spilled registers and their offsets.
        if (spill_offsets.size()) {
            out += "# Spilled Registers:\n";
            for (auto [id, offset] : spill_offsets) {
                if (spill_id_to_register.contains(id)) {
                    auto r = spill_id_to_register.at(id);
                    out += fmt::format(
                        "# ID {}, %{}, -{}(%rbp)\n",
                        id,
                        ToString((RegisterId) r.value, r.size),
                        offset
                    );
                } else out += fmt::format("# ID {}, -{}(%rbp)\n", id, offset);
            }
        }

        emit_stack_frame_entry(out, frame_kind);

        if (stack_frame_size) {
            constexpr usz alignment = 16;
            stack_frame_size = utils::AlignTo(stack_frame_size, alignment);
            out += fmt::format("    sub ${}, %rsp\n", stack_frame_size);
        }

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
                    (instruction.opcode() == +x86_64::Opcode::Move
                     or instruction.opcode() == +x86_64::Opcode::ScalarFloatMove)
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
                    and instruction.opcode() == +x86_64::Opcode::Jump
                    and is_block(instruction)
                ) {
                    auto* target_block = extract_block(instruction);
                    auto next_block = function.blocks().at(usz(block_index + 1));
                    if (target_block->name() == next_block.name())
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
                    auto mnemonic = std::string(ToString(Opcode(+x86_64::Opcode::MoveDereferenceRHS)));
                    // Append "sd" to mnemonic if saving scalar
                    // FIXME: is_scalar()
                    if (r.value >= +x86_64::RegisterId::XMM0)
                        mnemonic += "sd";
                    out += fmt::format(
                        "    {} {}, -{}(%rbp)\n",
                        mnemonic,
                        ToString(function, r),
                        spill_offsets.at(i.value)
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
                    // Append "sd" to mnemonic if saving scalar
                    // FIXME: is_scalar()
                    auto mnemonic = std::string(ToString(Opcode(+x86_64::Opcode::MoveDereferenceLHS)));
                    if (instruction.reg() >= +x86_64::RegisterId::XMM0)
                        mnemonic += "sd";

                    out += fmt::format(
                        "    {} -{}(%rbp), {}\n",
                        mnemonic,
                        spill_offsets.at(i.value),
                        ToString(
                            function,
                            MOperandRegister(instruction.reg(), (uint) instruction.regsize())
                        )
                    );
                    continue;
                }

                // ================================
                // CONFIDENCE CHECK (moves between registers must match sizes)
                // ================================
                if (
                    instruction.opcode() == +x86_64::Opcode::Move
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

                // ================================
                // TODO: CFA: Record saved registers
                // ================================
                if (
                    instruction.opcode() == +x86_64::Opcode::Push
                    and is_reg(instruction)
                ) {}

                // ================================
                // INSTRUCTION FIXUP
                //   movzx from 32 bit to 64 bit register is just a mov between 32 bit
                //   registers...
                // ================================
                if (
                    instruction.opcode() == +x86_64::Opcode::MoveZeroExtended
                    and is_reg_reg(instruction)
                ) {
                    auto [lhs, rhs] = extract_reg_reg(instruction);
                    if (lhs.size == 32 and rhs.size == 64) {
                        instruction.opcode(+x86_64::Opcode::Move);
                        rhs.size = 32;
                        instruction.all_operands().at(1) = rhs;
                    }
                }

                // Update 1-bit operations (boolean) to the minimum addressable on x86_64: a byte.
                if (instruction.regsize() == 1) instruction.regsize(8);

                // ================================
                // INSTRUCTION PROLOGUE (some insts have preceding instructions)
                // ================================
                if (instruction.opcode() == +x86_64::Opcode::Return) {
                    emit_stack_frame_exit(out, frame_kind);
                }

                // ================================
                // INSTRUCTION DEBUG LOCATION
                // ================================
                {
                    auto loc = instruction.location();
                    if (loc.seekable(module->context()) and not loc.equal_position(last_location)) {
                        auto l = loc.seek_line_column(module->context());
                        out += fmt::format(
                            "# FILE {} ({}), LINE {}, COLUMN {}\n",
                            loc.file_id,
                            fs::absolute(module->context()->files().at(loc.file_id)->path()).string(),
                            l.line,
                            l.col
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
                out += ToString(Opcode(instruction.opcode()));

                // ================================
                // CUSTOM INSTRUCTION SUFFIX HANDLING
                // ================================
                if (instruction.opcode() == +x86_64::Opcode::MoveDereferenceRHS) {
                    auto lhs = instruction.get_operand(0);
                    auto rhs = instruction.get_operand(1);
                    if (
                        std::holds_alternative<MOperandImmediate>(lhs)
                        and std::holds_alternative<MOperandLocal>(rhs)
                    ) {
                        // Moving immediate into local (memory) requires mov suffix in GNU.

                        // We use size of immediate to determine how big of a move to do.
                        auto bitwidth = std::get<MOperandImmediate>(lhs).size;
                        switch (bitwidth) {
                            default: LCC_ASSERT(false, "Invalid move (bitwidth {})", bitwidth);

                            case 64: out += 'q'; break;
                            case 32: out += 'l'; break;
                            case 16: out += 'w'; break;
                            case 1:
                            case 8: out += 'b'; break;
                        }
                    }
                }

                // FLOAT: "ss" or "sd" suffix
                else if (
                    (instruction.opcode() > +x86_64::Opcode::ScalarFloatFENCEBegin
                     and instruction.opcode() < +x86_64::Opcode::ScalarFloatFENCEEnd)
                    and instruction.all_operands().size() == 2
                ) {
                    auto lhs = instruction.get_operand(0);
                    auto rhs = instruction.get_operand(1);
                    if (operand_is_float(lhs) or operand_is_float(rhs)) {
                        usz bitwidth = 0;
                        if (operand_is_float(lhs))
                            bitwidth = std::get<MOperandRegister>(lhs).size;
                        else bitwidth = std::get<MOperandRegister>(rhs).size;
                        LCC_ASSERT(bitwidth);
                        switch (bitwidth) {
                            default:
                                Diag::ICE(
                                    "Float bitwidths must be 64 or 32 on x86_64..."
                                );
                            case 64: out += 'd'; break;
                            case 32: out += 's'; break;
                        }
                    }
                }

                // ================================
                // CUSTOM OPERAND HANDLING (dereference register operand on rhs of move)
                // ================================
                if (
                    (instruction.opcode() == +x86_64::Opcode::MoveDereferenceRHS
                     or instruction.opcode() == +x86_64::Opcode::ScalarFloatMoveDereferenceRHS)
                    and std::holds_alternative<MOperandRegister>(instruction.get_operand(1))
                ) {
                    auto lhs = instruction.get_operand(0);
                    auto rhs = instruction.get_operand(1);

                    usz offset = 0;
                    if (instruction.all_operands().size() == 3) {
                        auto given_offset = instruction.get_operand(2);
                        LCC_ASSERT(
                            std::holds_alternative<MOperandImmediate>(given_offset),
                            "Offset operand of dereferencing move must be an immediate"
                        );
                        offset = std::get<MOperandImmediate>(given_offset).value;
                    }

                    if (offset) {
                        out += fmt::format(
                            " {}, {}({})\n",
                            ToString(function, lhs),
                            offset,
                            ToString(function, rhs)
                        );
                    } else {
                        out += fmt::format(
                            " {}, ({})\n",
                            ToString(function, lhs),
                            ToString(function, rhs)
                        );
                    }
                    continue;
                }

                // ================================
                // CUSTOM OPERAND HANDLING (dereference register operand on lhs of move)
                // ================================
                if (
                    (instruction.opcode() == +x86_64::Opcode::MoveDereferenceLHS
                     or instruction.opcode() == +x86_64::Opcode::ScalarFloatMoveDereferenceLHS)
                    and std::holds_alternative<MOperandRegister>(instruction.get_operand(0))
                ) {
                    auto lhs = instruction.get_operand(0);
                    auto rhs = instruction.get_operand(1);
                    usz offset = 0;
                    if (instruction.all_operands().size() == 3) {
                        auto given_offset = instruction.get_operand(2);
                        LCC_ASSERT(
                            std::holds_alternative<MOperandImmediate>(given_offset),
                            "Offset operand of dereferencing move must be an immediate"
                        );
                        offset = std::get<MOperandImmediate>(given_offset).value;
                    }
                    if (offset)
                        out += fmt::format(" {}({}), {}\n", offset, ToString(function, lhs), ToString(function, rhs));
                    else out += fmt::format(" ({}), {}\n", ToString(function, lhs), ToString(function, rhs));
                    continue;
                }

                // ================================
                // INSTRUCTION OPERANDS
                // ================================
                usz i = 0;
                for (auto& operand : instruction.all_operands()) {
                    if (i == 0) out += ' ';
                    else out += ", ";
                    // Update 1-bit operations (boolean) to the minimum addressable on x86_64: a byte.
                    if (std::holds_alternative<MOperandRegister>(operand) and std::get<MOperandRegister>(operand).size == 1) {
                        auto tmp = std::get<MOperandRegister>(operand);
                        tmp.size = 8;
                        operand = tmp;
                    }
                    out += ToString(function, operand);
                    ++i;
                }
                out += '\n';

                // ================================
                // INSTRUCTION EPILOGUE (some insts have instructions following)
                // ================================
                const auto return_register_by_category = [&](usz category) {
                    for (auto regcategory : desc.return_registers) {
                        if (regcategory.category == category)
                            return regcategory.registers.at(0);
                    }
                    fmt::print(
                        stderr,
                        "Could not find return register for category {}\n",
                        +category
                    );
                    LCC_UNREACHABLE();
                };

                if (instruction.opcode() == +x86_64::Opcode::Call) {
                    // Move return value from return register to result register, if necessary.
                    if (
                        instruction.use_count() and instruction.reg()
                        and instruction.reg() != return_register_by_category(instruction.regcategory())
                    ) {
                        out += fmt::format(
                            "    mov %{}, %{}\n",
                            ToString(
                                x86_64::RegisterId(return_register_by_category(instruction.regcategory())),
                                instruction.regsize()
                            ),
                            ToString(x86_64::RegisterId(instruction.reg()), instruction.regsize())
                        );
                    }
                }
            }
        }

        out += fmt::format("    .cfi_endproc\n");

        for (auto n : function.names()) {
            if (IsExportedLinkage(n.linkage))
                out += fmt::format("    .size {}, .-{}\n", n.name, n.name);
        }
    }

    // Emit initialized global variable definitions in .data
    bool init_vars_present{false};
    for (auto* var : module->vars()) {
        if (not var->init()) continue;

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
                    integer_constant->type()->bytes() <= x86_64::GeneralPurposeBytewidth,
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

            default:
                LCC_ASSERT(
                    false,
                    "Sorry, but global variable initialisation with value kind {} is not supported.",
                    Value::ToString(var->init()->kind())
                );
        }
        out += '\n';
    }

    // Emit uninitialized global variable definitions in .bss
    bool uninit_vars_present{false};
    for (auto* var : module->vars()) {
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

    for (auto& section : module->extra_sections()) {
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

    out += ".section .note.GNU-stack\n";

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

} // namespace lcc::x86_64
