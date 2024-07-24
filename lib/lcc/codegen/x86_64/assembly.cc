#include <lcc/codegen/mir.hh>
#include <lcc/codegen/mir_utils.hh>
#include <lcc/codegen/register_allocation.hh>
#include <lcc/codegen/x86_64/assembly.hh>
#include <lcc/codegen/x86_64/x86_64.hh>
#include <lcc/context.hh>
#include <lcc/core.hh>
#include <lcc/ir/ir.hh>
#include <lcc/utils.hh>

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
    LCC_ASSERT(not in.empty(), "safe_name does not handle empty string input");
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

void emit_gnu_att_assembly(
    const fs::path& output_path,
    Module* module,
    const MachineDescription& desc,
    std::vector<MFunction>& mir
) {
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

    for (auto* var : module->vars()) {
        // From GNU as manual: `.extern` is accepted in the source program--for
        // compatibility with other assemblers--but it is ignored. `as` treats all
        // undefined symbols as external. That's why imported is ignored.
        for (auto n : var->names()) {
            if (IsExportedLinkage(n.linkage))
                out += fmt::format("    .globl {}\n", n.name);
        }

        if (var->init()) {
            for (auto n : var->names())
                out += fmt::format("{}:\n", safe_name(n.name));
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
                    LCC_ASSERT(integer_constant->type()->bytes() <= 8, "Oversized integer constant");
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
            out += fmt::format("{}:\n", n.name);

        // TODO: Total hack just to try and get the source to show up at all in a
        // debugger. We would need real location information to properly do this.
        // Keep in mind that debug lines are 1-indexed.
        //   .loc <file-id> <line-number> [ <column-number> ]
        if (function.location().is_valid()) {
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

        // Function Header
        // TODO: Different stack frame kinds.
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
            out += fmt::format("    sub ${}, %rsp\n", stack_frame_size);
        }

        Location last_location{};
        for (auto [block_index, block] : vws::enumerate(function.blocks())) {
            out += fmt::format("{}:\n", block_name(block.name()));

            for (auto& instruction : block.instructions()) {
                // ================================
                // QUICK PATH OPTIMISATION (don't move a register into itself)
                // ================================
                if (
                    instruction.opcode() == +x86_64::Opcode::Move
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
                    // Function Footer
                    // TODO: Different stack frame kinds.
                    out +=
                        "    mov %rbp, %rsp\n"
                        "    pop %rbp\n";

                    // Update CFA expression since 16(%rbp) is no longer accurate.
                    out += "    .cfi_def_cfa %rsp, 8\n";

                } else if (instruction.opcode() == +x86_64::Opcode::Call) {
                    // Save return register, if necessary.
                    // TODO: CFA `.cfi_offset`
                    if (instruction.reg() != desc.return_register)
                        out += fmt::format("    push %{}\n", ToString(x86_64::RegisterId(desc.return_register)));
                }

                // ================================
                // INSTRUCTION DEBUG LOCATION
                // ================================
                {
                    auto loc = instruction.location();
                    if (loc.is_valid() and not loc.equal_position(last_location)) {
                        auto l = loc.seek_line_column(module->context());
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
                // CUSTOM OPERAND HANDLING (dereference register operand on rhs of move)
                // ================================
                if (
                    instruction.opcode() == +x86_64::Opcode::MoveDereferenceRHS
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

                    if (offset)
                        out += fmt::format(" {}, {}({})\n", ToString(function, lhs), offset, ToString(function, rhs));
                    else out += fmt::format(" {}, ({})\n", ToString(function, lhs), ToString(function, rhs));
                    continue;
                }
                // ================================
                // CUSTOM OPERAND HANDLING (dereference register operand on lhs of move)
                // ================================
                if (
                    instruction.opcode() == +x86_64::Opcode::MoveDereferenceLHS
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
                // CUSTOM INSTRUCTION SUFFIX HANDLING
                // ================================
                if (instruction.opcode() == +x86_64::Opcode::MoveDereferenceRHS) {
                    auto lhs = instruction.get_operand(0);
                    auto rhs = instruction.get_operand(1);
                    if (std::holds_alternative<MOperandImmediate>(lhs)) {
                        if (std::holds_alternative<MOperandLocal>(rhs)) {
                            // Moving immediate into local (memory) requires mov suffix in GNU.

                            // We use size of local to determine how big of a move to do.
                            // auto local_index = std::get<MOperandLocal>(rhs).index;
                            // auto* local = function.locals().at(local_index);
                            // auto bitwidth = local->allocated_type()->bits();

                            auto bitwidth = std::get<MOperandImmediate>(lhs).size;
                            switch (bitwidth) {
                                case 64: out += 'q'; break;
                                case 32: out += 'l'; break;
                                case 16: out += 'w'; break;
                                case 8: out += 'b'; break;
                                default: LCC_ASSERT(false, "Invalid move");
                            }
                        }
                    }
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
                if (instruction.opcode() == +x86_64::Opcode::Call) {
                    // Move return value from return register to result register, if necessary.
                    // Also restore return register, if necessary.
                    if (instruction.use_count() and instruction.reg() and instruction.reg() != desc.return_register) {
                        out += fmt::format(
                            "    mov %{}, %{}\n",
                            ToString(x86_64::RegisterId(desc.return_register), instruction.regsize()),
                            ToString(x86_64::RegisterId(instruction.reg()), instruction.regsize())
                        );
                        out += fmt::format("    pop %{}\n", ToString(x86_64::RegisterId(desc.return_register)));
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

    for (auto& section : module->extra_sections()) {
        out += fmt::format(".section {}\n", section.name);
        LCC_ASSERT(not section.is_fill, "Sorry, haven't handled fill extra sections");
        if (section.contents().empty()) continue;
        const auto write_byte = [&](u8 byte) {
            return fmt::format("0x{:x}", byte);
        };
        out += fmt::format(
            ".byte {}\n",
            fmt::join(vws::transform(section.contents(), write_byte), ",")
        );
    }

    out += ".section .note.GNU-stack\n";

    if (output_path == "-")
        fmt::print("{}", out);
    else File::WriteOrTerminate(out.data(), out.size(), output_path);
}

} // namespace lcc::x86_64
