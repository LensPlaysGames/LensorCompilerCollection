#include <lcc/codegen/mir.hh>
#include <lcc/codegen/register_allocation.hh>
#include <lcc/codegen/x86_64/assembly.hh>
#include <lcc/codegen/x86_64/x86_64.hh>
#include <lcc/context.hh>
#include <lcc/ir/ir.hh>
#include <lcc/utils.hh>
#include <string>
#include <type_traits>
#include <variant>

namespace lcc {
namespace x86_64 {

// NOTE: Does not handle empty string (because we want every input of this
// function to produce the same output)
static std::string safe_name(std::string in) {
    LCC_ASSERT(not in.empty(), "safe_name does not handle empty string input");
    // . in the middle of an identifier is not allowed
    std::replace(in.begin(), in.end(), '.', '_');
    return fmt::format("{}", in);
}

static std::string block_name(std::string in) {
    if (in.empty()) {
        static usz block_count = 0;
        return fmt::format(".__block_{}", block_count++);
    }
    // . at the beginning tells the assembler it's a local label and not a
    // function.
    return fmt::format(".{}", safe_name(in));
}

std::string ToString(MFunction& function, MOperand op) {
    if (std::holds_alternative<MOperandRegister>(op)) {
        // TODO: Assert that register id is one of the x86_64 register ids...
        MOperandRegister reg = std::get<MOperandRegister>(op);
        return fmt::format("%{}", ToString(RegisterId(reg.value), reg.size));
    } else if (std::holds_alternative<MOperandImmediate>(op)) {
        return fmt::format("${}", std::get<MOperandImmediate>(op).value);
    } else if (std::holds_alternative<MOperandLocal>(op)) {
        usz offset = 0;
        for (usz index = 0; index <= +std::get<MOperandLocal>(op); ++index)
            offset += function.locals().at(index)->allocated_type()->bytes();
        return fmt::format("{}(%rbp)", -isz(offset));
    } else if (std::holds_alternative<MOperandGlobal>(op)) {
        return fmt::format("{}(%rip)", safe_name(std::get<MOperandGlobal>(op)->name()));
    } else if (std::holds_alternative<MOperandFunction>(op)) {
        return fmt::format("{}", std::get<MOperandFunction>(op)->name());
    } else if (std::holds_alternative<MOperandBlock>(op)) {
        return fmt::format("{}", block_name(std::get<MOperandBlock>(op)->name()));
    } else LCC_ASSERT(false, "Unhandled MOperand kind (index {})", op.index());
}

void emit_gnu_att_assembly(std::filesystem::path output_path, Module* module, const MachineDescription& desc, std::vector<MFunction>& mir) {
    auto out = fmt::format("    .file \"{}\"\n", output_path.string());

    for (auto* var : module->vars()) {
        if (var->init()) {
            out += fmt::format("{}: ", safe_name(var->name()));
            switch (var->init()->kind()) {
                case Value::Kind::ArrayConstant: {
                    auto array_constant = as<ArrayConstant>(var->init());
                    out += fmt::format(
                        ".byte {}\n",
                        fmt::join(
                            vws::transform(*array_constant, [&](char c) {
                                return fmt::format("0x{:x}", int(c));
                            }),
                            ","
                        )
                    );
                } break;

                case Value::Kind::IntegerConstant: {
                    auto integer_constant = as<IntegerConstant>(var->init());
                    LCC_ASSERT(integer_constant->type()->bytes() <= 8, "Oversized integer constant");
                    // Represent bytes literally
                    out += ".byte ";
                    u64 value = integer_constant->value().value();
                    for (usz i = 0; i < integer_constant->type()->bytes(); ++i) {
                        int byte = (value >> (i * 8)) & 0xff;
                        out += fmt::format("0x{:x}", byte);
                        if (i + 1 < integer_constant->type()->bytes())
                            out += ", ";
                    }
                    out += '\n';
                } break;

                default:
                    LCC_ASSERT(false, "Sorry, but global variable initialisation with value kind {} is not supported.", Value::ToString(var->init()->kind()));
            }
            out += '\n';
        }
        // From GNU as manual: `.extern` is accepted in the source program--for
        // compatibility with other assemblers--but it is ignored. `as` treats all
        // undefined symbols as external.
        if (var->linkage() != Linkage::Imported)
            out += fmt::format(".globl {}\n", var->name());
    }

    for (auto& function : mir) {
        if (function.linkage() == Linkage::Exported || function.linkage() == Linkage::Reexported)
            out += fmt::format("    .globl {}\n", function.name());

        if (function.linkage() == Linkage::Imported || function.linkage() == Linkage::Reexported) {
            out += fmt::format("    .extern {}\n", function.name());
            continue;
        }

        out += fmt::format("{}:\n", function.name());

        // Function Header
        // TODO: Different stack frame kinds.
        out +=
            "    push %rbp\n"
            "    mov %rsp, %rbp\n";

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

        for (auto [block_index, block] : vws::enumerate(function.blocks())) {
            out += fmt::format("{}:\n", block_name(block.name()));

            for (auto& instruction : block.instructions()) {
                // Don't move a register into itself.
                if (instruction.opcode() == +x86_64::Opcode::Move and instruction.all_operands().size() == 2) {
                    auto lhs = instruction.get_operand(0);
                    auto rhs = instruction.get_operand(1);
                    if (std::holds_alternative<MOperandRegister>(lhs)
                        and std::holds_alternative<MOperandRegister>(rhs)
                        and std::get<MOperandRegister>(lhs).value == std::get<MOperandRegister>(rhs).value)
                        continue;
                }
                if (instruction.opcode() == +x86_64::Opcode::Move and instruction.all_operands().size() == 2) {
                    auto lhs = instruction.get_operand(0);
                    auto rhs = instruction.get_operand(1);
                    if (
                        std::holds_alternative<MOperandRegister>(lhs)
                        and std::holds_alternative<MOperandRegister>(rhs)
                        and std::get<MOperandRegister>(lhs).size != std::get<MOperandRegister>(rhs).size
                    ) Diag::ICE("Move from register to register has mismatched sizes");
                }

                // Simple jump threading
                // clang-format off
                if (&block != &function.blocks().back()
                    and instruction.opcode() == +x86_64::Opcode::Jump
                    and instruction.all_operands().size() == 1
                    and std::holds_alternative<MOperandBlock>(instruction.get_operand(0))) {
                    // clang-format on
                    auto target_block = std::get<MOperandBlock>(instruction.get_operand(0));
                    auto next_block = function.blocks().at(usz(block_index + 1));
                    if (target_block->name() == next_block.name())
                        continue;
                }

                // Update 1-bit operations (boolean) to the minimum addressable on x86_64: a byte.
                if (instruction.regsize() == 1) instruction.regsize(8);

                if (instruction.opcode() == +x86_64::Opcode::Return) {
                    // Function Footer
                    // TODO: Different stack frame kinds.
                    out +=
                        "    mov %rbp, %rsp\n"
                        "    pop %rbp\n";
                } else if (instruction.opcode() == +x86_64::Opcode::Call) {
                    // Save return register, if necessary.
                    if (instruction.reg() != desc.return_register)
                        out += fmt::format("    push %{}\n", ToString(x86_64::RegisterId(desc.return_register)));
                }

                out += "    ";
                out += ToString(Opcode(instruction.opcode()));
                if (instruction.opcode() == +x86_64::Opcode::MoveDereferenceRHS and std::holds_alternative<MOperandRegister>(instruction.get_operand(1))) {
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
                if (instruction.opcode() == +x86_64::Opcode::MoveDereferenceLHS and std::holds_alternative<MOperandRegister>(instruction.get_operand(0))) {
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
                if (instruction.opcode() == +x86_64::Opcode::MoveDereferenceRHS) {
                    auto lhs = instruction.get_operand(0);
                    auto rhs = instruction.get_operand(1);
                    if (std::holds_alternative<MOperandImmediate>(lhs)) {
                        if (std::holds_alternative<MOperandLocal>(rhs)) {
                            // Moving immediate into local (memory) requires mov suffix in GNU. We use
                            // size of local to determine how big of a move to do.
                            auto local_index = +std::get<MOperandLocal>(rhs);
                            auto* local = function.locals().at(local_index);
                            auto bitwidth = local->allocated_type()->bits();
                            switch (bitwidth) {
                                case 64: out += 'q'; break;
                                case 32: out += 'd'; break;
                                case 16: out += 'w'; break;
                                case 8: out += 'b'; break;
                                default: LCC_ASSERT(false, "Invalid move");
                            }
                        }
                    }
                }
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

                if (instruction.opcode() == +x86_64::Opcode::Call) {
                    // Move return value from return register to result register, if necessary.
                    // Also restore return register, if necessary.
                    if (instruction.reg() and instruction.reg() != desc.return_register) {
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
    }

    for (auto& section : module->extra_sections()) {
        out += fmt::format(".section {}\n", section.name);
        LCC_ASSERT(not section.is_fill, "Sorry, haven't handled fill extra sections");
        if (section.contents().empty()) continue;
        const auto write_byte = [&](u8 byte) {
            return fmt::format("0x{:x}", byte);
        };
        out += fmt::format(".byte {}\n", fmt::join(vws::transform(section.contents(), write_byte), ","));
    }

    out += ".section .note.GNU-stack\n";

    if (output_path == "-")
        fmt::print("{}", out);
    else File::WriteOrTerminate(out.data(), out.size(), output_path);
}

} // namespace x86_64
} // namespace lcc
