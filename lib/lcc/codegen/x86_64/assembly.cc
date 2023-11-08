#include <lcc/codegen/mir.hh>
#include <lcc/codegen/x86_64.hh>
#include <lcc/codegen/x86_64/assembly.hh>
#include <lcc/context.hh>
#include <lcc/utils.hh>
#include <string>
#include <variant>

namespace lcc {
namespace x86_64 {

std::string ToString(MFunction& function, MOperand op) {
    if (std::holds_alternative<MOperandRegister>(op)) {
        // TODO: Assert that register id is one of the x86_64 register ids...
        MOperandRegister reg = std::get<MOperandRegister>(op);
        return fmt::format("%{}", ToString(RegisterId(reg.value), reg.size));
    } else if (std::holds_alternative<MOperandImmediate>(op)) {
        return fmt::format("${}", std::get<MOperandImmediate>(op));
    } else if (std::holds_alternative<MOperandLocal>(op)) {
        usz offset = 0;
        for (usz index = 0; index <= +std::get<MOperandLocal>(op); ++index)
            offset += function.locals().at(index)->allocated_type()->bytes();
        return fmt::format("{}(%rbp)", -isz(offset));
    } else if (std::holds_alternative<MOperandGlobal>(op)) {
        return fmt::format("{}(%rip)", std::get<MOperandGlobal>(op)->name());
    } else if (std::holds_alternative<MOperandFunction>(op)) {
        return fmt::format("{}", std::get<MOperandFunction>(op)->name());
    } else if (std::holds_alternative<MOperandBlock>(op)) {
        return fmt::format("{}", std::get<MOperandBlock>(op)->name());
    } else LCC_ASSERT(false, "Unhandled MOperand kind (index {})", op.index());
}

void emit_gnu_att_assembly(std::filesystem::path output_path, Module* module, std::vector<MFunction>& mir) {
    auto out = fmt::format("    .file \"{}\"\n", output_path.string());

    for (auto* var : module->vars()) {
        if (var->init()) {
            out += fmt::format("{}: ", var->name());
            switch (var->init()->kind()) {
                case Value::Kind::ArrayConstant: {
                    auto array_constant = as<ArrayConstant>(var->init());
                    out += fmt::format(".byte {}\n", fmt::join(vws::transform(*array_constant, [&](char c) {
                        return fmt::format("0x{:x}", int(c));
                    }), ","));
                } break;

                default:
                    LCC_ASSERT(false, "Sorry, but global variable initialisation with value kind {} is not supported.", Value::ToString(var->init()->kind()));
            }
            out += '\n';
        } else out += fmt::format("{}\n", var->name());
    }

    for (auto& function : mir) {
        if (function.linkage() == Linkage::Exported)
            out += fmt::format("    .globl {}\n", function.name());
        out += fmt::format("{}:\n", function.name());

        // Function Header
        // TODO: Different stack frame kinds.
        out +=
            "    push %rbp\n"
            "    mov %rsp, %rbp\n";

        for (auto& block : function.blocks()) {
            out += fmt::format("{}:\n", block.name());
            for (auto& instruction : block.instructions()) {
                // Don't move a register into itself.
                if (instruction.opcode() == +x86_64::Opcode::Move and instruction.all_operands().size() == 2) {
                    auto lhs = instruction.get_operand(0);
                    auto rhs = instruction.get_operand(1);
                    if (std::holds_alternative<MOperandRegister>(lhs) and std::holds_alternative<MOperandRegister>(rhs) and std::get<MOperandRegister>(lhs).value == std::get<MOperandRegister>(rhs).value)
                        continue;
                    ;
                }

                if (instruction.opcode() == +x86_64::Opcode::Return) {
                    // Function Footer
                    // TODO: Different stack frame kinds.
                    out += fmt::format(
                        "    mov %rbp, %rsp\n"
                        "    pop %rbp\n"
                    );
                }

                out += "    ";
                out += ToString(Opcode(instruction.opcode()));
                if (instruction.opcode() == +x86_64::Opcode::Move) {
                    if (std::holds_alternative<MOperandImmediate>(instruction.get_operand(0))) {
                        if (std::holds_alternative<MOperandLocal>(instruction.get_operand(1))) {
                            // Moving immediate into local (memory) requires mov suffix in GNU. We use
                            // size of local to determine how big of a move to do.
                            auto local_index = +std::get<MOperandLocal>(instruction.get_operand(1));
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
                    out += ToString(function, operand);
                    ++i;
                }
                out += '\n';
            }
        }
    }

    File::WriteOrTerminate(out.data(), out.size(), output_path);
}

} // namespace x86_64
} // namespace lcc
