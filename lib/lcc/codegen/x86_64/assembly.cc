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
        // TODO: Offset starts at different amounts based on stack frame kind.
        usz offset = 16;
        for (usz index = 0; index < +std::get<MOperandLocal>(op); ++index) {
            offset += function.locals().at(index)->allocated_type()->bytes();
        }
        return fmt::format("{}(%rbp)", -isz(offset));
    } else if (std::holds_alternative<MOperandStatic>(op)) {
        return fmt::format("{}(%rip)", std::get<MOperandStatic>(op)->name());
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
