#include <lcc/codegen/generic_object.hh>
#include <lcc/codegen/mir.hh>
#include <lcc/codegen/x86_64/object.hh>
#include <lcc/context.hh>
#include <variant>

namespace lcc {
namespace x86_64 {

// Must be MOperand* types
template <typename Op1, typename Op2>
bool is_two_operand(MInst& inst) {
    // clang-format off
    return inst.all_operands().size() == 2
           and std::holds_alternative<Op1>(inst.get_operand(0))
           and std::holds_alternative<Op2>(inst.get_operand(1));
    // clang-format on
}

bool is_reg_imm(MInst& inst) {
    return is_two_operand<MOperandRegister, MOperandImmediate>(inst);
}
bool is_reg_reg(MInst& inst) {
    return is_two_operand<MOperandRegister, MOperandRegister>(inst);
}
bool is_reg_local(MInst& inst) {
    return is_two_operand<MOperandRegister, MOperandLocal>(inst);
}
bool is_imm_imm(MInst& inst) {
    return is_two_operand<MOperandImmediate, MOperandImmediate>(inst);
}
bool is_imm_reg(MInst& inst) {
    return is_two_operand<MOperandImmediate, MOperandRegister>(inst);
}
bool is_imm_local(MInst& inst) {
    return is_two_operand<MOperandImmediate, MOperandLocal>(inst);
}
bool is_local_imm(MInst& inst) {
    return is_two_operand<MOperandLocal, MOperandImmediate>(inst);
}
bool is_local_reg(MInst& inst) {
    return is_two_operand<MOperandLocal, MOperandRegister>(inst);
}

static void assemble(MFunction& func, Section& text) {
    for (auto& block : func.blocks()) {
        for (auto& inst : block.instructions()) {
            // TODO: Assemble instruction
        }
    }
}

GenericObject emit_mcode_gobj(Module* module, const MachineDescription& desc, std::vector<MFunction>& mir) {
    GenericObject out{};

    Section text_{".text", {}, 0, 0, false};
    Section data_{".data", {}, 0, 0, false};
    Section bss_{".bss", {}, 0, 0, true};
    out.sections.push_back(text_);
    out.sections.push_back(data_);
    out.sections.push_back(bss_);

    Section& text = out.section(".text");
    Section& data = out.section(".data");
    Section& bss = out.section(".bss");

    for (auto* var : module->vars())
        out.symbol_from_global(var);

    for (auto& func : mir) {
        const bool exported = func.linkage() == Linkage::Exported || func.linkage() == Linkage::Reexported;
        const bool imported = func.linkage() == Linkage::Imported || func.linkage() == Linkage::Reexported;

        if (imported) {
            Symbol sym{};
            sym.kind = Symbol::Kind::EXTERNAL;
            sym.name = func.name();
            // FIXME: Is section name or byte offset needed?
            out.symbols.push_back(sym);
        } else {
            Symbol sym{};
            sym.kind = Symbol::Kind::FUNCTION;
            sym.name = func.name();
            sym.section_name = text.name;
            sym.byte_offset = text.contents.size();
            out.symbols.push_back(sym);
        }

        // Assemble function into machine code.
        assemble(func, text);
    }

    // LCC_TODO("Actually assemble into machine code, populate symbols, etc");

    return out;
}

} // namespace x86_64
} // namespace lcc
