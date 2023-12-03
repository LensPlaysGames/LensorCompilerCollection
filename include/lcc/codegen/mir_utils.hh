#ifndef LCC_CODEGEN_MIR_UTILS_HH
#define LCC_CODEGEN_MIR_UTILS_HH

#include <lcc/codegen/mir.hh>

namespace lcc {

// ================================
// = ONE OPERAND                  =
// ================================
template <typename Op>
requires std::is_convertible_v<Op, MOperand>
bool is_one_operand(MInst& inst) {
    // clang-format off
    return inst.all_operands().size() == 1
           and std::holds_alternative<Op>(inst.get_operand(0));
    // clang-format on
}

bool is_reg(MInst& inst) {
    return is_one_operand<MOperandRegister>(inst);
}
auto extract_reg(MInst& inst) {
    return std::get<MOperandRegister>(inst.get_operand(0));
}
bool is_imm(MInst& inst) {
    return is_one_operand<MOperandImmediate>(inst);
}
auto extract_imm(MInst& inst) {
    return std::get<MOperandImmediate>(inst.get_operand(0));
}
bool is_local(MInst& inst) {
    return is_one_operand<MOperandLocal>(inst);
}
auto extract_local(MInst& inst) {
    return std::get<MOperandLocal>(inst.get_operand(0));
}
bool is_global(MInst& inst) {
    return is_one_operand<MOperandGlobal>(inst);
}
auto extract_global(MInst& inst) {
    return std::get<MOperandGlobal>(inst.get_operand(0));
}
bool is_function(MInst& inst) {
    return is_one_operand<MOperandFunction>(inst);
}
auto extract_function(MInst& inst) {
    return std::get<MOperandFunction>(inst.get_operand(0));
}
bool is_block(MInst& inst) {
    return is_one_operand<MOperandBlock>(inst);
}
auto extract_block(MInst& inst) {
    return std::get<MOperandBlock>(inst.get_operand(0));
}

// ================================
// = TWO OPERAND                  =
// ================================
template <typename Op1, typename Op2>
requires std::is_convertible_v<Op1, MOperand> and std::is_convertible_v<Op2, MOperand>
bool is_two_operand(MInst& inst) {
    // clang-format off
    return inst.all_operands().size() == 2
           and std::holds_alternative<Op1>(inst.get_operand(0))
           and std::holds_alternative<Op2>(inst.get_operand(1));
    // clang-format on
}
template <typename Op1, typename Op2>
std::tuple<Op1, Op2> extract_two_operand(MInst& inst) {
    return {
        std::get<Op1>(inst.get_operand(0)),
        std::get<Op2>(inst.get_operand(1))};
}

bool is_reg_imm(MInst& inst) {
    return is_two_operand<MOperandRegister, MOperandImmediate>(inst);
}
auto extract_reg_imm(MInst& inst) {
    return extract_two_operand<MOperandRegister, MOperandImmediate>(inst);
}
bool is_reg_reg(MInst& inst) {
    return is_two_operand<MOperandRegister, MOperandRegister>(inst);
}
auto extract_reg_reg(MInst& inst) {
    return extract_two_operand<MOperandRegister, MOperandRegister>(inst);
}
bool is_reg_local(MInst& inst) {
    return is_two_operand<MOperandRegister, MOperandLocal>(inst);
}
auto extract_reg_local(MInst& inst) {
    return extract_two_operand<MOperandRegister, MOperandLocal>(inst);
}
bool is_reg_global(MInst& inst) {
    return is_two_operand<MOperandRegister, MOperandGlobal>(inst);
}
auto extract_reg_global(MInst& inst) {
    return extract_two_operand<MOperandRegister, MOperandGlobal>(inst);
}

bool is_imm_reg(MInst& inst) {
    return is_two_operand<MOperandImmediate, MOperandRegister>(inst);
}
auto extract_imm_reg(MInst& inst) {
    return extract_two_operand<MOperandImmediate, MOperandRegister>(inst);
}
bool is_imm_local(MInst& inst) {
    return is_two_operand<MOperandImmediate, MOperandLocal>(inst);
}
auto extract_imm_local(MInst& inst) {
    return extract_two_operand<MOperandImmediate, MOperandLocal>(inst);
}
bool is_imm_global(MInst& inst) {
    return is_two_operand<MOperandImmediate, MOperandGlobal>(inst);
}
auto extract_imm_global(MInst& inst) {
    return extract_two_operand<MOperandImmediate, MOperandGlobal>(inst);
}

bool is_local_imm(MInst& inst) {
    return is_two_operand<MOperandLocal, MOperandImmediate>(inst);
}
auto extract_local_imm(MInst& inst) {
    return extract_two_operand<MOperandLocal, MOperandImmediate>(inst);
}
bool is_local_reg(MInst& inst) {
    return is_two_operand<MOperandLocal, MOperandRegister>(inst);
}
auto extract_local_reg(MInst& inst) {
    return extract_two_operand<MOperandLocal, MOperandRegister>(inst);
}

bool is_global_reg(MInst& inst) {
    return is_two_operand<MOperandGlobal, MOperandRegister>(inst);
}
auto extract_global_reg(MInst& inst) {
    return extract_two_operand<MOperandGlobal, MOperandRegister>(inst);
}

} // namespace lcc

#endif /* LCC_CODEGEN_MIR_UTILS_HH */
