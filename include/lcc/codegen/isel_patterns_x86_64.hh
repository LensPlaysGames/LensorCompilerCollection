#ifndef LCC_CODEGEN_ISEL_X86_64_PATTERNS_HH
#define LCC_CODEGEN_ISEL_X86_64_PATTERNS_HH

#include <lcc/codegen/isel.hh>
#include <lcc/codegen/x86_64.hh>

namespace lcc {
namespace isel {

using OK = OperandKind;

using store_reg_local = Pattern<
    InstList<Inst<usz(MInst::Kind::Store), Operand<OK::Register, Register<0, 0>>, Operand<OK::Local, Local<>>>>,
    InstList<Inst<usz(x86_64::Opcode::Move), Operand<OK::InputOperandReference, o<0>>, Operand<OK::InputOperandReference, o<1>>>>>;

using ret = Pattern<
    InstList<Inst<usz(MInst::Kind::Return)>>,
    InstList<Inst<usz(x86_64::Opcode::Return)>>>;

// InputOperandReference operand with a value of o<0> means replace the
// operand with whatever the zero-eth operand is in the input
// instructions; in this case, the immediate operand of the gMIR return.
using ret_imm = Pattern<
    InstList<Inst<usz(MInst::Kind::Return), Operand<OK::Immediate, Immediate<>>>>,
    InstList<
        Inst<usz(x86_64::Opcode::Move), Operand<OK::InputOperandReference, o<0>>, Operand<OK::Register, Register<usz(lcc::x86_64::RegisterId::RETURN), 0>>>,
        Inst<usz(x86_64::Opcode::Return)>>>;

using x86_64PatternList = PatternList<
    ret,
    ret_imm,
    store_reg_local>;

} // namespace isel
} // namespace lcc

#endif /* LCC_CODEGEN_ISEL_X86_64_PATTERNS_HH */
