#ifndef LCC_CODEGEN_ISEL_X86_64_PATTERNS_HH
#define LCC_CODEGEN_ISEL_X86_64_PATTERNS_HH

#include <lcc/codegen/isel.hh>

namespace lcc {
namespace isel {

enum struct x86_64Opcode : u32 {
    Poison = u32(lcc::MInst::Kind::ArchStart),
    Return,
};

using ret = Pattern<
    InstList<Inst<usz(MInst::Kind::Return)>>,
    InstList<Inst<usz(x86_64Opcode::Return)>>
>;

// InputOperandReference operand with a value of o<0> means replace the
// operand with whatever the zero-eth operand is in the input
// instructions; in this case, the immediate operand of the gMIR return.
using ret_imm = Pattern<
    InstList<Inst<usz(MInst::Kind::Return), Operand<OperandKind::Immediate, Immediate<>>>>,
    InstList<Inst<usz(x86_64Opcode::Return), Operand<OperandKind::InputOperandReference, o<0>>>>
>;

using x86_64PatternList = PatternList<
    ret,
    ret_imm
>;

} // namespace isel
} // namespace lcc

#endif /* LCC_CODEGEN_ISEL_X86_64_PATTERNS_HH */
