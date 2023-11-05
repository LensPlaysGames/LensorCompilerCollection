#ifndef LCC_CODEGEN_ISEL_X86_64_PATTERNS_HH
#define LCC_CODEGEN_ISEL_X86_64_PATTERNS_HH

#include <lcc/codegen/isel.hh>
#include <lcc/codegen/x86_64.hh>

namespace lcc {
namespace isel {

enum struct x86_64Opcode : u32 {
    Poison = u32(lcc::MInst::Kind::ArchStart),
    Return, // ret
    Move,   // mov
};

using ret = Pattern<
    InstList<Inst<usz(MInst::Kind::Return)>>,
    InstList<Inst<usz(x86_64Opcode::Return)>>>;

// InputOperandReference operand with a value of o<0> means replace the
// operand with whatever the zero-eth operand is in the input
// instructions; in this case, the immediate operand of the gMIR return.
using ret_imm = Pattern<
    InstList<Inst<usz(MInst::Kind::Return), Operand<OperandKind::Immediate, Immediate<>>>>,
    InstList<
        Inst<usz(x86_64Opcode::Move), Operand<OperandKind::InputOperandReference, o<0>>, Operand<OperandKind::Register, Register<usz(lcc::x86_64::RegisterId::RETURN), 0>>>,
        Inst<usz(x86_64Opcode::Return)>>>;

using x86_64PatternList = PatternList<
    ret,
    ret_imm>;

} // namespace isel
} // namespace lcc

#endif /* LCC_CODEGEN_ISEL_X86_64_PATTERNS_HH */
