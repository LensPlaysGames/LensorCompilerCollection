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
    Inst<usz(MInst::Kind::Return)>,
    Inst<usz(x86_64Opcode::Return)>
>;

using ret_imm = Pattern<
    Inst<usz(MInst::Kind::Return), Operand<MOperandImmediate, Immediate<>>>,
    Inst<usz(x86_64Opcode::Return), o<1>>
>;

using x86_64PatternList = PatternList<
    ret,
    ret_imm
>;

} // namespace isel
} // namespace lcc

#endif /* LCC_CODEGEN_ISEL_X86_64_PATTERNS_HH */
