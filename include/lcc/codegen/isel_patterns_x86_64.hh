#ifndef LCC_CODEGEN_ISEL_X86_64_PATTERNS_HH
#define LCC_CODEGEN_ISEL_X86_64_PATTERNS_HH

#include <lcc/codegen/isel.hh>
#include <lcc/codegen/mir.hh>
#include <lcc/codegen/x86_64.hh>

namespace lcc {
namespace isel {

using OK = OperandKind;
using Opcode = x86_64::Opcode;

using ret = Pattern<
    InstList<Inst<Clobbers<>, usz(MInst::Kind::Return)>>,
    InstList<Inst<Clobbers<>, usz(Opcode::Return)>>>;

// InputOperandReference operand with a value of o<0> means replace the
// operand with whatever the zero-eth operand is in the input
// instructions; in this case, the immediate operand of the gMIR return.
// FIXME: Return register size is hardcoded at 64. We need some way to get
// size from another operand, or something?
template <typename ret_op>
using ret_some_op = Pattern<
    InstList<Inst<Clobbers<>, usz(MInst::Kind::Return), ret_op>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, Register<usz(lcc::x86_64::RegisterId::RETURN), 64>>,
        Inst<Clobbers<>, usz(Opcode::Return)>>>;

using ret_imm = ret_some_op<Immediate<>>;
using ret_reg = ret_some_op<Register<0, 0>>;

// InputInstructionReference operand with a value of i<0> means replace
// the operand with a register with a value equal to the zero-eth
// instruction in the input instructions: in this case, the output
// register of the load.
template <typename load_op>
using load_some_op = Pattern<
    InstList<Inst<Clobbers<>, usz(MInst::Kind::Load), load_op>>,
    InstList<Inst<Clobbers<c<1>>, usz(Opcode::MoveDereferenceLHS), o<0>, i<0>>>>;

using load_local = load_some_op<Local<>>;
using load_reg = load_some_op<Register<0, 0>>;

template <typename store_op>
using store_some_op_local = Pattern<
    InstList<Inst<Clobbers<>, usz(MInst::Kind::Store), store_op, Local<>>>,
    InstList<Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, o<1>>>>;

using store_reg_local = store_some_op_local<Register<0, 0>>;
using store_imm_local = store_some_op_local<Immediate<0>>;

// store immediate 'imm' into register 'r':
//   mov $imm, %tmp
//   mov %tmp, (%r)
using store_imm_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MInst::Kind::Store), Immediate<0>, Register<0, 0>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::Move), o<0>, v<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::MoveDereferenceRHS), v<0>, o<1>>>>;

using store_reg_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MInst::Kind::Store), Register<0, 0>, Register<0, 0>>>,
    InstList<Inst<Clobbers<c<1>>, usz(Opcode::MoveDereferenceRHS), o<0>, o<1>>>>;

template <typename copy_op>
using copy_some_op = Pattern<
    InstList<Inst<Clobbers<>, usz(MInst::Kind::Copy), copy_op>>,
    InstList<Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>>>;

using copy_reg = copy_some_op<Register<0, 0>>;
using copy_imm = copy_some_op<Immediate<0>>;

template <typename copy_op>
using copy_mem_op = Pattern<
    InstList<Inst<Clobbers<>, usz(MInst::Kind::Copy), copy_op>>,
    InstList<Inst<Clobbers<c<1>>, usz(Opcode::LoadEffectiveAddress), o<0>, i<0>>>>;

using copy_global = copy_mem_op<Global<>>;
using copy_local = copy_mem_op<Local<>>;

template <typename callee>
using simple_call = Pattern<
    InstList<Inst<Clobbers<>, usz(MInst::Kind::Call), callee>>,
    InstList<Inst<Clobbers<>, usz(Opcode::Call), o<0>>>>;

using simple_function_call = simple_call<Function<>>;

template <typename callee>
using simple_branch = Pattern<
    InstList<Inst<Clobbers<>, usz(MInst::Kind::Branch), callee>>,
    InstList<Inst<Clobbers<>, usz(Opcode::Jump), o<0>>>>;

using simple_block_branch = simple_branch<Block<>>;

using s_ext_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MInst::Kind::SExt), Register<0, 0>>>,
    InstList<Inst<Clobbers<c<1>>, usz(Opcode::MoveSignExtended), o<0>, i<0>>>>;

using add_local_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MInst::Kind::Add), Local<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::LoadEffectiveAddress), o<0>, i<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Add), o<1>, i<0>>>>;

using add_reg_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MInst::Kind::Add), Register<0, 0>, Register<0, 0>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::Add), o<0>, o<1>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<1>, i<0>>>>;

using sub_reg_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MInst::Kind::Sub), Register<0, 0>, Register<0, 0>>>,
    InstList<
        // NOTE: GNU ordering of operands
        Inst<Clobbers<>, usz(Opcode::Sub), o<1>, o<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>>>;

using cond_branch_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MInst::Kind::CondBranch), Register<0, 0>, Block<>, Block<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::Test), o<0>, o<0>>,
        Inst<Clobbers<>, usz(Opcode::JumpIfZeroFlag), o<2>>,
        Inst<Clobbers<>, usz(Opcode::Jump), o<1>>>>;

using cond_branch_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MInst::Kind::CondBranch), Immediate<0>, Block<>, Block<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::Move), o<0>, v<0>>,
        Inst<Clobbers<>, usz(Opcode::Test), v<0>, v<0>>,
        Inst<Clobbers<>, usz(Opcode::JumpIfZeroFlag), o<2>>,
        Inst<Clobbers<>, usz(Opcode::Jump), o<1>>>>;

template <MInst::Kind kind, Opcode set_opcode>
using cmp_reg_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(kind), Register<0, 0>, Register<0, 0>>>,
    InstList<
        // NOTE: GNU ordering of operands
        Inst<Clobbers<>, usz(Opcode::Compare), o<1>, o<0>>,
        Inst<Clobbers<>, usz(Opcode::Move), Immediate<0>, i<0>>,
        Inst<Clobbers<c<0>>, usz(set_opcode), i<0>>>>;

using u_lt_reg_reg = cmp_reg_reg<MInst::Kind::ULt, Opcode::SetByteIfLessUnsigned>;
using s_lt_reg_reg = cmp_reg_reg<MInst::Kind::SLt, Opcode::SetByteIfLessSigned>;
using u_lt_eq_reg_reg = cmp_reg_reg<MInst::Kind::ULe, Opcode::SetByteIfEqualOrLessUnsigned>;
using s_lt_eq_reg_reg = cmp_reg_reg<MInst::Kind::SLe, Opcode::SetByteIfEqualOrLessSigned>;
using u_gt_reg_reg = cmp_reg_reg<MInst::Kind::UGt, Opcode::SetByteIfGreaterUnsigned>;
using s_gt_reg_reg = cmp_reg_reg<MInst::Kind::SGt, Opcode::SetByteIfGreaterSigned>;
using u_gt_eq_reg_reg = cmp_reg_reg<MInst::Kind::UGe, Opcode::SetByteIfEqualOrGreaterUnsigned>;
using s_gt_eq_reg_reg = cmp_reg_reg<MInst::Kind::SGe, Opcode::SetByteIfEqualOrGreaterSigned>;
using eq_reg_reg = cmp_reg_reg<MInst::Kind::Eq, Opcode::SetByteIfEqual>;

using x86_64PatternList = PatternList<
    ret,
    ret_imm,
    ret_reg,
    load_local,
    load_reg,
    store_reg_local,
    store_imm_local,
    store_imm_reg,
    store_reg_reg,
    copy_reg,
    copy_global,
    copy_local,
    copy_imm,
    s_ext_reg,
    add_local_imm,
    add_reg_reg,
    sub_reg_reg,
    simple_function_call,
    simple_block_branch,
    cond_branch_reg,
    cond_branch_imm,
    u_lt_reg_reg,
    s_lt_reg_reg,
    u_lt_eq_reg_reg,
    s_lt_eq_reg_reg,
    u_gt_reg_reg,
    s_gt_reg_reg,
    u_gt_eq_reg_reg,
    s_gt_eq_reg_reg,
    eq_reg_reg>;

} // namespace isel
} // namespace lcc

#endif /* LCC_CODEGEN_ISEL_X86_64_PATTERNS_HH */
