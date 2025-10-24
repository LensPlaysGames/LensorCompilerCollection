#ifndef LCC_CODEGEN_ISEL_X86_64_PATTERNS_HH
#define LCC_CODEGEN_ISEL_X86_64_PATTERNS_HH

#include <lcc/codegen/isel.hh>
#include <lcc/codegen/mir.hh>
#include <lcc/codegen/x86_64/x86_64.hh>

namespace lcc::isel::x86_64 {
// Just a NOTE: I don't like having lcc::x86_64 /and/ lcc::isel::x86_64,
// but I don't like having isel split up across all the arch namespaces
// either.

using MKind = MInst::Kind;
using OK = OperandKind;
using Opcode = lcc::x86_64::Opcode;
using RegId = lcc::x86_64::RegisterId;

using bitcast_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Bitcast), Immediate<>>>,
    InstList<Inst<Clobbers<>, usz(Opcode::Move), o<0>, i<0>>>>;

using ret = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Return)>>,
    InstList<Inst<Clobbers<>, usz(Opcode::Return)>>>;

// InputOperandReference operand with a value of o<0> means replace the
// operand with whatever the zero-eth operand is in the input
// instructions; in this case, the immediate operand of the gMIR return.
template <typename ret_op, Opcode opcode>
using ret_some_op = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Return), ret_op>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(opcode), o<0>, Register<usz(RegId::RETURN), Sizeof<0>>>,
        Inst<Clobbers<>, usz(Opcode::Return)>>>;

using ret_imm = ret_some_op<Immediate<>, Opcode::Move>;
using ret_reg = ret_some_op<Register<>, Opcode::Move>;
using ret_global = ret_some_op<Global<>, Opcode::MoveDereferenceLHS>;
using ret_local = ret_some_op<Local<>, Opcode::MoveDereferenceLHS>;

// InputInstructionReference operand with a value of i<0> means replace
// the operand with a register with a value equal to the zero-eth
// instruction in the input instructions: in this case, the output
// register of the load.
template <typename load_op>
using load_some_op = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Load), load_op>>,
    InstList<Inst<Clobbers<c<1>>, usz(Opcode::MoveDereferenceLHS), o<0>, i<0>>>>;

using load_global = load_some_op<Global<>>;
using load_local = load_some_op<Local<>>;
using load_reg = load_some_op<Register<>>;

template <typename store_op>
using store_some_op_local = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Store), store_op, Local<>>>,
    InstList<Inst<Clobbers<c<1>>, usz(Opcode::MoveDereferenceRHS), o<0>, o<1>>>>;

using store_reg_local = store_some_op_local<Register<>>;
using store_imm_local = store_some_op_local<Immediate<>>;

// store local 'lhs' into local 'rhs':
//   mov (%lhs), %tmp
//   mov %tmp, (%rhs)
using store_local_local = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Store), Local<>, Local<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::MoveDereferenceLHS), o<0>, v<0, 1>>,
        Inst<Clobbers<c<1>>, usz(Opcode::MoveDereferenceRHS), v<0, 1>, o<1>>>>;

// store immediate 'imm' into register 'r':
//   mov $imm, %tmp
//   mov %tmp, (%r)
using store_imm_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Store), Immediate<>, Register<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::Move), o<0>, v<0, 0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::MoveDereferenceRHS), v<0, 0>, o<1>>>>;

using store_imm_global = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Store), Immediate<>, Global<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::Move), o<0>, v<0, 0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::MoveDereferenceRHS), v<0, 0>, o<1>>>>;

using store_reg_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Store), Register<>, Register<>>>,
    InstList<Inst<Clobbers<c<1>>, usz(Opcode::MoveDereferenceRHS), o<0>, o<1>>>>;

template <typename copy_op>
using copy_some_op = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Copy), copy_op>>,
    InstList<Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>>>;

using copy_reg = copy_some_op<Register<>>;
using copy_imm = copy_some_op<Immediate<>>;

template <typename copy_op>
using copy_mem_op = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Copy), copy_op>>,
    InstList<Inst<Clobbers<c<1>>, usz(Opcode::LoadEffectiveAddress), o<0>, i<0>>>>;

using copy_global = copy_mem_op<Global<>>;
using copy_local = copy_mem_op<Local<>>;

template <typename callee>
using simple_call = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Call), callee>>,
    InstList<Inst<Clobbers<>, usz(Opcode::Call), o<0>>>>;

using simple_function_call = simple_call<Function<>>;

template <typename callee>
using simple_branch = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Branch), callee>>,
    InstList<Inst<Clobbers<>, usz(Opcode::Jump), o<0>>>>;

using simple_block_branch = simple_branch<Block<>>;

using s_ext_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::SExt), Register<>>>,
    InstList<Inst<Clobbers<c<1>>, usz(Opcode::MoveSignExtended), o<0>, i<0>>>>;

using not_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Compl), Register<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::Not), o<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>>>;

using sar_imm_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Sar), Immediate<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::Move), o<0>, v<0, 0>>,
        Inst<Clobbers<>, usz(Opcode::ShiftRightArithmetic), o<1>, v<0, 0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), v<0, 0>, i<0>>>>;

using shr_imm_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Shr), Immediate<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::Move), o<0>, v<0, 0>>,
        Inst<Clobbers<>, usz(Opcode::ShiftRightLogical), o<1>, v<0, 0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), v<0, 0>, i<0>>>>;

using shl_imm_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Shl), Immediate<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::Move), o<0>, v<0, 0>>,
        Inst<Clobbers<>, usz(Opcode::ShiftLeft), o<1>, v<0, 0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), v<0, 0>, i<0>>>>;

using sar_imm_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Sar), Immediate<>, Register<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::Move), o<0>, v<0, 0>>,
        Inst<Clobbers<>, usz(Opcode::Move), ResizedRegister<1, 32>, Register<usz(RegId::RCX), Immediate<32>>>,
        Inst<Clobbers<>, usz(Opcode::ShiftRightArithmetic), Register<usz(RegId::RCX), Immediate<8>>, v<0, 0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), v<0, 0>, i<0>>>>;

using shr_imm_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Shr), Immediate<>, Register<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::Move), o<0>, v<0, 0>>,
        Inst<Clobbers<>, usz(Opcode::Move), ResizedRegister<1, 32>, Register<usz(RegId::RCX), Immediate<32>>>,
        Inst<Clobbers<>, usz(Opcode::ShiftRightLogical), Register<usz(RegId::RCX), Immediate<8>>, v<0, 0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), v<0, 0>, i<0>>>>;

using shl_imm_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Shl), Immediate<>, Register<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::Move), o<0>, v<0, 0>>,
        Inst<Clobbers<>, usz(Opcode::Move), ResizedRegister<1, 32>, Register<usz(RegId::RCX), Immediate<32>>>,
        Inst<Clobbers<>, usz(Opcode::ShiftLeft), Register<usz(RegId::RCX), Immediate<8>>, v<0, 0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), v<0, 0>, i<0>>>>;

// FIXME: If immediate's value doesn't fit into 8 bits, we can't encode it like this.
using sar_reg_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Sar), Register<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::ShiftRightArithmetic), o<1>, o<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<1>, i<0>>>>;

using shr_reg_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Shr), Register<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::ShiftRightLogical), o<1>, o<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<1>, i<0>>>>;

using shl_reg_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Shl), Register<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::ShiftLeft), o<1>, o<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<1>, i<0>>>>;

using sar_reg_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Sar), Register<>, Register<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::Move), o<1>, Register<usz(RegId::RCX), Immediate<32>>>, // 32 bits to clear dependencies
        Inst<Clobbers<>, usz(Opcode::ShiftRightArithmetic), Register<usz(RegId::RCX), Immediate<8>>, o<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<1>, i<0>>>>;

using shr_reg_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Shr), Register<>, Register<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::Move), o<1>, Register<usz(RegId::RCX), Immediate<32>>>, // 32 bits to clear dependencies
        Inst<Clobbers<>, usz(Opcode::ShiftRightLogical), Register<usz(RegId::RCX), Immediate<8>>, o<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<1>, i<0>>>>;

using shl_reg_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Shl), Register<>, Register<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::Move), o<1>, Register<usz(RegId::RCX), Immediate<32>>>, // 32 bits to clear dependencies
        Inst<Clobbers<>, usz(Opcode::ShiftLeft), Register<usz(RegId::RCX), Immediate<8>>, o<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<1>, i<0>>>>;

template <usz inst_kind, usz out_opcode>
using binary_commutative_reg_reg = Pattern<
    InstList<Inst<Clobbers<>, inst_kind, Register<>, Register<>>>,
    InstList<
        Inst<Clobbers<>, out_opcode, o<0>, o<1>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<1>, i<0>>>>;

using and_reg_reg = binary_commutative_reg_reg<usz(MKind::And), usz(Opcode::And)>;
using and_reg_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::And), Register<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::And), o<1>, o<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>>>;

using or_reg_reg = binary_commutative_reg_reg<usz(MKind::Or), usz(Opcode::Or)>;
using or_reg_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Or), Register<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::Or), o<1>, o<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>>>;

// A global is actually an lvalue (ptr to global). When adding to a
// global, we are actually trying to do ptr arithmetic; we use `lea` for
// that.
using add_global_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Add), Global<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::LoadEffectiveAddress), o<0>, i<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Add), o<1>, i<0>>>>;

using add_local_imm_1 = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Add), Local<>, Immediate<>>>,
    InstList<Inst<Clobbers<c<1>>, usz(Opcode::LoadEffectiveAddress), OffsetLocal<o<0>, o<1>>, i<0>>>>;

using add_local_imm_2 = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Add), Local<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::LoadEffectiveAddress), o<0>, i<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Add), o<1>, i<0>>>>;

using add_reg_reg = binary_commutative_reg_reg<usz(MKind::Add), usz(Opcode::Add)>;
using add_imm_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Add), Immediate<>, Register<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::Add), o<0>, o<1>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<1>, i<0>>>>;

using add_reg_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Add), Register<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::Add), o<1>, o<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>>>;

using add_imm_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Add), Immediate<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>,
        Inst<Clobbers<>, usz(Opcode::Add), o<1>, i<0>>>>;

using mul_reg_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Mul), Register<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::Multiply), o<1>, o<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>>>;

using mul_imm_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Mul), Immediate<>, Register<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::Multiply), o<0>, o<1>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<1>, i<0>>>>;

using mul_imm_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Mul), Immediate<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>,
        Inst<Clobbers<>, usz(Opcode::Multiply), o<1>, i<0>>>>;

using sub_reg_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Sub), Register<>, Register<>>>,
    InstList<
        // NOTE: GNU ordering of operands
        Inst<Clobbers<>, usz(Opcode::Sub), o<1>, o<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>>>;

using sub_reg_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Sub), Register<>, Immediate<>>>,
    InstList<
        // NOTE: GNU ordering of operands
        Inst<Clobbers<>, usz(Opcode::Sub), o<1>, o<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>>>;

using sdiv_reg_reg = Pattern<
    InstList<
        Inst<Clobbers<>, usz(MKind::SDiv), Register<>, Register<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<1>, v<0, 1>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, Register<usz(RegId::RAX), Sizeof<0>>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Xor), Register<usz(RegId::RDX), Immediate<32>>, Register<usz(RegId::RDX), Immediate<32>>>,
        Inst<Clobbers<r<usz(RegId::RAX)>, r<usz(RegId::RDX)>>, usz(Opcode::SignedDivide), v<0, 1>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), Register<usz(RegId::RAX), Sizeof<0>>, i<0>>>>;

using sdiv_reg_imm = Pattern<
    InstList<
        Inst<Clobbers<>, usz(MKind::SDiv), Register<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<1>, v<0, 1>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, Register<usz(RegId::RAX), Sizeof<0>>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Xor), Register<usz(RegId::RDX), Immediate<32>>, Register<usz(RegId::RDX), Immediate<32>>>,
        Inst<Clobbers<r<usz(RegId::RAX)>, r<usz(RegId::RDX)>>, usz(Opcode::SignedDivide), v<0, 1>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), Register<usz(RegId::RAX), Sizeof<0>>, i<0>>>>;

using srem_reg_reg = Pattern<
    InstList<
        Inst<Clobbers<>, usz(MKind::SRem), Register<>, Register<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<1>, v<0, 1>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, Register<usz(RegId::RAX), Sizeof<0>>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Xor), Register<usz(RegId::RDX), Immediate<32>>, Register<usz(RegId::RDX), Immediate<32>>>,
        Inst<Clobbers<r<usz(RegId::RAX)>, r<usz(RegId::RDX)>>, usz(Opcode::SignedDivide), v<0, 1>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), Register<usz(RegId::RDX), Sizeof<0>>, i<0>>>>;

using srem_reg_imm = Pattern<
    InstList<
        Inst<Clobbers<>, usz(MKind::SRem), Register<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<1>, v<0, 1>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, Register<usz(RegId::RAX), Sizeof<0>>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Xor), Register<usz(RegId::RDX), Immediate<32>>, Register<usz(RegId::RDX), Immediate<32>>>,
        Inst<Clobbers<r<usz(RegId::RAX)>, r<usz(RegId::RDX)>>, usz(Opcode::SignedDivide), v<0, 1>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), Register<usz(RegId::RDX), Sizeof<0>>, i<0>>>>;

using cond_branch_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::CondBranch), Register<>, Block<>, Block<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::Test), o<0>, o<0>>,
        Inst<Clobbers<>, usz(Opcode::JumpIfZeroFlag), o<2>>,
        Inst<Clobbers<>, usz(Opcode::Jump), o<1>>>>;

using cond_branch_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::CondBranch), Immediate<>, Block<>, Block<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::Move), o<0>, v<0, 0>>,
        Inst<Clobbers<>, usz(Opcode::Test), v<0, 0>, v<0, 0>>,
        Inst<Clobbers<>, usz(Opcode::JumpIfZeroFlag), o<2>>,
        Inst<Clobbers<>, usz(Opcode::Jump), o<1>>>>;

// Optional: Inst<Clobbers<c<1>>, usz(Opcode::Xor), i<0>, i<0>> instead of Move Immediate<0> i<0>
template <MKind kind, Opcode set_opcode>
using cmp_reg_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(kind), Register<>, Register<>>>,
    InstList<
        // NOTE: GNU ordering of operands
        Inst<Clobbers<>, usz(Opcode::Compare), o<1>, o<0>>,
        Inst<Clobbers<>, usz(Opcode::Move), Immediate<0>, i<0>>,
        Inst<Clobbers<c<0>>, usz(set_opcode), i<0>>>>;

using u_lt_reg_reg = cmp_reg_reg<MKind::ULt, Opcode::SetByteIfLessUnsigned>;
using s_lt_reg_reg = cmp_reg_reg<MKind::SLt, Opcode::SetByteIfLessSigned>;
using u_lt_eq_reg_reg = cmp_reg_reg<MKind::ULe, Opcode::SetByteIfEqualOrLessUnsigned>;
using s_lt_eq_reg_reg = cmp_reg_reg<MKind::SLe, Opcode::SetByteIfEqualOrLessSigned>;
using u_gt_reg_reg = cmp_reg_reg<MKind::UGt, Opcode::SetByteIfGreaterUnsigned>;
using s_gt_reg_reg = cmp_reg_reg<MKind::SGt, Opcode::SetByteIfGreaterSigned>;
using u_gt_eq_reg_reg = cmp_reg_reg<MKind::UGe, Opcode::SetByteIfEqualOrGreaterUnsigned>;
using s_gt_eq_reg_reg = cmp_reg_reg<MKind::SGe, Opcode::SetByteIfEqualOrGreaterSigned>;
using eq_reg_reg = cmp_reg_reg<MKind::Eq, Opcode::SetByteIfEqual>;
using ne_reg_reg = cmp_reg_reg<MKind::Ne, Opcode::SetByteIfNotEqual>;

template <MKind kind, Opcode set_opcode>
using cmp_reg_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(kind), Register<>, Immediate<>>>,
    InstList<
        // NOTE: GNU ordering of operands
        Inst<Clobbers<>, usz(Opcode::Compare), o<1>, o<0>>,
        Inst<Clobbers<>, usz(Opcode::Move), Immediate<0>, i<0>>,
        Inst<Clobbers<c<0>>, usz(set_opcode), i<0>>>>;

using u_lt_reg_imm = cmp_reg_imm<MKind::ULt, Opcode::SetByteIfLessUnsigned>;
using s_lt_reg_imm = cmp_reg_imm<MKind::SLt, Opcode::SetByteIfLessSigned>;
using u_lt_eq_reg_imm = cmp_reg_imm<MKind::ULe, Opcode::SetByteIfEqualOrLessUnsigned>;
using s_lt_eq_reg_imm = cmp_reg_imm<MKind::SLe, Opcode::SetByteIfEqualOrLessSigned>;
using u_gt_reg_imm = cmp_reg_imm<MKind::UGt, Opcode::SetByteIfGreaterUnsigned>;
using s_gt_reg_imm = cmp_reg_imm<MKind::SGt, Opcode::SetByteIfGreaterSigned>;
using u_gt_eq_reg_imm = cmp_reg_imm<MKind::UGe, Opcode::SetByteIfEqualOrGreaterUnsigned>;
using s_gt_eq_reg_imm = cmp_reg_imm<MKind::SGe, Opcode::SetByteIfEqualOrGreaterSigned>;
using eq_reg_imm = cmp_reg_imm<MKind::Eq, Opcode::SetByteIfEqual>;
using ne_reg_imm = cmp_reg_imm<MKind::Ne, Opcode::SetByteIfNotEqual>;

template <MKind kind, Opcode set_opcode>
using cmp_imm_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(kind), Immediate<>, Register<>>>,
    InstList<
        // NOTE: GNU ordering of operands
        Inst<Clobbers<>, usz(Opcode::Move), o<0>, v<0, 0>>,
        Inst<Clobbers<>, usz(Opcode::Compare), o<1>, v<0, 0>>,
        Inst<Clobbers<>, usz(Opcode::Move), Immediate<0>, i<0>>,
        Inst<Clobbers<c<0>>, usz(set_opcode), i<0>>>>;

using u_lt_imm_reg = cmp_imm_reg<MKind::ULt, Opcode::SetByteIfLessUnsigned>;
using s_lt_imm_reg = cmp_imm_reg<MKind::SLt, Opcode::SetByteIfLessSigned>;
using u_lt_eq_imm_reg = cmp_imm_reg<MKind::ULe, Opcode::SetByteIfEqualOrLessUnsigned>;
using s_lt_eq_imm_reg = cmp_imm_reg<MKind::SLe, Opcode::SetByteIfEqualOrLessSigned>;
using u_gt_imm_reg = cmp_imm_reg<MKind::UGt, Opcode::SetByteIfGreaterUnsigned>;
using s_gt_imm_reg = cmp_imm_reg<MKind::SGt, Opcode::SetByteIfGreaterSigned>;
using u_gt_eq_imm_reg = cmp_imm_reg<MKind::UGe, Opcode::SetByteIfEqualOrGreaterUnsigned>;
using s_gt_eq_imm_reg = cmp_imm_reg<MKind::SGe, Opcode::SetByteIfEqualOrGreaterSigned>;
using eq_imm_reg = cmp_imm_reg<MKind::Eq, Opcode::SetByteIfEqual>;
using ne_imm_reg = cmp_imm_reg<MKind::Ne, Opcode::SetByteIfNotEqual>;

template <MKind kind, Opcode set_opcode>
using cmp_imm_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(kind), Immediate<>, Immediate<>>>,
    InstList<
        // NOTE: GNU ordering of operands
        Inst<Clobbers<>, usz(Opcode::Move), o<0>, v<0, 0>>,
        Inst<Clobbers<>, usz(Opcode::Compare), o<1>, v<0, 0>>,
        Inst<Clobbers<>, usz(Opcode::Move), Immediate<0>, i<0>>,
        Inst<Clobbers<c<0>>, usz(set_opcode), i<0>>>>;

using u_lt_imm_imm = cmp_imm_imm<MKind::ULt, Opcode::SetByteIfLessUnsigned>;
using s_lt_imm_imm = cmp_imm_imm<MKind::SLt, Opcode::SetByteIfLessSigned>;
using u_lt_eq_imm_imm = cmp_imm_imm<MKind::ULe, Opcode::SetByteIfEqualOrLessUnsigned>;
using s_lt_eq_imm_imm = cmp_imm_imm<MKind::SLe, Opcode::SetByteIfEqualOrLessSigned>;
using u_gt_imm_imm = cmp_imm_imm<MKind::UGt, Opcode::SetByteIfGreaterUnsigned>;
using s_gt_imm_imm = cmp_imm_imm<MKind::SGt, Opcode::SetByteIfGreaterSigned>;
using u_gt_eq_imm_imm = cmp_imm_imm<MKind::UGe, Opcode::SetByteIfEqualOrGreaterUnsigned>;
using s_gt_eq_imm_imm = cmp_imm_imm<MKind::SGe, Opcode::SetByteIfEqualOrGreaterSigned>;
using eq_imm_imm = cmp_imm_imm<MKind::Eq, Opcode::SetByteIfEqual>;
using ne_imm_imm = cmp_imm_imm<MKind::Ne, Opcode::SetByteIfNotEqual>;

using z_ext_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::ZExt), Register<>>>,
    InstList<Inst<Clobbers<>, usz(Opcode::MoveZeroExtended), o<0>, i<0>>>>;

// clang-format off
// This doesn't really work, as far as I can tell. Not exactly sure yet,
// could be o<1> in input pattern not being resolved, could be order of
// patterns, could be a bug in counting the longest pattern.
using collapse_local_reg_move_reg = Pattern<
    InstList<
        Inst<Clobbers<>, +x86_64::Opcode::LoadEffectiveAddress, Local<>, Register<>>,
        Inst<Clobbers<>, +x86_64::Opcode::MoveDereferenceLHS, o<1>, Register<>>
    >,
    InstList<
        Inst<Clobbers<>, +x86_64::Opcode::MoveDereferenceLHS, o<0>, o<3>>
    >
>;

// clang-format on

using AllPatterns = PatternList<
    collapse_local_reg_move_reg,

    ret,
    ret_imm,
    ret_reg,
    ret_global,
    ret_local,
    load_global,
    load_local,
    load_reg,
    store_reg_local,
    store_imm_local,
    store_local_local,
    store_imm_reg,
    store_imm_global,
    store_reg_reg,
    copy_reg,
    copy_global,
    copy_local,
    copy_imm,

    s_ext_reg,
    z_ext_reg,

    not_reg,

    shl_imm_imm,
    shr_imm_imm,
    sar_imm_imm,

    shl_imm_reg,
    shr_imm_reg,
    sar_imm_reg,

    shl_reg_imm,
    shr_reg_imm,
    sar_reg_imm,

    shl_reg_reg,
    shr_reg_reg,
    sar_reg_reg,

    and_reg_reg,
    and_reg_imm,

    or_reg_reg,
    or_reg_imm,

    add_local_imm_1,
    add_local_imm_2,
    add_reg_reg,
    add_imm_reg,
    add_reg_imm,
    add_imm_imm,
    add_global_imm,

    mul_imm_reg,
    mul_reg_imm,
    mul_imm_imm,

    sub_reg_reg,
    sub_reg_imm,

    sdiv_reg_reg,
    sdiv_reg_imm,

    srem_reg_reg,
    srem_reg_imm,

    bitcast_imm,

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
    eq_reg_reg,
    ne_reg_reg,

    u_lt_reg_imm,
    s_lt_reg_imm,
    u_lt_eq_reg_imm,
    s_lt_eq_reg_imm,
    u_gt_reg_imm,
    s_gt_reg_imm,
    u_gt_eq_reg_imm,
    s_gt_eq_reg_imm,
    eq_reg_imm,
    ne_reg_imm,

    u_lt_imm_reg,
    s_lt_imm_reg,
    u_lt_eq_imm_reg,
    s_lt_eq_imm_reg,
    u_gt_imm_reg,
    s_gt_imm_reg,
    u_gt_eq_imm_reg,
    s_gt_eq_imm_reg,
    eq_imm_reg,
    ne_imm_reg,

    u_lt_imm_imm,
    s_lt_imm_imm,
    u_lt_eq_imm_imm,
    s_lt_eq_imm_imm,
    u_gt_imm_imm,
    s_gt_imm_imm,
    u_gt_eq_imm_imm,
    s_gt_eq_imm_imm,
    eq_imm_imm,
    ne_imm_imm>;

} // namespace lcc::isel::x86_64

#endif /* LCC_CODEGEN_ISEL_X86_64_PATTERNS_HH */
