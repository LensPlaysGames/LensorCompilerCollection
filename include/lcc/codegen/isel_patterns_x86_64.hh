#ifndef LCC_CODEGEN_ISEL_X86_64_PATTERNS_HH
#define LCC_CODEGEN_ISEL_X86_64_PATTERNS_HH

#include <lcc/codegen/isel.hh>
#include <lcc/codegen/x86_64.hh>

namespace lcc {
namespace isel {

using OK = OperandKind;

using ret = Pattern<
    InstList<Inst<Clobbers<>, usz(MInst::Kind::Return)>>,
    InstList<Inst<Clobbers<>, usz(x86_64::Opcode::Return)>>>;

// InputOperandReference operand with a value of o<0> means replace the
// operand with whatever the zero-eth operand is in the input
// instructions; in this case, the immediate operand of the gMIR return.
// FIXME: Return register size is hardcoded at 64. We need some way to get
// size from another operand, or something?
template <typename ret_op>
using ret_some_op = Pattern<
    InstList<Inst<Clobbers<>, usz(MInst::Kind::Return), ret_op>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(x86_64::Opcode::Move), o<0>, Register<usz(lcc::x86_64::RegisterId::RETURN), 64>>,
        Inst<Clobbers<>, usz(x86_64::Opcode::Return)>>>;

using ret_imm = ret_some_op<Immediate<>>;
using ret_reg = ret_some_op<Register<0, 0>>;

// InputInstructionReference operand with a value of i<0> means replace
// the operand with a register with a value equal to the zero-eth
// instruction in the input instructions: in this case, the output
// register of the load.
template <typename load_op>
using load_some_op = Pattern<
    InstList<Inst<Clobbers<>, usz(MInst::Kind::Load), load_op>>,
    InstList<Inst<Clobbers<c<1>>, usz(x86_64::Opcode::MoveDereferenceLHS), o<0>, i<0>>>>;

using load_local = load_some_op<Local<>>;
using load_reg = load_some_op<Register<0, 0>>;

template <typename store_op>
using store_some_op_local = Pattern<
    InstList<Inst<Clobbers<>, usz(MInst::Kind::Store), store_op, Local<>>>,
    InstList<Inst<Clobbers<c<1>>, usz(x86_64::Opcode::Move), o<0>, o<1>>>>;

using store_reg_local = store_some_op_local<Register<0, 0>>;
using store_imm_local = store_some_op_local<Immediate<0>>;

// store immediate 'imm' into register 'r':
//   mov $imm, %tmp
//   mov %tmp, (%r)
using store_imm_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MInst::Kind::Store), Immediate<0>, Register<0, 0>>>,
    InstList<
        Inst<Clobbers<>, usz(x86_64::Opcode::Move), o<0>, v<0>>,
        Inst<Clobbers<c<1>>, usz(x86_64::Opcode::MoveDereferenceRHS), v<0>, o<1>>>>;

template <typename copy_op>
using copy_some_op = Pattern<
    InstList<Inst<Clobbers<>, usz(MInst::Kind::Copy), copy_op>>,
    InstList<Inst<Clobbers<c<1>>, usz(x86_64::Opcode::Move), o<0>, i<0>>>>;

using copy_reg = copy_some_op<Register<0, 0>>;
using copy_imm = copy_some_op<Immediate<0>>;

template <typename copy_op>
using copy_mem_op = Pattern<
    InstList<Inst<Clobbers<>, usz(MInst::Kind::Copy), copy_op>>,
    InstList<Inst<Clobbers<c<1>>, usz(x86_64::Opcode::LoadEffectiveAddress), o<0>, i<0>>>>;

using copy_global = copy_mem_op<Global<>>;
using copy_local = copy_mem_op<Local<>>;

template <typename callee>
using simple_call = Pattern<
    InstList<Inst<Clobbers<>, usz(MInst::Kind::Call), callee>>,
    InstList<Inst<Clobbers<>, usz(x86_64::Opcode::Call), o<0>>>>;

using simple_function_call = simple_call<Function<>>;

template <typename callee>
using simple_branch = Pattern<
    InstList<Inst<Clobbers<>, usz(MInst::Kind::Branch), callee>>,
    InstList<Inst<Clobbers<>, usz(x86_64::Opcode::Jump), o<0>>>>;

using simple_block_branch = simple_branch<Block<>>;

using s_ext_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MInst::Kind::SExt), Register<0, 0>>>,
    InstList<Inst<Clobbers<c<1>>, usz(x86_64::Opcode::MoveSignExtended), o<0>, i<0>>>>;

using add_local_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MInst::Kind::Add), Local<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<>, usz(x86_64::Opcode::LoadEffectiveAddress), o<0>, i<0>>,
        Inst<Clobbers<c<1>>, usz(x86_64::Opcode::Add), o<1>, i<0>>>>;

using add_reg_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MInst::Kind::Add), Register<0, 0>, Register<0, 0>>>,
    InstList<
        Inst<Clobbers<>, usz(x86_64::Opcode::Add), o<0>, o<1>>,
        Inst<Clobbers<c<1>>, usz(x86_64::Opcode::Move), o<1>, i<0>>>>;

using x86_64PatternList = PatternList<
    ret,
    ret_imm,
    ret_reg,
    load_local,
    load_reg,
    store_reg_local,
    store_imm_local,
    store_imm_reg,
    copy_reg,
    copy_global,
    copy_local,
    copy_imm,
    s_ext_reg,
    add_local_imm,
    add_reg_reg,
    simple_function_call,
    simple_block_branch>;

} // namespace isel
} // namespace lcc

#endif /* LCC_CODEGEN_ISEL_X86_64_PATTERNS_HH */
