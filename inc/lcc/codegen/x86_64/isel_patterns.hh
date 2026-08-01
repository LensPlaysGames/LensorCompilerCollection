#ifndef LCC_CODEGEN_ISEL_X86_64_PATTERNS_HH
#define LCC_CODEGEN_ISEL_X86_64_PATTERNS_HH

#include <lcc/codegen/isel.hh>
#include <lcc/codegen/mir.hh>
#include <lcc/codegen/x86_64/x86_64.hh>

namespace lcc::isel::x86_64 {

namespace {

// Just a NOTE: I don't like having lcc::x86_64 /and/ lcc::isel::x86_64,
// but I don't like having isel split up across all the arch namespaces
// either.

using MKind = MInst::Kind;
using OK = OperandKind;
using Opcode = lcc::x86_64::Opcode;
using RegId = lcc::x86_64::RegisterId;

using bitcast_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Bitcast), Immediate<>>>,
    InstList<Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>>>;

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
    InstList<Inst<Clobbers<>, usz(Opcode::MoveDereferenceRHS), o<0>, o<1>>>>;

using store_reg_local = store_some_op_local<Register<>>;
using store_imm_local = store_some_op_local<Immediate<>>;

// store local 'lhs' into local 'rhs':
//   mov (%lhs), %tmp
//   mov %tmp, (%rhs)
using store_local_local = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Store), Local<>, Local<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::MoveDereferenceLHS), o<0>, v<0, 1>>,
        Inst<Clobbers<>, usz(Opcode::MoveDereferenceRHS), v<0, 1>, o<1>>>>;

// Store address of global into local
using store_global_local = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Store), Global<>, Local<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::LoadEffectiveAddress), o<0>, v<0, 1>>,
        Inst<Clobbers<>, usz(Opcode::MoveDereferenceRHS), v<0, 1>, o<1>>>>;

// Store address of global into another global
using store_global_global = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Store), Global<>, Global<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::LoadEffectiveAddress), o<0>, v<0, 1>>,
        Inst<Clobbers<>, usz(Opcode::MoveDereferenceRHS), v<0, 1>, o<1>>>>;

// store immediate 'imm' into register 'r':
//   mov $imm, %tmp
//   mov %tmp, (%r)
using store_imm_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Store), Immediate<>, Register<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, v<0, 0>>,
        Inst<Clobbers<>, usz(Opcode::MoveDereferenceRHS), v<0, 0>, o<1>>>>;

using store_imm_global = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Store), Immediate<>, Global<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, v<0, 0>>,
        Inst<Clobbers<>, usz(Opcode::MoveDereferenceRHS), v<0, 0>, o<1>>>>;

using store_reg_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Store), Register<>, Register<>>>,
    InstList<Inst<Clobbers<>, usz(Opcode::MoveDereferenceRHS), o<0>, o<1>>>>;

template <typename copy_op>
using copy_some_op = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Copy), copy_op>>,
    InstList<Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>>>;

using copy_float_to_reg = Pattern<
    InstList<InstOfCategory<
        +::lcc::Register::Category::DEFAULT,
        Clobbers<>,
        usz(MKind::Copy),
        RegisterOfCategory<+::lcc::Register::Category::FLOAT>>>,
    InstList<Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>>>;

using copy_reg_to_float = Pattern<
    InstList<InstOfCategory<
        +::lcc::Register::Category::FLOAT,
        Clobbers<>,
        usz(MKind::Copy),
        RegisterOfCategory<+::lcc::Register::Category::DEFAULT>>>,
    InstList<Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>>>;

using copy_reg = copy_some_op<Register<>>;
using copy_imm = copy_some_op<Immediate<>>;

template <typename copy_op>
using copy_mem_op = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Copy), copy_op>>,
    InstList<Inst<Clobbers<c<1>>, usz(Opcode::LoadEffectiveAddress), o<0>, i<0>>>>;

using copy_global = copy_mem_op<Global<>>;
using copy_local = copy_mem_op<Local<>>;

using simple_function_call = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Call), Function<>>>,
    InstList<Inst<Clobbers<r<usz(RegId::RETURN)>>, usz(Opcode::Call), o<0>>>>;

using float_function_call = Pattern<
    InstList<
        InstOfCategory<+::lcc::Register::Category::FLOAT, Clobbers<>, usz(MKind::Call), Function<>>>,
    InstList<Inst<Clobbers<r<usz(RegId::RETURN)>>, usz(Opcode::Call), o<0>>>>;

template <typename callee>
using simple_branch = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Branch), callee>>,
    InstList<Inst<Clobbers<>, usz(Opcode::Jump), o<0>>>>;

using simple_block_branch = simple_branch<Block<>>;

using s_ext_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::SExt), Register<>>>,
    InstList<Inst<Clobbers<c<1>>, usz(Opcode::MoveSignExtended), o<0>, i<0>>>>;

using s_ext_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::SExt), Immediate<>>>,
    InstList<Inst<Clobbers<c<1>>, usz(Opcode::MoveSignExtended), o<0>, i<0>>>>;

using not_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Compl), Register<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::Not), o<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>>>;

using sar_imm_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Sar), Immediate<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, v<0, 0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::ShiftRightArithmetic), o<1>, v<0, 0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), v<0, 0>, i<0>>>>;

using shr_imm_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Shr), Immediate<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, v<0, 0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::ShiftRightLogical), o<1>, v<0, 0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), v<0, 0>, i<0>>>>;

using shl_imm_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Shl), Immediate<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, v<0, 0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::ShiftLeft), o<1>, v<0, 0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), v<0, 0>, i<0>>>>;

using sar_imm_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Sar), Immediate<>, Register<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, v<0, 0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), ResizedRegister<1, 32>, Register<usz(RegId::RCX), Immediate<32>>>,
        Inst<Clobbers<c<1>>, usz(Opcode::ShiftRightArithmetic), Register<usz(RegId::RCX), Immediate<8>>, v<0, 0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), v<0, 0>, i<0>>>>;

using shr_imm_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Shr), Immediate<>, Register<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, v<0, 0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), ResizedRegister<1, 32>, Register<usz(RegId::RCX), Immediate<32>>>,
        Inst<Clobbers<c<1>>, usz(Opcode::ShiftRightLogical), Register<usz(RegId::RCX), Immediate<8>>, v<0, 0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), v<0, 0>, i<0>>>>;

using shl_imm_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Shl), Immediate<>, Register<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, v<0, 0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), ResizedRegister<1, 32>, Register<usz(RegId::RCX), Immediate<32>>>,
        Inst<Clobbers<c<1>>, usz(Opcode::ShiftLeft), Register<usz(RegId::RCX), Immediate<8>>, v<0, 0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), v<0, 0>, i<0>>>>;

using sar_reg_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Sar), Register<>, SizeRestrictedImmediate<8>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::ShiftRightArithmetic), o<1>, o<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>>>;

using shr_reg_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Shr), Register<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::ShiftRightLogical), o<1>, o<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>>>;

using shl_reg_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Shl), Register<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::ShiftLeft), o<1>, o<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>>>;

using sar_reg_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Sar), Register<>, Register<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<1>, Register<usz(RegId::RCX), Immediate<32>>>, // 32 bits to clear dependencies
        Inst<Clobbers<c<1>>, usz(Opcode::ShiftRightArithmetic), Register<usz(RegId::RCX), Immediate<8>>, o<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>>>;

using shr_reg_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Shr), Register<>, Register<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<1>, Register<usz(RegId::RCX), Immediate<32>>>, // 32 bits to clear dependencies
        Inst<Clobbers<c<1>>, usz(Opcode::ShiftRightLogical), Register<usz(RegId::RCX), Immediate<8>>, o<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>>>;

using shl_reg_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Shl), Register<>, Register<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<1>, Register<usz(RegId::RCX), Immediate<32>>>, // 32 bits to clear dependencies
        Inst<Clobbers<c<1>>, usz(Opcode::ShiftLeft), Register<usz(RegId::RCX), Immediate<8>>, o<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>>>;

template <usz inst_kind, usz out_opcode>
using binary_commutative_reg_reg = Pattern<
    InstList<Inst<Clobbers<>, inst_kind, Register<>, Register<>>>,
    InstList<
        Inst<Clobbers<c<1>>, out_opcode, o<0>, o<1>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<1>, i<0>>>>;

using and_reg_reg = binary_commutative_reg_reg<usz(MKind::And), usz(Opcode::And)>;
using and_reg_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::And), Register<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::And), o<1>, o<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>>>;
using and_imm_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::And), Immediate<>, Register<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::And), o<0>, o<1>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<1>, i<0>>>>;

using or_reg_reg = binary_commutative_reg_reg<usz(MKind::Or), usz(Opcode::Or)>;
using or_reg_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Or), Register<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Or), o<1>, o<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>>>;
using or_imm_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Or), Immediate<>, Register<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Or), o<0>, o<1>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<1>, i<0>>>>;

using xor_reg_reg = binary_commutative_reg_reg<usz(MKind::Xor), usz(Opcode::Xor)>;
using xor_reg_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Xor), Register<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Xor), o<1>, o<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>>>;
using xor_imm_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Xor), Immediate<>, Register<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Xor), o<0>, o<1>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<1>, i<0>>>>;

// A global is actually an lvalue (ptr to global). When adding to a
// global, we are actually trying to do ptr arithmetic; we use `lea` for
// that.
using add_global_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Add), Global<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::LoadEffectiveAddress), o<0>, i<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Add), o<1>, i<0>>>>;

// NOTE: We cannot use i<0> (output register of first instruction) to
// store the LEA intermediate result, because the input register operand
// may be the output register of the first instruction, and we need that
// unclobbered for the later add.
using add_global_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Add), Global<>, Register<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::LoadEffectiveAddress), o<0>, v<0, 0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Add), o<1>, v<0, 0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), v<0, 0>, i<0>>>>;

using add_local_imm_1 = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Add), Local<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::LoadEffectiveAddress), OffsetLocal<o<0>, o<1>>, i<0>>>>;

using add_local_imm_2 = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Add), Local<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::LoadEffectiveAddress), o<0>, i<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Add), o<1>, i<0>>>>;

using add_reg_reg = binary_commutative_reg_reg<usz(MKind::Add), usz(Opcode::Add)>;
using add_imm_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Add), Immediate<>, Register<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<1>, i<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Add), o<0>, i<0>>>>;

using add_reg_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Add), Register<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Add), o<1>, i<0>>>>;

using add_imm_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Add), Immediate<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Add), o<1>, i<0>>>>;

using mul_reg_reg = binary_commutative_reg_reg<usz(MKind::Mul), usz(Opcode::Multiply)>;

using mul_reg_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Mul), Register<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Multiply), o<1>, o<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>>>;

using mul_imm_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Mul), Immediate<>, Register<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Multiply), o<0>, o<1>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<1>, i<0>>>>;

using mul_imm_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Mul), Immediate<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Multiply), o<1>, i<0>>>>;

using sub_reg_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Sub), Register<>, Register<>>>,
    InstList<
        // NOTE: GNU ordering of operands
        Inst<Clobbers<c<1>>, usz(Opcode::Sub), o<1>, o<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>>>;

using sub_reg_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Sub), Register<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Sub), o<1>, o<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, i<0>>>>;

template <Opcode div_op, bool remainder>
using divrem = InstList<
    Inst<Clobbers<c<1>>, usz(Opcode::Move), o<1>, v<0, 1>>,
    Inst<Clobbers<c<1>>, usz(Opcode::Move), o<0>, Register<usz(RegId::RAX), Sizeof<0>>>,
    Inst<Clobbers<c<1>>, usz(Opcode::Xor), Register<usz(RegId::RDX), Immediate<32>>, Register<usz(RegId::RDX), Immediate<32>>>,
    Inst<Clobbers<r<usz(RegId::RAX)>, r<usz(RegId::RDX)>>, usz(div_op), v<0, 1>>,
    Inst<
        Clobbers<c<1>>,
        usz(Opcode::Move),
        std::conditional_t<
            remainder,
            Register<usz(RegId::RDX), Sizeof<0>>,
            Register<usz(RegId::RAX), Sizeof<0>>>,
        i<0>>>;

template <Opcode div_op>
using div = divrem<div_op, false>;

template <MKind in_op, Opcode div_op>
struct divide {
    using imm_imm = Pattern<
        InstList<
            Inst<Clobbers<>, usz(in_op), Immediate<>, Immediate<>>>,
        div<div_op>>;

    using reg_reg = Pattern<
        InstList<
            Inst<
                Clobbers<>,
                usz(in_op),
                RegisterOfCategory<+::lcc::Register::Category::DEFAULT>,
                RegisterOfCategory<+::lcc::Register::Category::DEFAULT>>>,
        div<div_op>>;

    using reg_imm = Pattern<
        InstList<
            Inst<
                Clobbers<>,
                usz(in_op),
                RegisterOfCategory<+::lcc::Register::Category::DEFAULT>,
                Immediate<>>>,
        div<div_op>>;
};

using sdiv = divide<MKind::SDiv, Opcode::SignedDivide>;
using udiv = divide<MKind::UDiv, Opcode::UnsignedDivide>;

template <usz in, usz op>
using float_reg_reg = Pattern<
    InstList<
        InstOfCategory<
            usz(+::lcc::Register::Category::FLOAT),
            Clobbers<>,
            in,
            RegisterOfCategory<+::lcc::Register::Category::FLOAT>,
            RegisterOfCategory<+::lcc::Register::Category::FLOAT>>>,
    InstList<
        Inst<Clobbers<c<1>>, op, o<0>, o<1>>,
        Inst<Clobbers<c<1>>, usz(Opcode::ScalarFloatMove), o<1>, i<0>>>>;

using float_div_reg_reg = Pattern<
    InstList<
        InstOfCategory<
            usz(+::lcc::Register::Category::FLOAT),
            Clobbers<>,
            usz(MKind::SDiv),
            RegisterOfCategory<+::lcc::Register::Category::FLOAT>,
            RegisterOfCategory<+::lcc::Register::Category::FLOAT>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::ScalarFloatDiv), o<1>, o<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::ScalarFloatMove), o<0>, i<0>>>>;

using float_add_reg_reg = float_reg_reg<usz(MKind::Add), usz(Opcode::ScalarFloatAdd)>;
using float_sub_reg_reg = float_reg_reg<usz(MKind::Sub), usz(Opcode::ScalarFloatSub)>;
using float_mul_reg_reg = float_reg_reg<usz(MKind::Mul), usz(Opcode::ScalarFloatMul)>;

using float_store_reg_reg = Pattern<
    InstList<Inst<
        Clobbers<>,
        usz(MKind::Store),
        RegisterOfCategory<+::lcc::Register::Category::FLOAT>,
        Register<>>>,
    InstList<Inst<Clobbers<>, usz(Opcode::ScalarFloatMoveDereferenceRHS), o<0>, o<1>>>>;

using float_store_reg_local = Pattern<
    InstList<Inst<
        Clobbers<>,
        usz(MKind::Store),
        RegisterOfCategory<+::lcc::Register::Category::FLOAT>,
        Local<>>>,
    InstList<Inst<Clobbers<>, usz(Opcode::ScalarFloatMoveDereferenceRHS), o<0>, o<1>>>>;

using float_copy_reg = Pattern<
    InstList<InstOfCategory<
        usz(+::lcc::Register::Category::FLOAT),
        Clobbers<>,
        usz(MKind::Copy),
        RegisterOfCategory<+::lcc::Register::Category::FLOAT>>>,
    InstList<Inst<Clobbers<c<1>>, usz(Opcode::ScalarFloatMove), o<0>, i<0>>>>;

using float_load_reg = Pattern<
    InstList<InstOfCategory<
        usz(+::lcc::Register::Category::FLOAT),
        Clobbers<>,
        usz(MKind::Load),
        Register<>>>,
    InstList<Inst<Clobbers<c<1>>, usz(Opcode::ScalarFloatMoveDereferenceLHS), o<0>, i<0>>>>;

using float_load_global = Pattern<
    InstList<InstOfCategory<
        usz(+::lcc::Register::Category::FLOAT),
        Clobbers<>,
        usz(MKind::Load),
        Global<>>>,
    InstList<Inst<Clobbers<c<1>>, usz(Opcode::ScalarFloatMoveDereferenceLHS), o<0>, i<0>>>>;

using float_load_local = Pattern<
    InstList<InstOfCategory<
        usz(+::lcc::Register::Category::FLOAT),
        Clobbers<>,
        usz(MKind::Load),
        Local<>>>,
    InstList<Inst<Clobbers<c<1>>, usz(Opcode::ScalarFloatMoveDereferenceLHS), o<0>, i<0>>>>;

using float_ret_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Return), RegisterOfCategory<+::lcc::Register::Category::FLOAT>>>,
    InstList<
        Inst<
            Clobbers<c<1>>,
            usz(Opcode::ScalarFloatMove),
            o<0>,
            RegisterOfCategory<
                +::lcc::Register::Category::FLOAT,
                usz(RegId::RETURN),
                Sizeof<0>>>,
        Inst<Clobbers<>, usz(Opcode::Return)>>>;

template <Opcode div_op>
using rem = divrem<div_op, true>;

template <MKind in, Opcode op>
struct remainder {
    using reg_reg = Pattern<
        InstList<
            Inst<
                Clobbers<>,
                usz(in),
                RegisterOfCategory<+::lcc::Register::Category::DEFAULT>,
                RegisterOfCategory<+::lcc::Register::Category::DEFAULT>>>,
        rem<op>>;

    using reg_imm = Pattern<
        InstList<
            Inst<
                Clobbers<>,
                usz(in),
                RegisterOfCategory<+::lcc::Register::Category::DEFAULT>,
                Immediate<>>>,
        rem<op>>;
};

using srem = remainder<MKind::SRem, x86_64::Opcode::SignedDivide>;
using urem = remainder<MKind::URem, x86_64::Opcode::UnsignedDivide>;

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

template <MKind kind, Opcode set_opcode>
struct cmp {
    // Optional: Inst<Clobbers<c<1>>, usz(Opcode::Xor), i<0>, i<0>> instead of Move Immediate<0> i<0>
    using reg_reg = Pattern<
        InstList<Inst<Clobbers<>, usz(kind), Register<>, Register<>>>,
        InstList<
            // NOTE: GNU ordering of operands
            Inst<Clobbers<>, usz(Opcode::Compare), o<1>, o<0>>,
            Inst<Clobbers<>, usz(Opcode::Move), Immediate<0>, i<0>>,
            Inst<Clobbers<c<0>>, usz(set_opcode), i<0>>>>;

    using reg_imm = Pattern<
        InstList<Inst<Clobbers<>, usz(kind), Register<>, Immediate<>>>,
        InstList<
            // NOTE: GNU ordering of operands
            Inst<Clobbers<>, usz(Opcode::Compare), o<1>, o<0>>,
            Inst<Clobbers<>, usz(Opcode::Move), Immediate<0>, i<0>>,
            Inst<Clobbers<c<0>>, usz(set_opcode), i<0>>>>;

    using imm_reg = Pattern<
        InstList<Inst<Clobbers<>, usz(kind), Immediate<>, Register<>>>,
        InstList<
            // NOTE: GNU ordering of operands
            Inst<Clobbers<>, usz(Opcode::Move), o<0>, v<0, 0>>,
            Inst<Clobbers<>, usz(Opcode::Compare), o<1>, v<0, 0>>,
            Inst<Clobbers<>, usz(Opcode::Move), Immediate<0>, i<0>>,
            Inst<Clobbers<c<0>>, usz(set_opcode), i<0>>>>;

    using imm_imm = Pattern<
        InstList<Inst<Clobbers<>, usz(kind), Immediate<>, Immediate<>>>,
        InstList<
            // NOTE: GNU ordering of operands
            Inst<Clobbers<>, usz(Opcode::Move), o<0>, v<0, 0>>,
            Inst<Clobbers<>, usz(Opcode::Compare), o<1>, v<0, 0>>,
            Inst<Clobbers<>, usz(Opcode::Move), Immediate<0>, i<0>>,
            Inst<Clobbers<c<0>>, usz(set_opcode), i<0>>>>;
};

using u_lt = cmp<MKind::ULt, Opcode::SetByteIfLessUnsigned>;
using s_lt = cmp<MKind::SLt, Opcode::SetByteIfLessSigned>;
using u_lt_eq = cmp<MKind::ULe, Opcode::SetByteIfEqualOrLessUnsigned>;
using s_lt_eq = cmp<MKind::SLe, Opcode::SetByteIfEqualOrLessSigned>;
using u_gt = cmp<MKind::UGt, Opcode::SetByteIfGreaterUnsigned>;
using s_gt = cmp<MKind::SGt, Opcode::SetByteIfGreaterSigned>;
using u_gt_eq = cmp<MKind::UGe, Opcode::SetByteIfEqualOrGreaterUnsigned>;
using s_gt_eq = cmp<MKind::SGe, Opcode::SetByteIfEqualOrGreaterSigned>;
using eq = cmp<MKind::Eq, Opcode::SetByteIfEqual>;
using ne = cmp<MKind::Ne, Opcode::SetByteIfNotEqual>;

using z_ext_reg = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::ZExt), Register<>>>,
    InstList<Inst<Clobbers<c<1>>, usz(Opcode::MoveZeroExtended), o<0>, i<0>>>>;

using z_ext_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::ZExt), Immediate<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::Move), o<0>, v<0, 0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::MoveZeroExtended), v<0, 0>, i<0>>>>;

using neg_reg = Pattern<
    InstList<
        Inst<Clobbers<>, usz(MKind::Neg), Register<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::Negate), o<0>>>>;

using neg_imm = Pattern<
    InstList<
        Inst<Clobbers<>, usz(MKind::Neg), Immediate<>>>,
    InstList<
        Inst<Clobbers<>, usz(Opcode::Negate), o<0>>>>;

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
        Inst<Clobbers<c<1>>, +x86_64::Opcode::MoveDereferenceLHS, o<0>, o<3>>
    >
>;

} // namespace

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
    store_global_local,
    store_global_global,
    store_imm_reg,
    store_imm_global,
    store_reg_reg,
    copy_reg,
    copy_global,
    copy_local,
    copy_imm,

    s_ext_reg,
    s_ext_imm,
    z_ext_reg,
    z_ext_imm,

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
    and_imm_reg,

    or_reg_reg,
    or_reg_imm,
    or_imm_reg,

    xor_reg_reg,
    xor_reg_imm,
    xor_imm_reg,

    add_reg_reg,
    add_imm_reg,
    add_global_reg,
    add_reg_imm,
    add_imm_imm,
    add_local_imm_1,
    // add_local_imm_2,
    add_global_imm,

    mul_reg_reg,
    mul_imm_reg,
    mul_reg_imm,
    mul_imm_imm,

    sub_reg_reg,
    sub_reg_imm,

    sdiv::imm_imm,
    sdiv::reg_imm,
    sdiv::reg_reg,

    srem::reg_reg,
    srem::reg_imm,

    udiv::imm_imm,
    udiv::reg_imm,
    udiv::reg_reg,

    urem::reg_reg,
    urem::reg_imm,

    copy_float_to_reg,
    copy_reg_to_float,

    float_add_reg_reg,
    float_copy_reg,
    float_div_reg_reg,
    float_load_global,
    float_load_local,
    float_load_reg,
    float_mul_reg_reg,
    float_ret_reg,
    float_store_reg_local,
    float_store_reg_reg,
    float_sub_reg_reg,
    float_function_call,

    bitcast_imm,

    simple_function_call,
    simple_block_branch,
    cond_branch_reg,
    cond_branch_imm,

    u_lt::reg_reg,
    s_lt::reg_reg,
    u_lt_eq::reg_reg,
    s_lt_eq::reg_reg,
    u_gt::reg_reg,
    s_gt::reg_reg,
    u_gt_eq::reg_reg,
    s_gt_eq::reg_reg,
    eq::reg_reg,
    ne::reg_reg,

    u_lt::reg_imm,
    s_lt::reg_imm,
    u_lt_eq::reg_imm,
    s_lt_eq::reg_imm,
    u_gt::reg_imm,
    s_gt::reg_imm,
    u_gt_eq::reg_imm,
    s_gt_eq::reg_imm,
    eq::reg_imm,
    ne::reg_imm,

    u_lt::imm_reg,
    s_lt::imm_reg,
    u_lt_eq::imm_reg,
    s_lt_eq::imm_reg,
    u_gt::imm_reg,
    s_gt::imm_reg,
    u_gt_eq::imm_reg,
    s_gt_eq::imm_reg,
    eq::imm_reg,
    ne::imm_reg,

    u_lt::imm_imm,
    s_lt::imm_imm,
    u_lt_eq::imm_imm,
    s_lt_eq::imm_imm,
    u_gt::imm_imm,
    s_gt::imm_imm,
    u_gt_eq::imm_imm,
    s_gt_eq::imm_imm,
    eq::imm_imm,
    ne::imm_imm,

    neg_reg,
    neg_imm>;

} // namespace lcc::isel::x86_64

#endif /* LCC_CODEGEN_ISEL_X86_64_PATTERNS_HH */
