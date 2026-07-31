#ifndef LCC_AARCH64_ISEL_PATTERNS_HH
#define LCC_AARCH64_ISEL_PATTERNS_HH

#include <lcc/codegen/aarch64/aarch64.hh>
#include <lcc/codegen/isel.hh>
#include <lcc/codegen/mir.hh>

namespace lcc::isel::aarch64 {

namespace {

// Just a NOTE: I don't like having lcc::aarch64 /and/ lcc::isel::aarch64,
// but I don't like having isel split up across all the arch namespaces
// either.

using MKind = MInst::Kind;
using OK = OperandKind;
using Opcode = lcc::aarch64::Opcode;
using RegId = lcc::aarch64::RegisterId;

using ret = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Return)>>,
    InstList<Inst<Clobbers<>, usz(Opcode::Return)>>>;

template <typename ret_op, Opcode opcode>
using ret_some_op = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Return), ret_op>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(opcode), o<0>, Register<usz(RegId::RETURN), Sizeof<0>>>,
        Inst<Clobbers<>, usz(Opcode::Return)>>>;

using ret_imm = ret_some_op<Immediate<>, Opcode::Move>;
using ret_reg = ret_some_op<Register<>, Opcode::Move>;

template <typename load_op>
using load_some_op = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Load), load_op>>,
    InstList<Inst<Clobbers<c<1>>, usz(Opcode::Load), i<0>, o<0>>>>;

using load_global = load_some_op<Global<>>;
using load_local = load_some_op<Local<>>;
using load_reg = load_some_op<Register<>>;

template <typename store_op>
using store_some_op_local = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Store), store_op, Local<>>>,
    InstList<Inst<Clobbers<>, usz(Opcode::Store), o<0>, o<1>>>>;

using store_reg_local = store_some_op_local<Register<>>;
using store_imm_local = store_some_op_local<Immediate<>>;

template <typename copy_op>
using copy_some_op = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Copy), copy_op>>,
    InstList<Inst<Clobbers<c<1>>, usz(Opcode::Move), i<0>, o<0>>>>;

using copy_reg = copy_some_op<Register<>>;
using copy_imm = copy_some_op<Immediate<>>;

template <typename copy_op>
using copy_mem_op = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Copy), copy_op>>,
    InstList<Inst<Clobbers<c<1>>, usz(Opcode::Address), i<0>, o<0>>>>;

using copy_global = copy_mem_op<Global<>>;
using copy_local = copy_mem_op<Local<>>;

using add_imm_imm = Pattern<
    InstList<Inst<Clobbers<>, usz(MKind::Add), Immediate<>, Immediate<>>>,
    InstList<
        Inst<Clobbers<c<1>>, usz(Opcode::Move), i<0>, o<0>>,
        Inst<Clobbers<c<1>>, usz(Opcode::Add), i<0>, i<0>, o<1>>>>;

} // namespace

using AllPatterns = PatternList<
    load_global,
    load_local,
    load_reg,

    store_imm_local,
    store_reg_local,

    copy_global,
    copy_imm,
    copy_local,
    copy_reg,

    add_imm_imm,

    ret_imm,
    ret_reg,

    ret>;

} // namespace lcc::isel::aarch64

#endif /* LCC_AARCH64_ISEL_PATTERNS_HH */
