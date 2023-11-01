#ifndef LCC_CODEGEN_INSTRUCTION_SELECTION_HH
#define LCC_CODEGEN_INSTRUCTION_SELECTION_HH

#include <lcc/utils.hh>
#include <lcc/codegen/mir.hh>

namespace lcc {

namespace isel {

template<i64 imm = 0>
struct Immediate {
    static constexpr i64 immediate = imm;
};

template<typename kind, typename value>
struct Operand{};

// Operand reference, by index.
template <usz idx>
struct o {
    static constexpr usz index = idx;
};

template<usz opcode, typename... operands>
struct Inst {};

template <typename i, typename o>
struct Pattern {
    void rewrite(MFunction function);
};

using ret = Pattern<
    Inst<usz(Value::Kind::Return)>,
    Inst<0x420>
    >;

using ret_imm = Pattern<
    Inst<usz(Value::Kind::Return), Operand<MOperandImmediate, Immediate<>>>,
    Inst<0x420, o<1>>
    >;

template<typename in, typename out>
void Pattern<in, out>::rewrite(MFunction function) {
    // I don't even know how this would work. Switching between types and
    // values is frustrating and confusing.
}

}

} // namespace lcc

#endif /* LCC_CODEGEN_INSTRUCTION_SELECTION_HH */
