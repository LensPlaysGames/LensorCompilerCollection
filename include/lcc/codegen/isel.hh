#ifndef LCC_CODEGEN_INSTRUCTION_SELECTION_HH
#define LCC_CODEGEN_INSTRUCTION_SELECTION_HH

#include <lcc/utils.hh>
#include <lcc/context.hh>
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

template<usz opcode_, typename... operands>
struct Inst {
    static constexpr usz opcode = opcode_;
};

template <typename in, typename out>
struct Pattern {
    static void rewrite(MFunction& function) {
        for (auto& block : function.blocks()) {
            for (auto& instruction : block.instructions()) {
                if (instruction.opcode() == in::opcode) {
                    fmt::print("Matching {}!\n", ToString(instruction.kind()));
                }
            }
        }
    }
};

template<typename... Patterns>
struct PatternList {
    static void rewrite(MFunction& function) {
        (Patterns::rewrite(function), ...);
    }
};


} // namespace isel

void select_instructions(const Context& ctx, MFunction& function);

} // namespace lcc

#endif /* LCC_CODEGEN_INSTRUCTION_SELECTION_HH */
