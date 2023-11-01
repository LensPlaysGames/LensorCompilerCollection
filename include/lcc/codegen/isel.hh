#ifndef LCC_CODEGEN_INSTRUCTION_SELECTION_HH
#define LCC_CODEGEN_INSTRUCTION_SELECTION_HH

#include <lcc/utils.hh>
#include <lcc/ir/module.hh>
#include <lcc/codegen/mir.hh>

namespace lcc {
namespace isel {

/// `while`-like iteration over a template parameter pack.
template<typename... pack>
constexpr void While(bool& cond, auto&& lambda) {
    auto impl = [&]<typename t>() {
        if (not cond) return false;
        lambda.template operator()<t>();
        return true;
    };

    (impl.template operator()<pack>() and ...);
}

template<typename... pack>
constexpr void Foreach(auto&& lambda) {
    (lambda.template operator()<pack>(), ...);
}

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
    using input = in;
    using output = out;
};

template<typename... Patterns>
struct PatternList {
    static MFunction rewrite(lcc::Module* mod, MFunction& function) {
        MFunction out{};
        out.name() = function.name();
        out.locals() = function.locals();

        // TODO: Get the longest pattern length
        usz longest_pattern_length = 1;

        // NOTE: If you modify block.instructions() in any way, you are going to
        // have a bad time.
        std::vector<MInst*> instructions{};

        for (auto& old_block : function.blocks()) {
            out.add_block(MBlock(old_block.name()));
            auto& new_block = out.blocks().back();
            for (auto& instruction : old_block.instructions()) {
                usz instructions_handled = 0;

                do {
                    for (; instructions_handled < old_block.instructions().size(); ++instructions_handled) {
                        // Add (up to) `longest_pattern_length` instructions to the instructions vector.
                        if (instructions.size() >= longest_pattern_length) break;
                        instructions.push_back(old_block.instructions().data() + instructions_handled);
                    }

                    bool to_be_handled = true;
                    While<Patterns...>(to_be_handled, [&]<typename pattern>() {
                            if (instructions.front()->opcode() == pattern::input::opcode) {
                                fmt::print("Pattern matched!\n");
                                LCC_TODO();

                                to_be_handled = false; // break
                            }
                        });

                    // If we get through *all* of the patterns, and none of them matched, we
                    // can pop an instruction off the front and emit it into the output,
                    // before going back to the "add instructions" bit.
                    if (to_be_handled) {
                        if (instructions.size()) {
                            // Add front of `instructions` to emission output.
                            new_block.insert(*instructions.front());
                            // Pop instruction from front of vector.
                            instructions.erase(instructions.begin());
                        }
                    }

                    // If there are still instructions in the window to be handled, go back
                    // and handle them (after topping off the window with any more
                    // instructions that there may be). If the window is empty, but there are
                    // still more instructions to handle in this block, also go back and
                    // handle them.
                } while (instructions.size() != 0 || instructions_handled < old_block.instructions().size());
            }
        }

        return out;
    }
};


} // namespace isel

void select_instructions(Module* ctx, MFunction& function);

} // namespace lcc

#endif /* LCC_CODEGEN_INSTRUCTION_SELECTION_HH */
