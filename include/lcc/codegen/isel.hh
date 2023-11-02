#ifndef LCC_CODEGEN_INSTRUCTION_SELECTION_HH
#define LCC_CODEGEN_INSTRUCTION_SELECTION_HH

#include <lcc/utils.hh>
#include <lcc/ir/module.hh>
#include <lcc/codegen/mir.hh>
#include <unordered_map>

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

enum struct OperandKind {
    Immediate,
    InputOperandReference,
    // TODO: The operand kinds
};

// NOTE: The operand values are a bit scuffed, in that each one needs all
// of the accessors in order for everything to compile...

template<i64 imm = 0>
struct Immediate {
    static constexpr i64 immediate = imm;
    static constexpr usz index = usz(-1);
};

// Operand reference, by index.
template <usz idx>
struct o {
    static constexpr i64 immediate = 0;
    static constexpr usz index = idx;
};

template<OperandKind kind_, typename value_>
struct Operand{
    static constexpr const auto kind = kind_;
    using value = value_;
};


template<usz opcode_, typename... operands>
struct Inst {
    static constexpr usz opcode = opcode_;

    // FIXME: We may need another parameter denoting the index of input and
    // output we are "currently on" or whatever.
    static constexpr void rewrite_operands(std::vector<MInst*> input, MInst* out) {
        LCC_ASSERT(out, "Out parameter must not be nullptr");
        Foreach<operands...>([&]<typename operand>() {
                // Initialise operand based on output pattern.
                MOperand op{};
                switch (operand::kind) {
                    case OperandKind::Immediate:
                        op = MOperandImmediate(operand::value::immediate);
                        break;

                    case OperandKind::InputOperandReference: {
                        // Current operand index.
                        usz i = 0;
                        // Operand index we need to find.
                        usz needle = operand::value::index;
                        // Whether or not we've found the operand we are looking for.
                        bool found = false;
                        for (auto instruction : input) {
                            for (auto op_candidate : instruction->all_operands()) {
                                // This is the instruction we are looking for!
                                if (i == needle) {
                                    found = true;
                                    op = op_candidate;
                                }

                                // We can stop looking at operands if we found the one we needed.
                                if (found) break;

                                // Increment operand index for the next operand.
                                ++i;
                            }
                            // We can stop looking at instructions if we found the operand we needed.
                            if (found) break;
                        }

                        // FIXME: Which pattern? Possible to include it in error message somehow?
                        LCC_ASSERT(found, "Pattern has ill-formed o<{}> operand: index greater than amount of operands in input.", needle);

                    } break;
                }

                out->add_operand(op);
            });
    }
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
        // Any instructions we allocte are stored here, so that we can free them
        // after everything has been inserted as a value. Ideally we wouldn't have
        // to do this, but I can't make a vector of references, sadly.
        std::vector<MInst*> pool{};

        for (auto& old_block : function.blocks()) {
            out.add_block(MBlock(old_block.name()));
            auto& new_block = out.blocks().back();
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
                            // Remove pattern input instruction(s) from instruction window,
                            // keeping references to it/them.
                            std::vector<MInst*> input{};
                            // TODO: Handle multiple input instructions
                            input.push_back(instructions.front());
                            instructions.erase(instructions.begin());


                            // Add pattern output instruction(s) to instruction window, fixing
                            // up reference-type operands (operand references get updated to the
                            // operand they reference). This is implemented in `Inst::rewrite_operands`.

                            // TODO: foreach output instruction...

                            // Use last instruction's vreg from input of pattern. TODO: Pass size.
                            auto output = new MInst(pattern::output::opcode, input.back()->reg());

                            // Match use count.
                            // TODO: How will multiple output instructions be handled here? Do we need
                            // to calculate the matched pattern's use counts and set those?
                            usz use_count = input.back()->use_count();
                            while (use_count) output->add_use();

                            pool.push_back(output);
                            instructions.insert(instructions.begin(), output);
                            pattern::output::rewrite_operands(input, output);

                            to_be_handled = false; // break
                        }
                    });

                // If we get through *all* of the patterns, and none of them matched, we
                // can pop an instruction off the front and emit it into the output,
                // before going back to the "add instructions" bit.
                if (to_be_handled) {
                    if (instructions.size()) {
                        // Add front of `instructions` to emission output.
                        auto inst = instructions.front();
                        new_block.insert(*inst);
                        // Pop instruction from front of instruction window.
                        instructions.erase(instructions.begin());
                        // If instruction was a temporary allocation, free it.
                        if (auto found = rgs::find(pool, inst) != pool.end()) {
                            std::erase(pool, inst);
                            delete inst;
                        }
                    }
                }

                // If there are still instructions in the window to be handled, go back
                // and handle them (after topping off the window with any more
                // instructions that there may be). If the window is empty, but there are
                // still more instructions to handle in this block, also go back and
                // handle them.
            } while (instructions.size() != 0 || instructions_handled < old_block.instructions().size());
        }

        fmt::print("{}\n", PrintMFunction(out));

        return out;
    }
};


} // namespace isel

void select_instructions(Module* ctx, MFunction& function);

} // namespace lcc

#endif /* LCC_CODEGEN_INSTRUCTION_SELECTION_HH */
