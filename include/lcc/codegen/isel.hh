#ifndef LCC_CODEGEN_INSTRUCTION_SELECTION_HH
#define LCC_CODEGEN_INSTRUCTION_SELECTION_HH

#include <lcc/codegen/mir.hh>
#include <lcc/ir/module.hh>
#include <lcc/utils.hh>
#include <lcc/utils/result.hh>
#include <unordered_map>
#include <variant>

namespace lcc {
namespace isel {

/// `while`-like iteration over a template parameter pack.
template <typename... pack>
constexpr void While(bool& cond, auto&& lambda) {
    auto impl = [&]<typename t>() {
        if (not cond) return false;
        lambda.template operator()<t>();
        return true;
    };

    (impl.template operator()<pack>() and ...);
}

template <typename... pack>
constexpr void Foreach(auto&& lambda) {
    (lambda.template operator()<pack>(), ...);
}

enum struct OperandKind {
    Immediate,
    // Eventually an immediate operand; takes an input operand index and gets
    // the size from it IN BITS. BITS. I'll say it a third time, IN BITS.
    Sizeof,

    Register,
    // For use in the output of patterns when a temporary register is needed.
    NewVirtual,
    // References an input register operand but changes the size.
    // Takes an index of a register operand from the input to copy the value
    // of and an explicit size to use instead of the copied one.
    ResizedRegister,

    // A reference to a stack frame object.
    Local,
    OffsetLocal,
    // A reference to a symbol.
    Global,

    Function,
    Block,

    // Resolves to a Register
    InputInstructionReference,
    InputOperandReference,

    // TODO: The operand kinds
};

// NOTE: The operand values are a bit scuffed, in that each one needs all
// of the accessors in order for everything to compile...

template <i64 imm = 0, u64 _size = 0>
struct Immediate {
    using sz = Immediate<>;
    using offset = Immediate<>;
    static constexpr OperandKind kind = OperandKind::Immediate;
    static constexpr i64 immediate = imm;
    static constexpr usz index = 0;
    static constexpr usz value = 0;
    static constexpr usz size = _size;
    static constexpr GlobalVariable* global = nullptr;
    static constexpr lcc::Function* function = nullptr;
    static constexpr lcc::Block* block = nullptr;
};

template <usz idx>
struct Sizeof {
    using sz = Immediate<>;
    using offset = Immediate<>;
    static constexpr OperandKind kind = OperandKind::Sizeof;
    static constexpr i64 immediate = 0;
    static constexpr usz index = idx;
    static constexpr usz value = 0;
    static constexpr usz size = 0;
    static constexpr GlobalVariable* global = nullptr;
    static constexpr lcc::Function* function = nullptr;
    static constexpr lcc::Block* block = nullptr;
};

template <usz val = 0, typename _size = Immediate<>>
struct Register {
    using sz = _size;
    using offset = Immediate<>;
    static constexpr OperandKind kind = OperandKind::Register;
    static constexpr i64 immediate = 0;
    static constexpr usz index = 0;
    static constexpr usz value = val;
    static constexpr usz size = 0;
    static constexpr GlobalVariable* global = nullptr;
    static constexpr lcc::Function* function = nullptr;
    static constexpr lcc::Block* block = nullptr;
};

template <typename... _>
struct Local {
    using sz = Immediate<>;
    using offset = Immediate<>;
    static constexpr OperandKind kind = OperandKind::Local;
    static constexpr i64 immediate = 0;
    static constexpr usz index = 0;
    static constexpr usz value = 0;
    static constexpr usz size = 0;
    static constexpr GlobalVariable* global = nullptr;
    static constexpr lcc::Function* function = nullptr;
    static constexpr lcc::Block* block = nullptr;
};

template <typename local_op, typename immediate_op>
struct OffsetLocal {
    using sz = local_op;
    using offset = immediate_op;
    static constexpr OperandKind kind = OperandKind::OffsetLocal;
    static constexpr i64 immediate = 0;
    static constexpr usz index = 0;
    static constexpr usz value = 0;
    static constexpr usz size = 0;
    static constexpr GlobalVariable* global = nullptr;
    static constexpr lcc::Function* function = nullptr;
    static constexpr lcc::Block* block = nullptr;
};

template <typename... _>
struct Global {
    using sz = Immediate<>;
    using offset = Immediate<>;
    static constexpr OperandKind kind = OperandKind::Global;
    static constexpr i64 immediate = 0;
    static constexpr usz index = 0;
    static constexpr usz value = 0;
    static constexpr usz size = 0;
    static constexpr GlobalVariable* global = nullptr;
    static constexpr lcc::Function* function = nullptr;
    static constexpr lcc::Block* block = nullptr;
};

template <typename... _>
struct Function {
    using sz = Immediate<>;
    using offset = Immediate<>;
    static constexpr OperandKind kind = OperandKind::Function;
    static constexpr i64 immediate = 0;
    static constexpr usz index = 0;
    static constexpr usz value = 0;
    static constexpr usz size = 0;
    static constexpr GlobalVariable* global = nullptr;
    static constexpr lcc::Function* function = nullptr;
    static constexpr lcc::Block* block = nullptr;
};

template <typename... _>
struct Block {
    using sz = Immediate<>;
    using offset = Immediate<>;
    static constexpr OperandKind kind = OperandKind::Block;
    static constexpr i64 immediate = 0;
    static constexpr usz index = 0;
    static constexpr usz value = 0;
    static constexpr usz size = 0;
    static constexpr GlobalVariable* global = nullptr;
    static constexpr lcc::Function* function = nullptr;
    static constexpr lcc::Block* block = nullptr;
};

// New virtual register, by unique index within each pattern.
template <usz idx, usz op_idx>
struct v {
    using sz = Immediate<>;
    using offset = Immediate<>;
    static constexpr OperandKind kind = OperandKind::NewVirtual;
    static constexpr i64 immediate = 0;
    static constexpr usz index = idx;
    static constexpr usz value = 0;
    static constexpr usz size = op_idx;
    static constexpr GlobalVariable* global = nullptr;
    static constexpr lcc::Function* function = nullptr;
    static constexpr lcc::Block* block = nullptr;
};

template <usz op_idx, usz new_size>
struct ResizedRegister {
    using sz = Immediate<>;
    using offset = Immediate<>;
    static constexpr OperandKind kind = OperandKind::ResizedRegister;
    static constexpr i64 immediate = 0;
    static constexpr usz index = op_idx;
    static constexpr usz value = 0;
    static constexpr usz size = new_size;
    static constexpr GlobalVariable* global = nullptr;
    static constexpr lcc::Function* function = nullptr;
    static constexpr lcc::Block* block = nullptr;
};

// Operand reference, by index.
template <usz idx>
struct o {
    using sz = Immediate<>;
    using offset = Immediate<>;
    static constexpr OperandKind kind = OperandKind::InputOperandReference;
    static constexpr i64 immediate = 0;
    static constexpr usz index = idx;
    static constexpr usz value = 0;
    static constexpr usz size = 0;
    static constexpr GlobalVariable* global = nullptr;
    static constexpr lcc::Function* function = nullptr;
    static constexpr lcc::Block* block = nullptr;
};

// Instruction reference, by index.
template <usz idx>
struct i {
    using sz = Immediate<>;
    using offset = Immediate<>;
    static constexpr OperandKind kind = OperandKind::InputInstructionReference;
    static constexpr i64 immediate = 0;
    static constexpr usz index = idx;
    static constexpr usz value = 0;
    static constexpr usz size = 0;
    static constexpr GlobalVariable* global = nullptr;
    static constexpr lcc::Function* function = nullptr;
    static constexpr lcc::Block* block = nullptr;
};

enum struct ClobberKind {
    Operand,
    RegisterValue,
};
// A list of clobbers (c<index> or r<value>)
template <typename... clobbers>
struct Clobbers {
    static constexpr void foreach (auto&& lambda) {
        (lambda.template operator()<clobbers>(), ...);
    }
};
template <usz idx>
struct c {
    static constexpr ClobberKind kind = ClobberKind::Operand;
    static constexpr usz index = idx;
    static constexpr usz value = 0;
};
template <usz register_value>
struct r {
    static constexpr ClobberKind kind = ClobberKind::RegisterValue;
    static constexpr usz index = 0;
    static constexpr usz value = register_value;
};

template <typename clobbers_, usz opcode_, typename... operands>
struct Inst {
    using clobbers = clobbers_;

    static constexpr usz opcode = opcode_;

    static constexpr usz operand_count = sizeof...(operands);

    static constexpr void foreach_operand(auto&& lambda) {
        (lambda.template operator()<operands>(), ...);
    }

    template <typename operand>
    static constexpr MOperand get_operand(
        Module* mod,
        MFunction& function,       // for locals lookup
        std::vector<MInst*> input, // for Input*Reference,
        std::unordered_map<usz, usz>& new_virtuals
    ) {
        const auto input_operand_by_index = [&](usz index) -> Result<MOperand> {
            // Current operand index.
            usz i = 0;
            for (auto instruction : input) {
                for (auto op_candidate : instruction->all_operands()) {
                    // This is the instruction we are looking for!
                    if (i == index) return op_candidate;

                    // Increment operand index for the next operand.
                    ++i;
                }
            }
            return {};
        };

        switch (operand::kind) {
            case OperandKind::Immediate:
                return MOperandImmediate(operand::immediate);

            case OperandKind::Sizeof: {
                auto op_result = input_operand_by_index(operand::index);
                // FIXME: Which pattern? Possible to include it in error message somehow?
                LCC_ASSERT(op_result, "Pattern has ill-formed Sizeof<{}> operand: index greater than amount of operands in input.", operand::index);
                auto op = *op_result;

                u64 size{};
                if (std::holds_alternative<MOperandRegister>(op))
                    size = std::get<MOperandRegister>(op).size;
                else if (std::holds_alternative<MOperandImmediate>(op))
                    size = std::get<MOperandImmediate>(op).size;
                else if (std::holds_alternative<MOperandGlobal>(op))
                    size = std::get<MOperandGlobal>(op)->allocated_type()->bits();
                else if (std::holds_alternative<MOperandLocal>(op)) {
                    auto* local = function.locals().at(std::get<MOperandLocal>(op).index);
                    size = local->allocated_type()->bits();
                } else LCC_ASSERT(
                    false,
                    "Unhandled operand kind in Sizeof handling, sorry"
                );

                return MOperandImmediate(size);
            }

            case OperandKind::Register: {
                uint size{};
                switch (operand::sz::kind) {
                    case OperandKind::Immediate: {
                        size = operand::sz::immediate;
                    } break;

                    case OperandKind::Sizeof: {
                        auto op = get_operand<typename operand::sz>(mod, function, input, new_virtuals);
                        size = uint(std::get<MOperandImmediate>(op).value);
                    } break;

                    default:
                        LCC_ASSERT(
                            false,
                            "Unhandled operand kind in sz field (size) of Register, sorry"
                        );
                }
                LCC_ASSERT(
                    size != 0,
                    "Zero-sized register is not allowed!"
                );
                return MOperandRegister(operand::value, size);
            }

            case OperandKind::ResizedRegister: {
                auto op_result = input_operand_by_index(operand::index);
                // FIXME: Which pattern? Possible to include it in error message somehow?
                LCC_ASSERT(op_result, "Pattern has ill-formed ResizedRegister<{}, {}> operand: index greater than amount of operands in input.", operand::index, operand::size);
                auto op = *op_result;
                LCC_ASSERT(std::holds_alternative<MOperandRegister>(op), "Pattern has ill-formed ResizedRegister<{}, {}> operand: index within bounds, but does not reference a register operand", operand::index, operand::size);

                return MOperandRegister(std::get<MOperandRegister>(op).value, operand::size);
            }

            case OperandKind::NewVirtual: {
                if (not new_virtuals.contains(operand::index)) {
                    new_virtuals[operand::index] = mod->next_vreg();
                }

                MOperand op{};
                // Current operand index.
                usz i = 0;
                // Operand index we need to find.
                usz needle = operand::size;
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

                usz size = 0;
                if (std::holds_alternative<MOperandRegister>(op))
                    size = std::get<MOperandRegister>(op).size;
                else if (std::holds_alternative<MOperandImmediate>(op))
                    size = std::get<MOperandImmediate>(op).size;
                else LCC_ASSERT(false, "Sorry, moperand type not handled in NewVirtual handling...");

                return MOperandRegister(new_virtuals[operand::index], uint(size));
            }

            case OperandKind::Local:
                return MOperandLocal(operand::index);

            case OperandKind::OffsetLocal: {
                auto local = get_operand<typename operand::sz>(mod, function, input, new_virtuals);
                auto offset = get_operand<typename operand::offset>(mod, function, input, new_virtuals);
                LCC_ASSERT(std::holds_alternative<MOperandLocal>(local), "OffsetLocal operand requires Local as first operand");
                LCC_ASSERT(std::holds_alternative<MOperandImmediate>(offset), "OffsetLocal operand requires Immediate as second operand");
                auto local_op = std::get<MOperandLocal>(local);
                return MOperandLocal(local_op.index, local_op.offset + i32(std::get<MOperandImmediate>(offset).value));
            }

            case OperandKind::Global:
                return MOperandGlobal(operand::global);

            case OperandKind::Function:
                return MOperandFunction(operand::function);

            case OperandKind::Block:
                return MOperandGlobal(operand::block);

            case OperandKind::InputOperandReference: {
                auto op_result = input_operand_by_index(operand::index);
                // FIXME: Which pattern? Possible to include it in error message somehow?
                LCC_ASSERT(op_result, "Pattern has ill-formed o<{}> operand: index greater than amount of operands in input.", operand::index);
                return *op_result;
            }

            case OperandKind::InputInstructionReference: {
                MOperand op{};
                // Current instruction index.
                usz i = 0;
                // Instruction index we need to find.
                usz needle = operand::index;
                // Whether or not we've found the instruction we are looking for.
                bool found = false;
                for (auto instruction : input) {
                    if (i == needle) {
                        found = true;
                        op = MOperandRegister(instruction->reg(), uint(instruction->regsize()));
                    }
                    if (found) break;
                    ++i;
                }

                LCC_ASSERT(found, "Pattern has ill-formed i<{}> operand: index greater than amount of instructions in input.", needle);

                return op;
            }
        }
        LCC_UNREACHABLE();
    }
};

template <typename... instructions>
struct InstList {
    static constexpr usz size() {
        return (sizeof(instructions), ...);
    }

    static constexpr void foreach (auto&& lambda) {
        (lambda.template operator()<instructions>(), ...);
    }
};

template <typename in, typename out>
struct Pattern {
    using input = in;
    using output = out;
};

template <typename... Patterns>
struct PatternList {
    static MFunction rewrite(lcc::Module* mod, MFunction& function) {
        MFunction out{function.calling_convention()};
        out.names() = function.names();
        out.locals() = function.locals();
        out.location(function.location());

        // Get the longest input pattern length
        usz longest_pattern_length = 0;
        Foreach<Patterns...>([&]<typename _> {
            ++longest_pattern_length;
        });
        LCC_ASSERT(longest_pattern_length, "Cannot do instruction selection with no input patterns to match");

        // NOTE: If you modify block.instructions() in any way, you are going to
        // have a bad time.
        std::vector<MInst*> instructions{};
        // Any instructions we allocte are stored here, so that we can free them
        // after everything has been inserted as a value. Ideally we wouldn't have
        // to do this, but I can't make a vector of references, sadly.
        std::vector<MInst*> pool{};

        for (auto& old_block : function.blocks()) {
            out.add_block(MBlock(old_block));
            auto& new_block = out.blocks().back();
            new_block.instructions().clear();

            usz instructions_handled = 0;
            do {
                for (; instructions_handled < old_block.instructions().size(); ++instructions_handled) {
                    // Add (up to) `longest_pattern_length` instructions to the instructions vector.
                    if (instructions.size() >= longest_pattern_length) break;
                    instructions.push_back(old_block.instructions().data() + instructions_handled);
                }

                bool to_be_handled = true;
                While<Patterns...>(to_be_handled, [&]<typename pattern>() {
                    // If the input pattern is longer than the current amount of instructions,
                    // skip this pattern.
                    if (pattern::input::size() > instructions.size()) return;

                    // Ensure all opcodes and operand types match from the input pattern.
                    usz input_i = 0;
                    bool pattern_matches = true;
                    pattern::input::foreach ([&]<typename inst> {
                        // If the pattern has failed to match, skip the rest of the instructions
                        // in the pattern.
                        if (not pattern_matches) return;

                        auto& instruction = instructions[input_i];

                        // If the `i`th input instruction's opcode doesn't match the `i`th
                        // instruction window's opcode, the pattern does not match.
                        if (instruction->opcode() != inst::opcode) {
                            pattern_matches = false;
                            return;
                        }

                        // Ensure operand amount and kinds match the input pattern.
                        bool operands_match = false;
                        if (inst::operand_count == instruction->all_operands().size()) {
                            operands_match = true;

                            usz op_i = 0;
                            inst::foreach_operand([&]<typename op> {
                                // If the operands have failed to match, skip the rest of the operands.
                                if (not operands_match) return;

                                auto& operand = instruction->all_operands().at(op_i);
                                if (std::holds_alternative<MOperandImmediate>(operand)) {
                                    operands_match = op::kind == OperandKind::Immediate;
                                } else if (std::holds_alternative<MOperandRegister>(operand)) {
                                    operands_match = op::kind == OperandKind::Register;
                                } else if (std::holds_alternative<MOperandLocal>(operand)) {
                                    operands_match = op::kind == OperandKind::Local;
                                } else if (std::holds_alternative<MOperandGlobal>(operand)) {
                                    operands_match = op::kind == OperandKind::Global;
                                } else if (std::holds_alternative<MOperandFunction>(operand)) {
                                    operands_match = op::kind == OperandKind::Function;
                                } else if (std::holds_alternative<MOperandBlock>(operand)) {
                                    operands_match = op::kind == OperandKind::Block;
                                } else {
                                    LCC_ASSERT(false, "Unhandled MIR Operand Kind in ISel...");
                                }
                                ++op_i;
                            });
                        }

                        pattern_matches = operands_match;

                        ++input_i;
                    });

                    // If pattern does not match, continue trying more patterns.
                    if (not pattern_matches) return;

                    // Remove pattern input instruction(s) from instruction window,
                    // keeping references to it/them.
                    std::vector<MInst*> input{};
                    input.insert(input.begin(), instructions.begin(), instructions.begin() + pattern::input::size());
                    instructions.erase(instructions.begin(), instructions.begin() + pattern::input::size());

                    // Add pattern output instruction(s) to instruction window, fixing
                    // up reference-type operands (operand references get updated to the
                    // operand they reference).

                    // Map of the index from v<index> to the id of the new virtual register it
                    // should be replaced with.
                    std::unordered_map<usz, usz> new_virtuals{};

                    isz output_i = 0;
                    pattern::output::foreach ([&]<typename inst> {
                        // Use instruction's vreg from input of pattern.
                        auto output = new MInst(inst::opcode, {input.back()->reg(), uint(input.back()->regsize())});
                        // Use instruction's location from input of pattern.
                        output->location(input.back()->location());

                        // Keep track of newly allocated machine instructions.
                        pool.push_back(output);
                        instructions.insert(instructions.begin() + output_i, output);

                        inst::clobbers::foreach ([&]<typename clobber> {
                            if constexpr (clobber::kind == ClobberKind::Operand)
                                output->add_operand_clobber(clobber::index);
                            else if constexpr (clobber::kind == ClobberKind::RegisterValue)
                                LCC_ASSERT(false, "TODO: Register clobbers member in MIR");
                        });

                        inst::foreach_operand([&]<typename op> {
                            output->add_operand(inst::template get_operand<op>(mod, function, input, new_virtuals));
                        });

                        // Stupidly match use count (not sure if even necessary).
                        usz use_count = input.back()->use_count();
                        while (use_count--) output->add_use();

                        ++output_i;
                    });

                    to_be_handled = false; // break
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

        return out;
    }
};

} // namespace isel

void select_instructions(Module* ctx, MFunction& function);

} // namespace lcc

#endif /* LCC_CODEGEN_INSTRUCTION_SELECTION_HH */
