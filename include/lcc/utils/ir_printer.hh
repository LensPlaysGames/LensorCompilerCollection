#ifndef LCC_UTILS_IR_PRINTER_HH
#define LCC_UTILS_IR_PRINTER_HH

#include <lcc/context.hh>
#include <lcc/ir/ir.hh>
#include <lcc/ir/module.hh>
#include <lcc/utils.hh>

#include <iterator>
#include <string>
#include <unordered_map>
#include <utility>

namespace lcc {
// Colour Palette
struct IRColourPalette {
    using enum utils::Colour;
    // A color to use to write filler syntax, "unimportant" symbols, etc.
    static constexpr auto Filler = NormalWhite;
    // A color to use to write comments (beginning with ';')
    static constexpr auto Comment = NormalWhite;
    // A color to use to write names of declarations
    static constexpr auto Name = BoldDefault;
    // A color to use to write instruction opcodes (like store)
    static constexpr auto Opcode = NormalDefault;
    // A color to use to write the names of temporaries (like '%0')
    static constexpr auto Temp = BoldBlue;
    // A color to use to write the names of basic blocks (like 'bb0')
    static constexpr auto Block = NormalDefault;
    // A color to use to represent types
    static constexpr auto Type = BoldCyan;
    // A color to use to write literal data
    static constexpr auto Literal = BoldMagenta;
};

template <typename Derived, usz block_indent>
class IRPrinter {
private:
    std::string s{};
    isz tmp = 0;

    /// Map from blocks and instructions to their indices.
    std::unordered_map<Block*, isz> block_indices{};
    std::unordered_map<Inst*, isz> inst_indices{};


public:
    /// Entry point.
    [[nodiscard]]
    static auto Print(Module* mod, bool use_colour) -> std::string {
        return IRPrinter{use_colour}.PrintModule(mod);
    }

protected:
    explicit IRPrinter(bool use_colour) : _use_colour(use_colour) {}

    using P = IRColourPalette;

    bool _use_colour;
    utils::Colours C{_use_colour};

    /// Get the index of a block.
    auto Index(Block* b) const -> isz {
        if (not block_indices.contains(b)) return -1;
        return block_indices.at(b);
    }

    /// Get the index of an instruction.
    auto Index(Inst* inst) const -> isz {
        if (not inst_indices.contains(inst)) return -1;
        return inst_indices.at(inst);
    }

    /// Check if an instruction has an index.
    auto HasIndex(Inst* inst) const -> bool { return inst_indices.contains(inst); }

    /// Get an insert iterator to a string.
    static auto It(std::string& str) { return std::back_inserter(str); }

    /// Get the content to be printed.
    auto Output() -> std::string_view { return s; }

    /// Append text to the output.
    template <typename... Args>
    void Print(fmt::format_string<Args...> fmt, Args&&... args) {
        fmt::format_to(It(s), fmt, std::forward<Args>(args)...);
    }

    /// Emit a block and its containing instructions.
    void PrintBlock(Block* b) {
        for (usz i = 0; i < block_indent; i++) s += ' ';
        Print("{}bb{}{}:\n", C(P::Block), block_indices[b], C(P::Filler));
        for (auto inst : b->instructions()) {
            This()->PrintInst(inst);
            s += '\n';
        }
    }

    /// Emit a function definition or declaration.
    void PrintFunction(Function* f) {
        /// Print function signature.
        tmp = isz(f->param_count());
        This()->PrintFunctionHeader(f);
        if (f->blocks().empty()) {
            s += '\n';
            return;
        }

        SetFunctionIndices(f);

        /// Print the body.
        This()->EnterFunctionBody(f);
        for (auto b : f->blocks()) PrintBlock(b);
        This()->ExitFunctionBody(f);
    }

    [[nodiscard]]
    auto PrintModule(Module* mod) -> std::string {
        This()->PrintHeader(mod);
        for (auto struct_type : mod->context()->struct_types)
            This()->PrintStructType(struct_type);
        if (not mod->context()->struct_types.empty())
            s += '\n';
        for (auto var : mod->vars())
            This()->PrintGlobal(var);
        if (not mod->vars().empty())
            s += '\n';
        for (auto f : mod->code())
            PrintFunction(f);
        s += C(P::Reset);
        return std::move(s);
    }

    void SetFunctionIndices(Function* f) {
        for (auto [i, b] : vws::enumerate(f->blocks())) {
            block_indices[b] = i;
            for (auto inst : b->instructions()) {
                if (This()->RequiresTemporary(inst))
                    inst_indices[inst] = tmp++;
            }
        }
    }

    /// Get a pointer to the derived class.
    auto This() -> Derived* { return static_cast<Derived*>(this); }
};
} // namespace lcc

#endif // LCC_UTILS_IR_PRINTER_HH
