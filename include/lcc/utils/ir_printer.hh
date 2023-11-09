#ifndef LCC_UTILS_IR_PRINTER_HH
#define LCC_UTILS_IR_PRINTER_HH

#include <lcc/context.hh>
#include <lcc/ir/ir.hh>
#include <lcc/ir/module.hh>
#include <lcc/utils.hh>

namespace lcc {
template <typename Derived, usz block_indent>
class IRPrinter {
private:
    std::string s{};
    isz tmp = 0;

    /// Map from blocks and instructions to their indices.
    std::unordered_map<Block*, isz> block_indices{};
    std::unordered_map<Inst*, isz> inst_indices{};

    explicit IRPrinter(bool use_colour) : use_colour(use_colour) {}

public:
    /// Entry point.
    static auto Print(Module* mod, bool use_colour) -> std::string {
        return IRPrinter{use_colour}.PrintModule(mod);
    }

protected:
    bool use_colour;
    utils::Colours C{use_colour};
    using enum utils::Colour;

    /// Couldn’t decide on a colour for temporaries, so...
    static constexpr auto TempColour = utils::Colour::Blue;
    static constexpr auto BlockColour = utils::Colour::Green;

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

    /// Append text to the output.
    template <typename... Args>
    void Print(fmt::format_string<Args...> fmt, Args&&... args) {
        fmt::format_to(It(s), fmt, std::forward<Args>(args)...);
    }

private:
    /// Emit a block and its containing instructions.
    void PrintBlock(Block* b) {
        for (usz i = 0; i < block_indent; i++) s += ' ';
        Print("{}bb{}{}:\n", C(BlockColour), block_indices[b], C(Red));
        for (auto inst : b->instructions()) {
            This()->PrintInst(inst);
            s += '\n';
        }
    }

    /// Emit a function definition or declaration as LLVM IR.
    void PrintFunction(Function* f) {
        /// Print function signature.
        tmp = isz(f->param_count());
        This()->PrintFunctionHeader(f);
        if (f->blocks().empty()) {
            s += '\n';
            return;
        }

        /// Set indices for instructions and blocks.
        for (auto [i, b] : vws::enumerate(f->blocks())) {
            block_indices[b] = i;
            for (auto inst : b->instructions())
                if (This()->RequiresTemporary(inst))
                    inst_indices[inst] = tmp++;
        }

        /// Print the body.
        This()->EnterFunctionBody(f);
        for (auto b : f->blocks()) PrintBlock(b);
        This()->ExitFunctionBody(f);
    }

    auto PrintModule(Module* mod) -> std::string {
        This()->PrintHeader();
        for (auto struct_type : mod->context()->struct_types) This()->PrintStructType(struct_type);
        if (not mod->context()->struct_types.empty()) s += '\n';
        for (auto var : mod->vars()) This()->PrintGlobal(var);
        if (not mod->vars().empty()) s += '\n';
        bool first = true;
        for (auto f : mod->code()) {
            if (first) first = false;
            else s += '\n';
            PrintFunction(f);
        }
        s += C(Reset);
        return std::move(s);
    }

    /// Get a pointer to the derived class.
    auto This() -> Derived* { return static_cast<Derived*>(this); }
};
} // namespace lcc

#endif // LCC_UTILS_IR_PRINTER_HH
