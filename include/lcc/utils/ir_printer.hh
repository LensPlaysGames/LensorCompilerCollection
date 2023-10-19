#ifndef LCC_UTILS_IR_PRINTER_HH
#define LCC_UTILS_IR_PRINTER_HH

#include <lcc/ir/ir.hh>
#include <lcc/ir/module.hh>
#include <lcc/utils.hh>

namespace lcc {
template <typename Derived, usz block_indent>
class IRPrinter {
private:
    std::string s{};
    usz tmp = 0;

    /// Map from blocks and instructions to their indices.
    std::unordered_map<Block*, usz> block_indices{};
    std::unordered_map<Inst*, usz> inst_indices{};

public:
    /// Entry point.
    static auto Print(Module* mod) -> std::string {
        return IRPrinter{}.PrintModule(mod);
    }

protected:
    /// Get the index of a block.
    auto Index(Block* b) const -> usz { return block_indices.at(b); }

    /// Get the index of an instruction.
    auto Index(Inst* inst) const -> usz { return inst_indices.at(inst); }

    /// Check if an instruction has an index.
    auto HasIndex(Inst* inst) const -> bool { return inst_indices.contains(inst); }

    /// Get an insert iterator to a string.
    auto It(std::string& str) { return std::back_inserter(str); }

    /// Append text to the output.
    template <typename... Args>
    void Print(fmt::format_string<Args...> fmt, Args&&... args) {
        fmt::format_to(It(s), fmt, std::forward<Args>(args)...);
    }

private:
    /// Emit a block and its containing instructions.
    void PrintBlock(Block* b) {
        for (usz i = 0; i < block_indent; i++) s += ' ';
        Print("bb{}:\n", block_indices[b]);
        for (auto inst : b->instructions()) {
            This()->PrintInst(inst);
            s += '\n';
        }
    }

    /// Emit a function definition or declaration as LLVM IR.
    void PrintFunction(Function* f) {
        /// Print function signature.
        tmp = f->param_count();
        This()->PrintFunctionHeader(f);
        if (f->blocks().empty()) {
            s += '\n';
            return;
        }

        /// Set indices for instructions and blocks.
        for (auto [i, b] : vws::enumerate(f->blocks())) {
            block_indices[b] = usz(i);
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
        bool first = true;
        for (auto f : mod->code()) {
            if (first) first = false;
            else s += '\n';
            PrintFunction(f);
        }
        return std::move(s);
    }

    /// Get a pointer to the derived class.
    auto This() -> Derived* { return static_cast<Derived*>(this); }
};
} // namespace lcc

#endif // LCC_UTILS_IR_PRINTER_HH
