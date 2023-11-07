#include <lcc/opt/opt.hh>

namespace lcc::opt {
namespace {
/// Base class for all optimisation passes.
/// Optimisation pass that runs on an instruction kind.
struct OptimisationPass {
    Module* const mod;

    OptimisationPass(Module* mod) : mod(mod) {}

    /// Check if this pass has changed the ir.
    [[nodiscard]] bool changed() const { return has_changed; }

protected:
    /// Mark that this pass has changed the ir.
    void SetChanged() { has_changed = true; }

private:
    bool has_changed = false;
};

/// Optimisation pass that runs on an instruction kind.
///
/// API:
///
/// REQUIRED: void run(Inst* inst);
///
///     Called for every instruction `i` for which `is<I>(i)`
///     returns true, where `I` is any of the elements in the
///     pack `Instructions`.
///
template <typename... Instructions>
struct InstructionRewritePass : OptimisationPass {
    using IsInstRewritePass = std::true_type;

    /// Check if this an instruction should be handled by this pass.
    [[nodiscard]] static bool matches(Inst* inst) {
        return (Instructions::classof(inst) or ...);
    }
};

/// Optimisation pass that follows the control flow graph.
///
/// API:
///
/// REQUIRED: void run(Inst* inst);
///
///     Called for every instruction `i` for which `is<I>(i)`
///     returns true, where `I` is any of the elements in the
///     pack `Instructions`.
///
/// OPTIONAL: void atfork(Block* fork);
///
///     Called whenever there is a fork (conditional branch)
///     in the control flow graph. The `fork` block is the block
///     that contains the conditional branch.
///
/// OPTIONAL: void done();
///
///     Called when we’re done iterating over the control flow graph.
///
/// OPTIONAL: void enter(Block* block);
///
///     Called whenever we enter a new block.
///
/// OPTIONAL: void leave(Block* block);
///
///     Called whenever we leave a block.
///
struct CFGIterationPass : OptimisationPass {
    using IsCFGIterationPass = std::true_type;
};

/// Pass that deleted unused locals.
struct DeleteUnusedAllocasPass : InstructionRewritePass<AllocaInst> {
    void run(Inst* i) {
        auto alloca = as<AllocaInst>(i);

        /// If all of our uses are stores, then this alloca is dead.
        for (auto u : alloca->users())
            if (not is<StoreInst>(u))
                return;

        /// Delete the alloca and stores.
        alloca->erase_cascade();
        SetChanged();
    }
};

/// Pass that performs simple store forwarding.
struct StoreFowardingPass : CFGIterationPass {
    struct Var {
        AllocaInst* alloca;
        StoreInst* store{};
        Value* last_value{};
        bool last_store_used{};
        bool escaped{};
        bool needs_reload{};
    };

    std::vector<Var> vars{};

    /// This pass can’t handle forwarding stores across conditional branches.
    void atfork(Block*) { vars.clear(); }

    void done() {
        for (auto& var : vars) EraseLastStoreIfUnused(var);
    }

    void run(Inst* i) {
        /// Record a new variable.
        if (auto a = cast<AllocaInst>(i)) {
            vars.emplace_back(a);
            return;
        }

        /// Replace load if possible or cache new value.
        if (auto l = cast<LoadInst>(i)) {
            auto a = cast<AllocaInst>(l->ptr());
            if (not a) return;
            auto var = rgs::find(vars, a, &Var::alloca);

            /// No alloca for load.
            if (var == vars.end()) return;

            /// Cache this value if there was no last value.
            if (not var->last_value or var->needs_reload) {
                var->needs_reload = false;
                var->store = nullptr;
                var->last_value = l;
                return;
            }

            /// Otherwise, replace this with the last value.
            l->replace_with(var->last_value);
            var->last_store_used = true;
            SetChanged();
            return;
        }

        /// Update the value.
        if (auto s = cast<StoreInst>(i)) {
            /// If this store stores the address of the alloca
            /// somewhere else, mark the variable as escaped.
            if (auto a = cast<AllocaInst>(s->val())) {
                auto var = rgs::find(vars, a, &Var::alloca);
                if (var != vars.end()) var->escaped = true;
            }

            /// Check if we’re storing to an address.
            auto a = cast<AllocaInst>(s->ptr());
            if (not a) return;
            auto var = rgs::find(vars, a, &Var::alloca);

            /// No alloca for store.
            if (var == vars.end()) return;

            /// Erase the last value if it was as store and ended
            /// up not being used by anything.
            EraseLastStoreIfUnused(*var);
            var->store = s;
            var->last_value = s->val();
            var->last_store_used = false;
            var->needs_reload = false;
            return;
        }

        /// Any other instruction that uses an alloca escapes that alloca.
        for (auto a : i->children_of_kind<AllocaInst>()) {
            auto var = rgs::find(vars, a, &Var::alloca);
            if (var != vars.end()) var->escaped = true;
            else {
                vars.emplace_back(a);
                vars.back().escaped = true;
            }
        }

        /// Calls invalidate all escaped values.
        if (is<CallInst>(i)) {
            for (auto& var : vars) {
                if (var.escaped) {
                    var.last_store_used = true;
                    var.needs_reload = true;
                }
            }
        }
    }

private:
    /// Erase the last store if it ended up unused.
    void EraseLastStoreIfUnused(Var& var) {
        if (var.store and var.last_value == var.store->val() and not var.last_store_used) {
            var.store->erase();
            SetChanged();
        }
    }
};

struct Optimiser {
    Module* const mod;

    /// TODO: Actually use this.
    [[maybe_unused]] int const opt_level;

    /// Entry point.
    void run() { // clang-format off
        RunPasses<
            DeleteUnusedAllocasPass,
            StoreFowardingPass
        >();
    } // clang-format on

private:
    template <typename... Passes>
    void RunPasses() {
        for (auto f : mod->code()) {
            auto RunPass = [&]<typename Pass> {
                Pass p{mod};

                /// Use indices here to avoid iterator invalidation.
                for (usz bi = 0; bi < f->blocks().size(); bi++) {
                    /// Call enter() callback if there is one.
                    if constexpr (
                        requires { typename Pass::IsCFGIterationPass; } and
                        requires { p.enter(f->blocks()[bi]); }
                    ) p.enter(f->blocks()[bi]);

                    for (usz ii = 0; ii < f->blocks()[bi]->instructions().size(); ii++) {
                        /// Some passes may end up deleting all remaining instructions,
                        /// so make sure to check that we still have instructions left
                        /// after each pass.
                        if (
                            bi >= f->blocks().size() or
                            ii >= f->blocks()[bi]->instructions().size()
                        ) return p.changed();

                        /// Instruction rewrite pass.
                        if constexpr (requires { typename Pass::IsInstRewritePass; }) {
                            auto* inst = f->blocks()[bi]->instructions()[ii];
                            if (Pass::matches(inst)) p.run(inst);
                        }

                        /// CGF iteration pass.
                        else if constexpr (requires { typename Pass::IsCFGIterationPass; }) {
                            p.run(f->blocks()[bi]->instructions()[ii]);
                        }

                        /// Unknown pass.
                        else {
                            static_assert(always_false<Pass>, "Invalid pass");
                        }
                    }

                    /// Call leave() callback if there is one.
                    if constexpr (
                        requires { typename Pass::IsCFGIterationPass; } and
                        requires { p.leave(f->blocks()[bi]); }
                    ) p.leave(f->blocks()[bi]);

                    /// Call atfork() callback if there is one and we’re at a fork.
                    if constexpr (
                        requires { typename Pass::IsCFGIterationPass; } and
                        requires { p.atfork(f->blocks()[bi]); }
                    ) {
                        auto b = f->blocks()[bi];
                        if (b->terminator() and is<CondBranchInst>(b->terminator()))
                            p.atfork(b);
                    }
                }

                /// Call done() callback if there is one.
                if constexpr (
                    requires { typename Pass::IsCFGIterationPass; } and
                    requires { p.done(); }
                ) p.done();

                return p.changed();
            };

            /// Run all passes so long as at least one of them returns true.
            while ((int(RunPass.template operator()<Passes>()) | ...)) {}
        }
    }
};

} // namespace
} // namespace lcc::opt

void lcc::opt::Optimise(Module* module, int opt_level) {
    Optimiser o{module, opt_level};
    o.run();
}
