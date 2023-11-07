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
    /// Create a new instruction, replace another instruction with
    /// it, and mark that a change has occurred.
    template <typename Instruction, typename... Args>
    auto Replace(Inst* what, Args&&... args) -> Instruction* {
        auto i = new (*mod) Instruction(std::forward<Args>(args)...);
        what->replace_with(i);
        SetChanged();
        return i;
    }

    /// Replace an instruction with a value.
    auto Replace(Inst* i, Value* v) {
        i->replace_with(v);
        SetChanged();
    }

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
///     Called for every instruction in a function.
///
struct InstructionRewritePass : OptimisationPass {
    using IsInstRewritePass = std::true_type;
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

/// Pass that performs constant folding and propagation. Passes that
/// operate on individual instructions and don’t really fit in anywhere
/// else can also go here.
struct InstCombinePass : InstructionRewritePass {
private:
    /// Get the lhs and rhs of a binary expression as integer constants.
    static auto GetIntegerPair(BinaryInst* b) {
        using IC = IntegerConstant;

        struct Result {
            bool pair;
            u64 lhs;
            u64 rhs;
        };

        if (is<IC>(b->lhs()) and is<IC>(b->rhs())) return Result{
            true,
            cast<IC>(b->lhs())->value(),
            cast<IC>(b->rhs())->value(),
        };

        return Result{false, 0, 0};
    }

    /// Handle signed and unsigned division.
    template <typename DivInst, typename ShiftInst, auto Eval>
    void DivImpl(Inst* i) {
        auto d = as<DivInst>(i);
        auto rhs = cast<IntegerConstant>(d->rhs());
        if (not rhs) return;

        /// Check for division by zero.
        if (rhs->value() == 0) Replace<PoisonValue>(i, d->type());

        /// Division by 1 is a no-op.
        else if (rhs->value() == 1) Replace(i, d->lhs());

        /// Evaluate the division if both operands are constants.
        else if (auto lhs = cast<IntegerConstant>(d->lhs())) {
            auto result = new (*mod) IntegerConstant(d->type(), u64(Eval(lhs->value(), rhs->value())));
            Replace(i, result);
        }

        /// Division by a power of two is a right shift.
        else if (std::has_single_bit(rhs->value())) {
            auto shift_amount = new (*mod) IntegerConstant(d->type(), u64(std::countr_zero(rhs->value())));
            Replace<ShiftInst>(i, d->lhs(), shift_amount, d->location());
        }
    }

public:
    void run(Inst* i) {
        switch (i->kind()) {
            default: return;
            case Value::Kind::Alloca: {
                auto alloca = as<AllocaInst>(i);

                /// If all of our uses are stores, then this alloca is dead.
                for (auto u : alloca->users())
                    if (not is<StoreInst>(u))
                        return;

                /// Delete the alloca and stores.
                alloca->erase_cascade();
                SetChanged();
            } break;

            case Value::Kind::SDiv:
                DivImpl<SDivInst, SarInst, [](auto l, auto r) { return i64(l) / i64(r); }>(i);
                break;

            case Value::Kind::UDiv:
                DivImpl<UDivInst, ShrInst, [](auto l, auto r) { return u64(l) / u64(r); }>(i);
                break;

            case Value::Kind::Add: {
                auto lhs = cast<IntegerConstant>(as<AddInst>(i)->lhs());
                auto rhs = cast<IntegerConstant>(as<AddInst>(i)->rhs());

                /// Evaluate if possible.
                if (lhs and rhs) {
                    auto result = new (*mod) IntegerConstant(i->type(), lhs->value() + rhs->value());
                    Replace(i, result);
                }

                /// If either operand is zero, replace the instruction w/ the other one.
                else if (lhs and lhs->value() == 0)
                    Replace(i, as<AddInst>(i)->rhs());
                else if (rhs and rhs->value() == 0)
                    Replace(i, as<AddInst>(i)->lhs());
            } break;
        }
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
            StoreFowardingPass,
            InstCombinePass
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

                        /// Run the pass on the instruction.
                        p.run(f->blocks()[bi]->instructions()[ii]);
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
