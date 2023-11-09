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
    /// Helper to create an integer constant.
    auto MakeInt(aint value) -> IntegerConstant* {
        return new (*mod) IntegerConstant(IntegerType::Get(mod->context(), value.bits()), value);
    }

    /// Create a new instruction, replace another instruction with
    /// it, and mark that a change has occurred.
    template <typename Instruction, typename... Args>
    auto Replace(Inst* what, Args&&... args) -> Instruction* {
        auto i = new (*mod) Instruction(std::forward<Args>(args)...);
        what->replace_with(i);
        SetChanged();
        return i;
    }

    /// Create a new instruction, and insert it after another instruction.
    template <typename Instruction, typename... Args>
    auto Create(Inst* after, Args&&... args) -> Instruction* {
        LCC_ASSERT(after->block(), "Cannot insert after floating instruction");
        auto i = new (*mod) Instruction(std::forward<Args>(args)...);
        after->block()->insert_after(i, after);
        SetChanged();
        return i;
    }

    /// Replace an instruction with a value.
    auto Replace(Inst* i, Value* v) {
        i->replace_with(v);
        SetChanged();
    }

    /// Replace an instruction with a an integer constant.
    auto Replace(Inst* i, aint value) {
        i->replace_with(MakeInt(value));
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
struct InstructionRewritePass : OptimisationPass {
    using IsInstRewritePass = std::true_type;
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
            aint lhs;
            aint rhs;
        };

        if (is<IC>(b->lhs()) and is<IC>(b->rhs())) return Result{
            true,
            cast<IC>(b->lhs())->value(),
            cast<IC>(b->rhs())->value(),
        };

        return Result{false, {}, {}};
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
            Replace(i, Eval(lhs->value(), rhs->value()));
        }

        /// Division by a power of two is a right shift.
        else if (rhs->value().is_power_of_two()) {
            Replace<ShiftInst>(i, d->lhs(), MakeInt(rhs->value().log2()), d->location());
        }
    }

    /// Handle eq, ne, lt, le, gt, ge.
    template <bool (aint::*Eval)(aint) const>
    void CmpImpl(Inst* i) {
        auto b = as<BinaryInst>(i);

        /// A comparison against itself is false for lt, gt, ne and true for le, ge, eq.
        if (b->lhs() == b->rhs()) {
            Replace(i, is<EqInst, SLeInst, ULeInst, SGeInst, UGeInst>(i));
            return;
        }

        /// Try to fold the value.
        auto [ok, lhs, rhs] = GetIntegerPair(b);
        if (not ok) return;
        Replace(i, std::invoke(Eval, lhs, rhs));
    }

    /// Handle trunc, sext, zext.
    template <auto Eval>
    void TruncExtImpl(Inst* i) {
        auto e = as<UnaryInstBase>(i);
        auto op = cast<IntegerConstant>(e->operand());
        if (op) Replace(i, std::invoke(Eval, op->value(), u8(cast<IntegerType>(e->type())->bitwidth())));
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

            case Value::Kind::CondBranch: {
                auto br = cast<CondBranchInst>(i);
                auto cond = cast<IntegerConstant>(br->cond());

                /// Collapse if the condition is known at compile time.
                if (cond) Replace<BranchInst>(i, cond->value() == 1 ? br->then_block() : br->else_block());

                /// Or if the then and else blocks are the same.
                else if (br->then_block() == br->else_block()) Replace<BranchInst>(i, br->then_block());
            } break;

            case Value::Kind::Add: {
                auto add = as<AddInst>(i);
                auto lhs = cast<IntegerConstant>(add->lhs());
                auto rhs = cast<IntegerConstant>(add->rhs());

                /// Evaluate if possible.
                if (lhs and rhs) {
                    Replace(i, lhs->value() + rhs->value());
                }

                /// Fold if possible. Otherwise, try to see if our rhs is
                /// another add whose lhs is a constant and fold with it.
                else if (lhs) {
                    if (lhs->value() == 0) Replace(i, add->rhs());
                    else if (auto radd = cast<AddInst>(add->rhs()); radd and is<IntegerConstant>(radd->lhs())) {
                        auto rlhs = cast<IntegerConstant>(radd->lhs());
                        add->lhs(MakeInt(lhs->value() + rlhs->value()));
                        add->rhs(radd->rhs());
                        SetChanged();
                    }
                }

                /// If the rhs is a constant, fold it or move it to the left.
                else if (rhs) {
                    if (rhs->value() == 0) Replace(i, add->lhs());
                    else {
                        add->swap_operands();
                        SetChanged();
                    }
                }
            } break;

            case Value::Kind::Sub: {
                auto sub = as<SubInst>(i);
                auto lhs = cast<IntegerConstant>(sub->lhs());
                auto rhs = cast<IntegerConstant>(sub->rhs());

                /// If the operands are the same, the result is 0.
                if (lhs == rhs) {
                    Replace<IntegerConstant>(i, i->type(), 0);
                }

                /// Evaluate if possible.
                else if (lhs and rhs) {
                    Replace(i, lhs->value() - rhs->value());
                }

                /// If the RHS is a constant, fold it or move it to the
                /// left; the latter case involves rewriting `a - b` to
                /// `-b + a`.
                else if (rhs) {
                    if (rhs->value() == 0) Replace(i, sub->lhs());
                    else Replace<AddInst>(i, MakeInt(-rhs->value()), sub->lhs());
                }
            } break;

            case Value::Kind::Mul: {
                auto mul = cast<MulInst>(i);
                auto lhs = cast<IntegerConstant>(mul->lhs());
                auto rhs = cast<IntegerConstant>(mul->rhs());

                /// Evaluate if possible.
                if (lhs and rhs) {
                    Replace(i, lhs->value() * rhs->value());
                }

                /// Fold if possible. Otherwise, try to see if our rhs is
                /// another mul whose lhs is a constant and fold with it.
                else if (lhs) {
                    if (lhs->value() == 1) Replace(i, mul->rhs());
                    else if (auto rmul = cast<MulInst>(mul->rhs()); rmul and is<IntegerConstant>(rmul->lhs())) {
                        auto rlhs = cast<IntegerConstant>(rmul->lhs());
                        mul->lhs(MakeInt(lhs->value() * rlhs->value()));
                        mul->rhs(rmul->rhs());
                        SetChanged();
                    }
                }

                /// If the rhs is a constant, fold it or move it to the left.
                else if (rhs) {
                    if (rhs->value() == 1) Replace(i, mul->lhs());
                    else {
                        mul->swap_operands();
                        SetChanged();
                    }
                }
            } break;

            /// GEP w/ 0 is a no-op.
            case Value::Kind::GetElementPtr:
            case Value::Kind::GetMemberPtr: {
                auto gep = as<GEPBaseInst>(i);
                auto index = cast<IntegerConstant>(gep->idx());
                if (index and index->value() == 0) Replace(i, gep->ptr());
            } break;

            case Value::Kind::SDiv:
                DivImpl<SDivInst, SarInst, [](auto l, auto r) { return l.sdiv(r); }>(i);
                break;

            case Value::Kind::UDiv:
                DivImpl<UDivInst, ShrInst, [](auto l, auto r) { return l.udiv(r); }>(i);
                break;

            case Value::Kind::Eq: CmpImpl<&aint::operator== >(i); break;
            case Value::Kind::Ne: CmpImpl<&aint::operator!= >(i); break;
            case Value::Kind::SLt: CmpImpl<&aint::slt>(i); break;
            case Value::Kind::ULt: CmpImpl<&aint::ult>(i); break;
            case Value::Kind::SGt: CmpImpl<&aint::sgt>(i); break;
            case Value::Kind::UGt: CmpImpl<&aint::ugt>(i); break;
            case Value::Kind::SLe: CmpImpl<&aint::sle>(i); break;
            case Value::Kind::ULe: CmpImpl<&aint::ule>(i); break;
            case Value::Kind::SGe: CmpImpl<&aint::sge>(i); break;
            case Value::Kind::UGe: CmpImpl<&aint::uge>(i); break;

            case Value::Kind::Trunc: TruncExtImpl<&aint::trunc>(i); break;
            case Value::Kind::SExt: TruncExtImpl<&aint::sext>(i); break;
            case Value::Kind::ZExt: TruncExtImpl<&aint::zext>(i); break;
        }
    }
};

/// Scalar replacement of aggregates.
///
/// Split allocas of aggregate types (structs and arrays)
/// into multiple variables if possible so we can optimise
/// each one in isolation.
struct SROAPass : InstructionRewritePass {
private:
    void TrySplitAlloca(AllocaInst* a) {
        /// Skip if this is not a struct or array type.
        if (not is<StructType, ArrayType>(a->allocated_type())) return;

        /// Variable is unused, just delete it.
        if (a->users().empty()) {
            a->erase_cascade();
            SetChanged();
            return;
        }

        /// Any instruction that is not a gep causes this alloca
        /// to escape, in which case we can’t split it.
        ///
        /// If the entire aggregate is ever loaded from or stored
        /// to at once, we can’t split it either, so even stores
        /// and loads make this optimisation impossible here.
        ///
        /// (Note that we could do more if we treat converting
        /// pointers to struct members back to a ptr to the struct
        /// itself as UB, but we don’t enforce that atm).
        for (auto u : a->users()) {
            if (not is<GEPBaseInst>(u)) return;

            /// Furthermore, if an array is accessed dynamically,
            /// then we can’t split it either.
            auto gep = cast<GEPInst>(u);
            if (not gep) continue;
            if (not is<IntegerConstant>(gep->idx())) return;
        }

        if (is<StructType>(a->allocated_type())) SplitStruct(a);
        else SplitArray(a);
    }

    /// Unlike structs, arrays are split on-demand since we don’t
    /// want to e.g. split an array of 1000 elements into 1000
    /// allocas.
    void SplitArray(AllocaInst* a) {
        /// Split elements as needed.
        auto elem_type = cast<ArrayType>(a->allocated_type())->element_type();
        std::unordered_map<u64, AllocaInst*> insts{};

        /// Replace GEPs.
        while (not a->users().empty()) {
            auto gep = as<GEPInst>(a->users().front());
            auto idx = as<IntegerConstant>(gep->idx())->value();

            /// If the index is negative, then this is an out-of-bounds
            /// access. Replace it with a poison value.
            if (idx.is_negative()) {
                Replace<PoisonValue>(gep, gep->type());
                continue;
            }

            /// Create the alloca for this index, if there isn’t already one.
            AllocaInst* elem;
            if (auto it = insts.find(*idx); it != insts.end()) elem = it->second;
            else {
                elem = Create<AllocaInst>(a, elem_type);
                insts[*idx] = elem;
            }

            /// Replace the gep with it.
            gep->replace_with(elem);
        }

        /// Finally, delete the original alloca.
        a->erase_cascade();
    }

    /// Split a struct into multiple variables; since structs
    /// generally don’t contain that many members, we can do
    /// this eagerly.
    void SplitStruct(AllocaInst* a) {
        auto stype = as<StructType>(a->allocated_type());
        std::vector<AllocaInst*> insts{stype->member_count()};
        for (auto&& [i, f] : vws::enumerate(stype->members())) {
            insts[usz(i)] = Create<AllocaInst>(
                i == 0 ? a : insts[usz(i) - 1],
                f,
                a->location()
            );
        }

        /// Replace GEPs.
        while (not a->users().empty()) {
            auto gep = as<GetMemberPtrInst>(a->users().front());

            /// Convert out-of-bounds and non-constant GEPs to poison values.
            auto idx = cast<IntegerConstant>(gep->idx());
            if (not idx or idx->value().uge(insts.size())) {
                Replace<PoisonValue>(gep, gep->type());
                continue;
            }

            /// Replace inbounds GEPs with the corresponding alloca.
            gep->replace_with(insts[*idx->value()]);
        }

        /// Finally, delete the original alloca.
        a->erase_cascade();
        SetChanged();
    }

public:
    void run(Inst* i) {
        auto a = cast<AllocaInst>(i);
        if (a) TrySplitAlloca(a);
    }
};

/// Pass that performs simple store forwarding.
struct StoreFowardingPass : InstructionRewritePass {
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

/// Eliminated instructions whose results are unused if they have no side-effects.
struct DCEPass : InstructionRewritePass {
    void run(Inst* i) {
        if (not i->users().empty()) return;
        switch (i->kind()) {
            default: return;
            case Value::Kind::GetElementPtr:
            case Value::Kind::Load:
            case Value::Kind::Phi:
            case Value::Kind::ZExt:
            case Value::Kind::SExt:
            case Value::Kind::Trunc:
            case Value::Kind::Bitcast:
            case Value::Kind::Neg:
            case Value::Kind::Copy:
            case Value::Kind::Compl:
            case Value::Kind::Add:
            case Value::Kind::Sub:
            case Value::Kind::Mul:
            case Value::Kind::SDiv:
            case Value::Kind::UDiv:
            case Value::Kind::SRem:
            case Value::Kind::URem:
            case Value::Kind::Shl:
            case Value::Kind::Sar:
            case Value::Kind::Shr:
            case Value::Kind::And:
            case Value::Kind::Or:
            case Value::Kind::Xor:
            case Value::Kind::Eq:
            case Value::Kind::Ne:
            case Value::Kind::SLt:
            case Value::Kind::SLe:
            case Value::Kind::SGt:
            case Value::Kind::SGe:
            case Value::Kind::ULt:
            case Value::Kind::ULe:
            case Value::Kind::UGt:
            case Value::Kind::UGe:
                i->erase();
                return;
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
            SROAPass,
            StoreFowardingPass,
            InstCombinePass,
            DCEPass
        >();
    } // clang-format on

private:
    template <typename... Passes>
    void RunPasses() {
        for (auto f : mod->code()) {
            auto RunPass = [&]<typename Pass> {
                Pass p{{mod}};

                /// Use indices here to avoid iterator invalidation.
                for (usz bi = 0; bi < f->blocks().size(); bi++) {
                    /// Call enter() callback if there is one.
                    if constexpr (requires { p.enter(f->blocks()[bi]); }) p.enter(f->blocks()[bi]);

                    for (usz ii = 0; ii < f->blocks()[bi]->instructions().size(); ii++) {
                        auto Done = [&] {
                            return bi >= f->blocks().size() or
                                   ii >= f->blocks()[bi]->instructions().size();
                        };

                        /// Some passes may end up deleting all remaining instructions,
                        /// so make sure to check that we still have instructions left
                        /// after each pass.
                        if (Done()) return p.changed();

                        /// Run the pass on the instruction.
                        Inst* inst;
                        do {
                            inst = f->blocks()[bi]->instructions()[ii];
                            p.run(inst);
                        } while (inst != f->blocks()[bi]->instructions()[ii]);
                    }

                    /// Call leave() callback if there is one.
                    if constexpr (requires { p.leave(f->blocks()[bi]); }) p.leave(f->blocks()[bi]);

                    /// Call atfork() callback if there is one and we’re at a fork.
                    if constexpr (requires { p.atfork(f->blocks()[bi]); }) {
                        auto b = f->blocks()[bi];
                        if (b->terminator() and is<CondBranchInst>(b->terminator()))
                            p.atfork(b);
                    }
                }

                /// Call done() callback if there is one.
                if constexpr (requires { p.done(); }) p.done();
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
