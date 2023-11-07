#include <lcc/opt/opt.hh>

namespace lcc::opt {
namespace {
/// Optimisation pass that runs on an instruction kind.
template <typename Instruction>
struct InstructionRewritePass {
    using IsInstRewritePass = std::true_type;
    Module* const mod;

    /// Check if this an instruction should be handled by this pass.
    [[nodiscard]] static bool matches(Inst* inst) {
        return Instruction::classof(inst);
    }
};

/// Pass that deleted unused locals.
struct DeleteUnusedAllocasPass : InstructionRewritePass<AllocaInst> {
    bool run(Inst* i) {
        auto alloca = as<AllocaInst>(i);

        /// If all of our uses are stores, then this alloca is dead.
        for (auto u : alloca->users())
            if (not is<StoreInst>(u))
                return false;

        /// Delete the alloca and stores.
        alloca->erase_cascade();
        return true;
    }
};

struct Optimiser {
    Module* const mod;

    /// TODO: Actually use this.
    [[maybe_unused]] int const opt_level;

    /// Entry point.
    void run() {
        RunPasses<DeleteUnusedAllocasPass>();
    }

private:
    template <typename... Passes>
    void RunPasses() {
        RunPassesImpl<Passes...>(std::index_sequence_for<Passes...>{});
    }

    template <typename... Passes, usz... Is>
    void RunPassesImpl(std::index_sequence<Is...>) {
        std::tuple<Passes...> passes{Passes{mod}...};
        for (auto f : mod->code()) {
            /// Use indices here to avoid iterator invalidation.
            for (usz bi = 0; bi < f->blocks().size(); bi++) {
                for (usz ii = 0; ii < f->blocks()[bi]->instructions().size(); ii++) {
                    auto RunPass = [&]<typename Pass, usz Idx> {
                        /// Some passes may end up deleting all remaining instructions,
                        /// so make sure to check that we still have instructions left
                        /// after each pass.
                        if (
                            bi >= f->blocks().size() or
                            ii >= f->blocks()[bi]->instructions().size()
                        ) return false;

                        /// Instruction rewrite pass.
                        if constexpr (requires { typename Pass::IsInstRewritePass; }) {
                            auto* inst = f->blocks()[bi]->instructions()[ii];
                            auto& p = std::get<Idx>(passes);
                            if (Pass::matches(inst)) return p.run(inst);
                            return false;
                        }

                        /// Unknown pass.
                        else {
                            static_assert(always_false<Pass>, "Invalid pass");
                        }
                    };

                    /// Run all passes on the current instruction so long
                    /// as at least one of them returns true.
                    while ((RunPass.template operator()<Passes, Is>() or ...)) {}
                }
            }
        }
    }
};

} // namespace
} // namespace lcc::opt

void lcc::opt::Optimise(Module* module, int opt_level) {
    Optimiser o{module, opt_level};
    o.run();
}
