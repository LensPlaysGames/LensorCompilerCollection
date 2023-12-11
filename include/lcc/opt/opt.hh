#ifndef LCC_OPT_HH
#define LCC_OPT_HH

#include <lcc/ir/module.hh>

namespace lcc::opt {
/// Run the optimisation pipeline on the given module.
void Optimise(Module* module, int opt_level);

/// Run select optimisation passes on the module.
void RunPasses(Module* module, std::string_view passes);
}

#endif // LCC_OPT_HH
