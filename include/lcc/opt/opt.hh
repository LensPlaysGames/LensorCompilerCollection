#ifndef LCC_OPT_HH
#define LCC_OPT_HH

#include <lcc/ir/module.hh>

namespace lcc::opt {
/// Run the optimisation pipeline on the given module.
void Optimise(Module* module, int opt_level);
}

#endif // LCC_OPT_HH
