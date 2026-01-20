#ifndef LCC_GLINT_DRIVER_HH
#define LCC_GLINT_DRIVER_HH

#include <lcc/context.hh>
#include <lcc/file.hh>

namespace lcc::glint {
auto produce_module(Context* context, File& source) -> lcc::Module*;
} // namespace lcc::glint

#endif /* LCC_GLINT_DRIVER_HH */
