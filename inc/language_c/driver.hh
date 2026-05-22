#ifndef LCC_LANGUAGE_C_DRIVER_HH
#define LCC_LANGUAGE_C_DRIVER_HH

#include <lcc/context.hh>
#include <lcc/file.hh>

namespace lcc::language_c {
auto produce_module(Context* context, File& source) -> lcc::Module*;
} // namespace lcc::language_c

#endif /* LCC_LANGUAGE_C_DRIVER_HH */
