#ifndef LCC_LANGUAGE_C_DRIVER_HH
#define LCC_LANGUAGE_C_DRIVER_HH

#include <lccbase/context.hh>
#include <lccbase/file.hh>

namespace lcc::language_c {
auto produce_module(Context* context, File& source) -> lcc::Module*;
} // namespace lcc::language_c

#endif /* LCC_LANGUAGE_C_DRIVER_HH */
