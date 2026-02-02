#ifndef LCC_CALLING_CONVENTION_HH
#define LCC_CALLING_CONVENTION_HH

#include <lcc/codegen/register_allocation.hh>

namespace lcc::cconv {

auto machine_description(Context*) -> MachineDescription;

} // namespace lcc::cconv

#endif /* LCC_CALLING_CONVENTION_HH */
