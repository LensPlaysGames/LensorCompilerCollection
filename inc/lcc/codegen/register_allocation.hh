#ifndef LCC_REGISTER_ALLOCATION_HH
#define LCC_REGISTER_ALLOCATION_HH

#include <lcc/forward.hh>
#include <lcc/utils.hh>
#include <lcc/utils/result.hh>

#include <vector>

namespace lcc {

struct MachineDescription {
    usz return_register;
    usz return_register_to_replace;
    std::vector<usz> registers;
};

auto allocate_registers(
    const MachineDescription& desc,
    MFunction& function
) -> Result<void>;

} // namespace lcc

#endif /* LCC_REGISTER_ALLOCATION_HH */
