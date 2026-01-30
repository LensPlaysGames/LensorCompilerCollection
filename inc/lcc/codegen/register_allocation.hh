#ifndef LCC_REGISTER_ALLOCATION_HH
#define LCC_REGISTER_ALLOCATION_HH

#include <lcc/forward.hh>
#include <lcc/utils.hh>
#include <lcc/utils/result.hh>

#include <vector>

namespace lcc {

struct MachineDescription {
    struct RegistersPerCategory {
        usz category{};
        std::vector<usz> registers{};
    };

    // Replace occurences of "return register to replace" with given "return
    // register". This means ISel doesn't have to write different patterns per
    // calling convention.
    usz return_register{};
    usz return_register_to_replace{};

    // General Purpose Registers
    std::vector<RegistersPerCategory> registers{};
};

auto allocate_registers(
    const MachineDescription& desc,
    MFunction& function
) -> Result<void>;

} // namespace lcc

#endif /* LCC_REGISTER_ALLOCATION_HH */
