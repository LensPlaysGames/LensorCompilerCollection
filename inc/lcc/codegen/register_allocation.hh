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
    std::vector<RegistersPerCategory> return_registers{};
    usz return_register_to_replace{};

    std::vector<RegistersPerCategory> registers{};

    // For "preserve across calls"
    std::vector<usz> volatile_registers{};
    std::vector<usz> preserve_volatile_opcodes{};
};

auto allocate_registers(
    const MachineDescription& desc,
    MFunction& function
) -> Result<void>;

} // namespace lcc

#endif /* LCC_REGISTER_ALLOCATION_HH */
