#ifndef LCC_REGISTER_ALLOCATION_HH
#define LCC_REGISTER_ALLOCATION_HH

#include <lcc/forward.hh>
#include <lcc/utils.hh>
#include <lcc/utils/result.hh>

#include <unordered_map>
#include <vector>

namespace lcc {

struct MachineDescription {
    using RegisterCategory = usz;
    using RegisterList = std::vector<usz>;
    using RegistersPerCategory = std::unordered_map<RegisterCategory, RegisterList>;

    RegistersPerCategory return_registers{};

    // Replace occurences of "return register to replace" with given "return
    // register". This means ISel doesn't have to write different patterns per
    // calling convention.
    usz return_register_to_replace{};

    RegistersPerCategory registers{};

    // For "preserve across calls"
    RegisterList volatile_registers{};
    RegisterList preserve_volatile_opcodes{};
};

auto allocate_registers(
    const MachineDescription& desc,
    MFunction& function,
    usz& next_unique_register
) -> Result<void>;

} // namespace lcc

#endif /* LCC_REGISTER_ALLOCATION_HH */
