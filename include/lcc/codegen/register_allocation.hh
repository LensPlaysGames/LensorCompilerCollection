#ifndef LCC_REGISTER_ALLOCATION_HH
#define LCC_REGISTER_ALLOCATION_HH

#include <lcc/forward.hh>

#include <vector>

namespace lcc {

struct MachineDescription {
    usz return_register;
    usz return_register_to_replace;
    std::vector<usz> registers;
};

void allocate_registers(const MachineDescription& desc, MFunction& function);

}

#endif /* LCC_REGISTER_ALLOCATION_HH */
