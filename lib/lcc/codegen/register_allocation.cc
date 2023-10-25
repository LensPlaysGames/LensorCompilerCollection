#include <algorithm>
#include <lcc/codegen/register_allocation.hh>

#include <lcc/codegen/mir.hh>

namespace lcc {

void allocate_registers(const MachineDescription& desc, MFunction& function) {
    // Steps:
    //   1. Collect all existing registers, both hardware and virtual.
    //   2. Walk control flow in reverse, build adjacency matrix as you go.
    //   3. Build adjacency lists from adjacency matrix.
    //   4. Figure out order that registers should be allocated in: call this
    //      list the "coloring stack".
    //   5. Assign colors to registers, ensuring no overlap (adjacencies), in
    //      order of the coloring stack.
    //     5a. TODO If we can't color with the existing stack, spill a register
    //         and retry.
    //   6. Map colors to registers, updating all register operands to the
    //      allocated register.


    // STEP ONE
    // Populate list of registers, first using hardware registers, then using virtual registers.
    std::vector<Register> registers{};
    for (auto [index, reg] : vws::enumerate(desc.registers)) {
        registers.push_back(Register{reg, 0});
    }
    for (auto& block : function.blocks()) {
        for (auto& inst : block.instructions()) {
            if (std::find_if(registers.begin(), registers.end(),
                             [&](Register& r) {
                                 return r.value == inst.reg();
                             }) == registers.end()) {
                registers.push_back(Register{ inst.reg(), inst.regsize() });
            }
        }
    }
}

} // namespace lcc
