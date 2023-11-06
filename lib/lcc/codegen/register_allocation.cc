#include <algorithm>
#include <lcc/codegen/mir.hh>
#include <lcc/codegen/register_allocation.hh>
#include <variant>

namespace lcc {

struct AdjacencyMatrix {
    std::unique_ptr<bool[]> data;
    usz size;

    AdjacencyMatrix(usz sz) : data(std::make_unique<bool[]>(sz)), size(sz) {}

    usz coord(usz x, usz y) const {
        LCC_ASSERT(x < size, "AdjacencyMatrix: X out of bounds");
        LCC_ASSERT(y < size, "AdjacencyMatrix: Y out of bounds");
        return y * size + x;
    }

    bool at(usz x, usz y) const {
        return data[coord(x, y)];
    }

    void set(usz x, usz y) {
        data[coord(x, y)] = true;
    }

    void clear(usz x, usz y) {
        data[coord(x, y)] = false;
    }
};

static void collect_interferences_from_block(
    AdjacencyMatrix& matrix,
    std::vector<usz> live_values,
    std::vector<MBlock*> visited,
    std::vector<MBlock*> doubly_visited,
    MBlock* block
) {
    /// Don't visit the same block thrice.
    if (auto _ = std::find(visited.begin(), visited.end(), block) != visited.end()) {
        if (auto _ = std::find(doubly_visited.begin(), doubly_visited.end(), block) != doubly_visited.end())
            return;
        doubly_visited.push_back(block);
    } else visited.push_back(block);

    // Basically, walk over the instructions of the block backwards, keeping
    // track of all virtual registers that have been encountered but not
    // their defining use, as these are our "live values".
    for (auto& inst : block->instructions()) {
        // TODO:
        // If the defining use of a virtual register is an operand of this
        // instruction, remove it from vector of live vals.

        // TODO:
        /// Collect all register operands from this instruction that are used as
        /// operands somewhere in the function (i.e. within the list of registers).

        /// TODO: Make all register operands interfere with each other.

        /// TODO: Make all reg operands interfere with all clobbers of this instruction
    }
}

static void collect_interferences(AdjacencyMatrix& matrix, MFunction& function) {
    std::vector<MBlock*> exits{};
    for (auto& block : function.blocks())
        exits.push_back(&block);

    LCC_ASSERT(exits.size(), "Cannot walk CFG as function {} has no exit blocks", function.name());

    // From each exit block (collected above), follow control flow to the
    // root of the function (entry block), or to a block already visited.
    for (auto* exit : exits)
        collect_interferences_from_block(matrix, {}, {}, {}, exit);
}

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

    // STEP -1
    // Replace explicit return registers with the actual return register...
    for (auto& block : function.blocks()) {
        for (auto& inst : block.instructions()) {
            if (inst.reg() == desc.return_register_to_replace)
                inst.reg(desc.return_register);
            for (auto& op : inst.all_operands()) {
                if (std::holds_alternative<MOperandRegister>(op)) {
                    MOperandRegister reg = std::get<MOperandRegister>(op);
                    if (reg.value == desc.return_register_to_replace) {
                        reg.value = desc.return_register;
                        op = reg;
                    }
                }
            }
        }
    }

    // STEP ONE
    // Populate list of registers, first using hardware registers, then using virtual registers.
    std::vector<Register> registers{};
    for (auto [index, reg] : vws::enumerate(desc.registers))
        registers.push_back(Register{reg, 0});

    for (auto& block : function.blocks()) {
        for (auto& inst : block.instructions()) {
            if (std::find_if(registers.begin(), registers.end(), [&](Register& r) {
                    return r.value == inst.reg();
                }) == registers.end())
                registers.push_back(Register{inst.reg(), inst.regsize()});
        }
    }

    // Error if zero registers collected.
    LCC_ASSERT(registers.size(), "Cannot allocate registers when there are no registers to allocate");

    // STEP TWO
    // Walk control flow in reverse, build adjacency matrix as you go.
    AdjacencyMatrix matrix{registers.size()};

    // TODO: Collect the interferences from CFG
}

} // namespace lcc
