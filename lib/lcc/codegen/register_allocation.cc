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
    std::vector<Register>& registers,
    MFunction& function,
    std::vector<usz> live_values,
    std::vector<MBlock*> visited,
    std::vector<MBlock*> doubly_visited,
    MBlock* block
) {
    /// Don't visit the same block thrice.
    if (std::find(visited.begin(), visited.end(), block) != visited.end()) {
        if (std::find(doubly_visited.begin(), doubly_visited.end(), block) != doubly_visited.end())
            return;
        doubly_visited.push_back(block);
    } else visited.push_back(block);

    const auto live_idx_from_register_value = [&](usz value) -> usz {
        auto found = std::find_if(registers.begin(), registers.end(), [&](const Register& r) {
            return value == r.value;
        });
        LCC_ASSERT(found != registers.end(), "Did not find referenced register in register list");
        return usz(found - registers.begin());
    };
    const auto live_idx_from_register = [&](Register reg) -> usz {
        return live_idx_from_register_value(reg.value);
    };

    // Basically, walk over the instructions of the block backwards, keeping
    // track of all virtual registers that have been encountered but not
    // their defining use, as these are our "live values".
    for (auto& inst : block->instructions()) {
        // If the defining use of a virtual register is an operand of this
        // instruction, remove it from vector of live vals.
        if (inst.is_defining()) std::erase(live_values, inst.reg());
        for (auto& op : inst.all_operands()) {
            if (std::holds_alternative<MOperandRegister>(op)) {
                auto reg = std::get<MOperandRegister>(op);
                if (reg.defining_use) std::erase(live_values, reg.value);
            }
        }

        // Collect all register operands from this instruction that are
        // used as operands somewhere in the function (i.e. within the list of
        // registers). Cache the index within the adjacency matrix so we don't
        // have to keep recomputing it.
        typedef struct RegisterPlusLiveValIndex {
            Register reg;
            usz idx;
        } MIROperandPlusLiveValIndex;
        std::vector<MIROperandPlusLiveValIndex> vreg_operands{};
        for (auto& op : inst.all_operands()) {
            if (std::holds_alternative<MOperandRegister>(op)) {
                auto reg = std::get<MOperandRegister>(op);
                if (reg.value >= +MInst::Kind::ArchStart)
                    vreg_operands.push_back({reg, live_idx_from_register(reg)});
            }
        }

        // Make all register operands interfere with each other.
        for (auto& A : vreg_operands) {
            for (auto& B : vreg_operands) {
                matrix.set(A.idx, B.idx);
            }
        }

        // Make all reg operands interfere with the register of this function, if
        // it is a defining use.
        if (inst.is_defining()) {
            for (auto& r : vreg_operands) {
                matrix.set(r.idx, live_idx_from_register_value(inst.reg()));
            }
        }

        // TODO: Make all reg operands interfere with all clobbers of this instruction

        // Make all reg operands interfere with all currently live values
        for (auto& r : vreg_operands) {
            for (auto& live : live_values)
                matrix.set(r.idx, live_idx_from_register_value(live));
        }

        // If a virtual register is not live and is seen as an operand, it is
        // added to the vector of live values.
        for (auto& r : vreg_operands) {
            if (not r.reg.defining_use and std::find(live_values.begin(), live_values.end(), r.reg.value) == live_values.end())
                live_values.push_back(r.reg.value);
        }

    } // for inst

    // Walk CFG backwards (follow predecessors)

    // No predecessors == entry block: done walking.
    if (block->predecessors().empty()) return;

    // Follow all predecessors, resetting live values to what they are now
    // before each one.
    for (auto parent_name : block->predecessors()) {
        auto* parent = function.block_by_name(parent_name);
        auto live_values_copy{live_values};
        collect_interferences_from_block(matrix, registers, function, live_values_copy, visited, doubly_visited, parent);
    }
}

static void collect_interferences(AdjacencyMatrix& matrix, std::vector<Register>& registers, MFunction& function) {
    std::vector<MBlock*> exits{};
    for (auto& block : function.blocks()) {
        if (block.successors().empty())
            exits.push_back(&block);
    }

    LCC_ASSERT(exits.size(), "Cannot walk CFG as function {} has no exit blocks", function.name());

    //fmt::print(
    //    "Collected following exit blocks for function {}: {}\n",
    //    function.name(),
    //    fmt::join(vws::transform(exits, [](MBlock* b) { return b->name(); }), ", ")
    //);

    // From each exit block (collected above), follow control flow to the
    // root of the function (entry block), or to a block already visited.
    for (auto* exit : exits)
        collect_interferences_from_block(matrix, registers, function, {}, {}, {}, exit);
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
    // Helper function that handles not adding duplicates.
    auto add_reg = [&](usz id, usz size) {
        if (std::find_if(registers.begin(), registers.end(), [&](Register& r) {
                return r.value == id;
            }) == registers.end()) {
            registers.push_back(Register{id, uint(size)});
        }
    };
    for (auto [index, reg] : vws::enumerate(desc.registers))
        add_reg(reg, 0);

    for (auto& block : function.blocks()) {
        for (auto& inst : block.instructions()) {
            add_reg(inst.reg(), inst.regsize());
            for (auto& op : inst.all_operands()) {
                if (std::holds_alternative<MOperandRegister>(op)) {
                    MOperandRegister reg = std::get<MOperandRegister>(op);
                    add_reg(reg.value, reg.size);
                }
            }
        }
    }

    // Error if zero registers collected.
    LCC_ASSERT(registers.size(), "Cannot allocate registers when there are no registers to allocate");

    // STEP TWO
    // Walk control flow in reverse, build adjacency matrix as you go.
    AdjacencyMatrix matrix{registers.size()};

    // Collect the interferences by walking CFG in reverse.
    collect_interferences(matrix, registers, function);
}

} // namespace lcc
