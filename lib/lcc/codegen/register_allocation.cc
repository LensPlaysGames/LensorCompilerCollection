#include <lcc/codegen/mir.hh>
#include <lcc/codegen/register_allocation.hh>
#include <lcc/codegen/x86_64/x86_64.hh>
#include <lcc/utils.hh>

#include <fmt/format.h>
#include <fmt/ranges.h>

#include <algorithm>
#include <memory>
#include <ranges>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <vector>

namespace lcc {

struct AdjacencyMatrix {
    std::unique_ptr<bool[]> data;
    usz size;

    explicit AdjacencyMatrix(usz sz)
        : data(std::make_unique<bool[]>(sz * sz)),
          size(sz) {}

    [[nodiscard]]
    auto coord(usz x, usz y) const -> usz {
        LCC_ASSERT(x < size, "AdjacencyMatrix: X out of bounds");
        LCC_ASSERT(y < size, "AdjacencyMatrix: Y out of bounds");
        LCC_ASSERT(
            x != y,
            "AdjacencyMatrix: X and Y are equal; must not set adjacency with self"
        );
        return y * size + x;
    }

    [[nodiscard]]
    auto at(usz x, usz y) const -> bool {
        return data[coord(x, y)];
    }

    void set(usz x, usz y) {
        if (x == y) return;
        data[coord(x, y)] = true;
    }

    void clear(usz x, usz y) {
        if (x == y) return;
        data[coord(x, y)] = false;
    }
};

struct AdjacencyList {
    // List of live indices that interfere with this->value.
    std::vector<usz> adjacencies;

    [[nodiscard]]
    auto degree() const { return adjacencies.size(); }

    // TODO: Originating instruction/operand?

    // Value/Id of virtual register this list is for.
    usz value;

    // Live index of this list.
    usz index;

    usz category;

    // If a register value appears in this set, this list is said to interfere
    // with that register value.
    std::unordered_set<usz> regmask{};

    // Value/Id of register that this list has been colored with.
    usz color;

    // Whether or not this list has been allocated a register (color has been
    // set).
    bool allocated;

    // Spill handling.
    char spill_flag;
    usz spill_offset;
    usz spill_cost;

    [[nodiscard]]
    auto string_base() const -> std::string {
        return fmt::format("r{}", value, index);
    }

    [[nodiscard]]
    auto string(const std::vector<AdjacencyList>& lists) const -> std::string {
        auto out = string_base() + ": ";
        bool first{true};
        for (usz adj_i : adjacencies) {
            if (not first) out += ", ";
            else first = false;
            auto found = rgs::find_if(
                lists,
                [&](const AdjacencyList& l) {
                    return adj_i == l.value;
                }
            );
            out += lists.at(usz(found - lists.begin())).string_base();
        }
        return out;
    }
};

namespace {

usz live_idx_from_register_value(const std::vector<Register>& registers, usz value) {
    auto found = rgs::find_if(
        registers,
        [&](const Register& r) {
            return value == r.value;
        }
    );
    LCC_ASSERT(
        found != registers.end(),
        "Did not find referenced register in register list"
    );
    return usz(found - registers.begin());
}

usz live_idx_from_register(const std::vector<Register>& registers, Register r) {
    return live_idx_from_register_value(registers, r.value);
}

void remove_defining(std::vector<usz>& live_values, const MInst& inst) {
    // If the defining use of a virtual register is an operand of this
    // instruction, remove it from vector of live vals.
    if (inst.is_defining())
        std::erase(live_values, inst.reg());

    for (auto& op : inst.all_operands()) {
        if (std::holds_alternative<MOperandRegister>(op)) {
            auto reg = std::get<MOperandRegister>(op);
            if (reg.defining_use)
                std::erase(live_values, reg.value);
        }
    }
}

void matrix_set_clobbers(
    AdjacencyMatrix& matrix,
    const std::vector<Register>& registers,
    const std::vector<usz>& live_values,
    const MInst& inst
) {
    // Register Clobbers
    // Basically, a "clobbered register" has the affect that all live values
    // will interfere with the clobber.
    for (auto index : inst.operand_clobbers()) {
        auto op = inst.get_operand(index);
        if (std::holds_alternative<MOperandRegister>(op)) {
            auto reg = std::get<MOperandRegister>(op);
            auto live_idx = live_idx_from_register(registers, reg);
            for (auto live : live_values) {
                matrix.set(
                    live_idx_from_register_value(registers, live),
                    live_idx
                );
                // fmt::print("Clobber r{} interferes with live value r{}\n", reg.value, live);
            }
        }
    }

    for (auto r_id : inst.register_clobbers()) {
        auto live_idx = live_idx_from_register_value(registers, r_id);
        for (auto live : live_values) {
            matrix.set(
                live_idx_from_register_value(registers, live),
                live_idx
            );
            // fmt::print("Clobber r{} interferes with live value r{}\n", reg.value, live);
        }
    }
}

struct RegisterPlusLiveValIndex {
    Register reg;
    usz idx;
};

std::vector<RegisterPlusLiveValIndex> collect_vreg_operands(
    const std::vector<Register>& registers,
    const MInst& inst
) {
    std::vector<RegisterPlusLiveValIndex> vreg_operands{};
    if (inst.reg() >= +Module::first_virtual_register) {
        auto reg = Register{inst.reg(), uint(inst.regsize())};
        vreg_operands.push_back({reg, live_idx_from_register(registers, reg)});
    }
    for (auto& op : inst.all_operands()) {
        if (std::holds_alternative<MOperandRegister>(op)) {
            auto reg = std::get<MOperandRegister>(op);
            if (reg.value >= +Module::first_virtual_register)
                vreg_operands.push_back({reg, live_idx_from_register(registers, reg)});
        }
    }
    return vreg_operands;
}

std::vector<usz> collect_clobbered_registers(
    const MInst& inst
) {
    std::vector<usz> clobbered_regs{};

    // Operand clobbers that happen to be registers.
    for (auto index : inst.operand_clobbers()) {
        auto& op = inst.all_operands().at(index);
        if (std::holds_alternative<MOperandRegister>(op)) {
            auto reg = std::get<MOperandRegister>(op);
            clobbered_regs.push_back(reg.value);
        }
    }

    // TODO: Should we also take register clobbers into account here?

    return clobbered_regs;
}

void record_newly_live_values(
    const std::vector<RegisterPlusLiveValIndex>& vreg_operands,
    const MInst& inst,
    std::vector<usz>& live_values
) {
    // If a virtual register is not live and is seen as an operand, it is
    // added to the vector of live values.
    for (auto r : vreg_operands) {
        if (
            not r.reg.defining_use
            and rgs::find(live_values, r.reg.value) == live_values.end()
        ) live_values.push_back(r.reg.value);
    }
    // Handle the case of a non-defining register operand in use of the
    // instruction that defines that register.
    if (inst.is_defining())
        std::erase(live_values, inst.reg());
}

void collect_interferences_from_instruction(
    AdjacencyMatrix& matrix,
    const std::vector<Register>& registers,
    std::vector<usz>& live_values,
    std::vector<MBlock*>& visited,
    std::vector<MBlock*>& doubly_visited,
    MInst& inst
) {
    // fmt::print("{}\n", PrintMInstImpl(inst, x86_64::opcode_to_string));
    // fmt::print("live after: {}\n", fmt::join(live_values, ", "));

    // If the defining use of a virtual register is an operand of this
    // instruction, remove it from live values.
    remove_defining(live_values, inst);

    // fmt::print("live during: {}\n", fmt::join(live_values, ", "));

    // Make registers clobbered by this instruction's execution interfere with
    // all live values; this is necessary because, if any value we later
    // needed were in these registers, execution of this instruction would
    // invalidate that value.
    matrix_set_clobbers(matrix, registers, live_values, inst);

    // Collect all register operands from this instruction that are
    // used as operands somewhere in the function (i.e. within the list of
    // registers). Cache the index within the adjacency matrix so we don't
    // have to keep recomputing it.
    auto vreg_operands = collect_vreg_operands(registers, inst);

    // Collect clobbered registers
    auto clobbered_regs = collect_clobbered_registers(inst);

    // Make all reg operands interfere with each other; if two different,
    // non-clobbered registers are used as inputs to an instruction, they must
    // exist at the same time (at instruction execution), and therefore must
    // interfere. For cases like `move %v0 into %v1` where v1 is marked as
    // clobbered, v0 and v1 do not interfere.
    // x86_64 GNU ASM
    //     mov 40(%v0), %v1
    // MIR Representation
    //     MoveDereferenceLHS(v0, v1, 40) clobbers 1st operand
    // RESULT
    //     Both v0 and v1 interfere with both v3 and v7.
    for (auto A : vreg_operands) {
        for (auto B : vreg_operands) {
            // If either A or B is clobbered, do NOT set interference between the two.
            if (
                rgs::find(clobbered_regs, A.reg.value) == clobbered_regs.end()
                and rgs::find(clobbered_regs, B.reg.value) == clobbered_regs.end()
            ) {
                matrix.set(A.idx, B.idx);
                // fmt::print(
                //     "Non-clobbered register operands r{} and r{} interfere\n",
                //     A.reg.value,
                //     B.reg.value
                // );
            }
        }
    }

    // Make all virtual register operands interfere with all currently live values
    // Live Values: v3, v7
    // x86_64 GNU ASM
    //     mov 40(%v0), %v1
    // MIR Representation
    //     MoveDereferenceLHS(v0, v1, 40) clobbers 1st operand
    // RESULT
    //     Both v0 and v1 interfere with both v3 and v7.
    for (auto r : vreg_operands) {
        for (auto live : live_values) {
            matrix.set(
                r.idx,
                live_idx_from_register_value(registers, live)
            );
        }
    }

    // If a virtual register is not live and is seen as an operand, it is
    // added to the vector of live values.
    record_newly_live_values(vreg_operands, inst, live_values);

    // fmt::print("live before: {}\n", fmt::join(live_values, ", "));
}

void collect_interferences_from_block(
    AdjacencyMatrix& matrix,
    const std::vector<Register>& registers,
    MFunction& function,
    std::vector<usz> live_values,
    std::vector<MBlock*> visited,
    std::vector<MBlock*> doubly_visited,
    MBlock* block
) {
    // Don't visit the same block thrice.
    // TODO: In reality, it's not that simple, but this is an approximation we
    // are currently using and it *is* working.
    // Preferably, this would better match the actual semantics; I *think*
    // that would mean limiting the amount of times we visit a block to the
    // amount of successors it has, but also at least once.
    // Further TODO: Small diagram and explanation for why we need to visit
    // blocks multiple times at all; why isn't one pass enough?
    if (rgs::find(visited, block) != visited.end()) {
        if (rgs::find(doubly_visited, block) != doubly_visited.end())
            return;
        doubly_visited.push_back(block);
    } else visited.push_back(block);

    // Basically, walk over the instructions of the block backwards, keeping
    // track of all virtual registers that have been encountered but not
    // their defining use, as these are our "live values".
    for (auto& inst : vws::reverse(block->instructions())) {
        collect_interferences_from_instruction(
            matrix,
            registers,
            live_values,
            visited,
            doubly_visited,
            inst
        );
    }

    // Walk CFG backwards (follow predecessors)

    // No predecessors == entry block: done walking.
    if (block->predecessors().empty())
        return;

    // Follow all predecessors, resetting live values to what they are now
    // before each one.
    for (const auto& parent_name : block->predecessors()) {
        auto* parent = function.block_by_name(parent_name);
        auto live_values_copy{live_values};
        collect_interferences_from_block(
            matrix,
            registers,
            function,
            std::move(live_values_copy),
            visited,
            doubly_visited,
            parent
        );
    }
}

void collect_interferences(
    AdjacencyMatrix& matrix,
    const std::vector<Register>& registers,
    MFunction& function
) {
    std::vector<MBlock*> exits{};
    for (auto& block : function.blocks()) {
        if (block.successors().empty())
            exits.push_back(&block);
    }

    LCC_ASSERT(
        not exits.empty(),
        "Cannot walk CFG as function {} has no exit blocks",
        function.names().at(0).name
    );

    // fmt::print(
    //     "Collected following exit blocks for function {}: {}\n",
    //     function.names().at(0).name,
    //     fmt::join(vws::transform(exits, [](MBlock* b) { return b->name(); }), ", ")
    // );

    // From each exit block (collected above), follow control flow to the
    // root of the function (entry block), or to a block already visited.
    for (auto* exit : exits) {
        collect_interferences_from_block(
            matrix,
            registers,
            function,
            {},
            {},
            {},
            exit
        );
    }
}

} // namespace

auto allocate_registers(
    const MachineDescription& desc,
    MFunction& function
) -> Result<void> {
    // Don't allocate registers for empty functions.
    if (function.blocks().empty())
        return {};

    // Steps:
    //   1. Collect all existing registers, both hardware and virtual.
    //   2. Walk control flow in reverse, build adjacency matrix as you go.
    //   3. Build adjacency lists from adjacency matrix.
    //   4. Figure out order that registers should be allocated in: call this
    //      list the "coloring stack".
    //   5. Assign colors to registers, ensuring no overlap (adjacencies), in
    //      order of the coloring stack.
    //     5a. If we can't color with the existing stack, spill a register
    //         and retry.
    //   6. Map colors to registers, updating all register operands to the
    //      allocated register.

    // STEP -1
    // Replace explicit return registers with the actual return register...
    //
    // EXPLAINER: For ease of writing instruction selection patterns, there is
    // a reserved register value that means "the general purpose return
    // register according to the current calling convention"; this avoids
    // having to write different instruction selection patterns for every
    // single calling convention.
    const auto return_register_by_category = [&](usz category) {
        for (auto regcategory : desc.return_registers) {
            if (regcategory.category == category)
                return regcategory.registers.at(0);
        }
        fmt::print(
            stderr,
            "Could not find return register for category {}\n",
            +category
        );
        LCC_UNREACHABLE();
    };
    for (auto& block : function.blocks()) {
        for (auto& inst : block.instructions()) {
            if (inst.reg() == desc.return_register_to_replace)
                inst.reg(return_register_by_category(inst.regcategory()));
            for (auto& op : inst.all_operands()) {
                if (std::holds_alternative<MOperandRegister>(op)) {
                    MOperandRegister reg = std::get<MOperandRegister>(op);
                    if (reg.value == desc.return_register_to_replace) {
                        reg.value = return_register_by_category(+reg.category);
                        op = reg;
                    }
                }
            }
        }
    }

    // TODO: We need the context here (or the module so we can get to the
    // context) so that we can query if target is actually x86_64.
    // fmt::print(
    //    "Return register replaced.\n{}\n",
    //    PrintMFunctionImpl(function, x86_64::opcode_to_string)
    //);

    // STEP ONE
    // Populate list of registers, first using hardware registers, then using
    // virtual registers.
    std::vector<Register> registers{};

    // Helper function that ensures no duplicate registers show up in the
    // register list.
    auto add_reg = [&](usz id, usz size, usz category) {
        auto found = rgs::find_if(
            registers,
            [&](Register& r) {
                return r.value == id;
            }
        );
        if (found == registers.end())
            registers.emplace_back(
                id,
                uint(size),
                (Register::Category) category
            );
    };

    // Hardware registers are passed in through the machine description.
    // Add all (relevant) hardware registers to the list of all registers.
    for (auto register_category : desc.registers) {
        for (auto reg : register_category.registers) {
            add_reg(reg, 0, register_category.category);
        }
    }

    // Walk the MIR, and,
    //   A: for every instruction, add the result register to the list of
    //      registers, and,
    //   B: for every register operand of each instruction, add the referenced
    //      register to the list of registers.
    for (auto& block : function.blocks()) {
        for (auto& inst : block.instructions()) {
            add_reg(inst.reg(), inst.regsize(), inst.regcategory());
            for (auto& op : inst.all_operands()) {
                if (std::holds_alternative<MOperandRegister>(op)) {
                    MOperandRegister reg = std::get<MOperandRegister>(op);
                    add_reg(reg.value, reg.size, +reg.category);
                }
            }
        }
    }

    // STEP TWO
    // Walk control flow in reverse, build adjacency matrix as you go.
    // We walk in reverse because of how control flow tends to work; a single
    // vreg may have multiple defining uses in different predecessor blocks.
    AdjacencyMatrix matrix{registers.size()};

    // Collect the interferences into the matrix by walking CFG in reverse.
    collect_interferences(matrix, registers, function);

    // STEP THREE
    // Build adjacency lists from adjacency matrix
    std::vector<AdjacencyList> lists{};

    for (auto [i, reg] : vws::enumerate(registers)) {
        AdjacencyList list{};
        // The list index is equivalent to the index where the corresponding
        // register may be found in the main list of registers.
        list.index = usz(i);
        // The list value is equivalent to the corresponding register's value.
        list.value = reg.value;
        // The list category is equivalent to the corresponding register's
        // category.
        list.category = +reg.category;
        // Hardware registers do not need allocated; they are already "colored
        // in".
        if (list.value < +Module::first_virtual_register) {
            list.color = list.value;
            list.allocated = true;
        }
        lists.emplace_back(list);
    }

    for (auto [a_idx, a] : vws::enumerate(registers)) {
        for (auto [b_idx, b] : vws::enumerate(registers)) {
            if (a_idx == b_idx) break;
            if (matrix.at(usz(a_idx), usz(b_idx))) {
                auto& a_list = lists.at(usz(a_idx));
                auto& b_list = lists.at(usz(b_idx));
                a_list.adjacencies.push_back(b.value);
                b_list.adjacencies.push_back(a.value);
            }
        }
    }

    // fmt::print("AdjacencyLists:\n");
    // for (auto list : lists)
    //     fmt::print("{}\n", list.string(lists));

    // STEP FOUR
    // Build something called the "coloring stack": this is the list of live
    // indices (index into lists vector) that determine what order we should
    // assign registers in.
    std::vector<usz> coloring_stack{};

    const auto should_skip_list = [&](AdjacencyList& list) {
        // Skip hardware registers, and registers already allocated a value.
        return list.allocated or list.value < +Module::first_virtual_register;
    };

    std::unordered_map<usz, usz> ks{};
    for (auto register_category : desc.registers) {
        ks[register_category.category] = register_category.registers.size();
    }

    // How many registers we need to color in, total.
    usz count = registers.size();
    // We don't color hardware registers with other hardware registers,
    // so we don't count them.
    for (auto [category, k] : ks) {
        LCC_ASSERT(
            count > k,
            "RA: Somehow, we didn't collect all registers, or something."
            " Lens got this wrong before by setting the calling conventions register array size to an incorrect value."
        );
        count -= k;
    }

    // Until we've colored all necessary registers...
    while (count) {
        /// degree < k rule:
        ///   A graph G is k-colorable if, for every node N in G, the degree
        ///   of N < k.
        bool done{true};
        do {
            done = true;
            for (auto [i, list] : vws::enumerate(lists)) {
                if (should_skip_list(list)) continue;
                if (list.degree() < ks.at(list.category)) {
                    list.allocated = 1;
                    done = false;
                    count--;
                    coloring_stack.push_back(usz(i));
                }
            }
        } while ((not done) and count);

        if (count) {
            /// Determine node with minimal spill cost.
            usz min_cost = std::numeric_limits<usz>::max();
            usz node_to_spill = 0;

            for (auto& list : lists) {
                if (should_skip_list(list))
                    continue;

                if (list.degree())
                    list.spill_cost = list.spill_cost / list.degree();
                else list.spill_cost = 0;

                if (list.degree() and list.spill_cost <= min_cost) {
                    min_cost = list.spill_cost;
                    node_to_spill = list.index;
                    if (not min_cost) break;
                }
            }
            /// Push onto color allocation stack.
            coloring_stack.push_back(node_to_spill);
            lists.at(node_to_spill).allocated = true;
            count--;
        }
    }

    // fmt::print("Coloring Stack: {}\n", fmt::join(coloring_stack, ", "));

    // STEP FIVE
    // Use coloring stack to assign hardware registers (colors) to virtual
    // registers, ensuring no overlap (interferences/adjacencies).
    for (usz i : coloring_stack) {
        auto& list = lists.at(i);

        // Skip hardware registers (no need to color them).
        if (list.value < +Module::first_virtual_register)
            continue;

        auto& register_interferences = list.regmask;
        for (usz i_adj : list.adjacencies) {
            auto adj_list = rgs::find_if(
                lists,
                [&](AdjacencyList& l) {
                    return l.value == i_adj;
                }
            );
            LCC_ASSERT(
                adj_list != lists.end(),
                "Could not find adjacency list corresponding to vreg {}",
                i_adj
            );
            // If any adjacency of the current list is already colored, the current
            // list must not be colored with that color.
            if (adj_list->color) {
                // fmt::print("Colored adjacency: [r{}, {}]\n", adj_list->value, adj_list->color);
                register_interferences.emplace(adj_list->color);
            }
        }

        // Attempt to find a hardware register that is NOT marked as interfering
        // with the register associated with the current adjacency list.
        usz reg_value = 0;
        auto register_category = rgs::find_if(
            desc.registers,
            [&](const MachineDescription::RegistersPerCategory& r) {
                return r.category == list.category;
            }
        );
        LCC_ASSERT(
            register_category != desc.registers.end(),
            "Could not find register list corresponding to adjacency list category {}",
            list.category
        );
        for (auto reg : (*register_category).registers) {
            if (not register_interferences.contains(reg)) {
                reg_value = reg;
                break;
            }
        }

        // STEP FIVE A
        // 5a. If we can't color with the existing stack, spill a register
        //     and retry.
        //
        // If we were not able to assign a register value, that means there are
        // zero registers in the machine description that don't interfere with the
        // current virtual register; that is, there is no hardware register for
        // this virtual register to be assigned to. As such, in this case, we are
        // not able to allocate registers for the function (in it's current
        // state). So, let's modify the function's state such that we can :^).
        if (not reg_value) {
            // There are zero hardware registers that don't interfere with the
            // current virtual register we are trying to color; we need to pick one of
            // the virtual registers that has already been allocated a hardware
            // register, go back and insert a "save to stack" instruction (spill), and
            // then insert "load from stack" (unspill) instructions at every use.

            // Get virtual register value we need to spill
            LCC_ASSERT(
                register_interferences.size(),
                "Cannot spill when no registers have been allocated"
            );
            auto spilled_hardreg = *(register_interferences.begin());

            // Spilling hardware register 'spilled_hardreg'

            // Look in the adjacency lists to map the list with the interfering color
            // back to the virtual register the list is associated with.
            usz spilled_vreg{0};
            for (auto& l : lists) {
                if (l.value != spilled_hardreg and l.color == spilled_hardreg) {
                    spilled_vreg = l.value;
                    break;
                }
            }
            LCC_ASSERT(
                spilled_vreg,
                "Unable to resolve assigned hardware register back to virtual register for spilling"
            );

            // Get virtual register size (we really should have an "origin
            // instruction" member in adjacency lists... or /something/).
            usz spilled_vreg_size{0};
            for (auto& block : function.blocks()) {
                for (auto& inst : block.instructions()) {
                    if (inst.reg() == spilled_vreg) {
                        spilled_vreg_size = inst.regsize();
                        break;
                    }
                    for (auto op : inst.all_operands()) {
                        if (std::holds_alternative<MOperandRegister>(op)) {
                            auto& r = std::get<MOperandRegister>(op);
                            if (r.value == spilled_vreg) {
                                spilled_vreg_size = r.size;
                                break;
                            }
                        }
                    }
                    if (spilled_vreg_size) break;
                }
                if (spilled_vreg_size) break;
            }
            LCC_ASSERT(
                spilled_vreg_size,
                "Unable to resolve virtual register back to it's defining use for spilling"
            );

            // We need to find instruction by virtual register value (find relevant
            // defining use).
            // Once we have this instruction, we need to insert a "save to stack"
            // instruction (spill) directly following it.
            // We need to insert the spill after the last instruction with
            // inst.reg() == spilled_vreg in the function.
            // This is because a single instruction may become multiple during
            // instruction selection.
            //     %0 = add i32 1, 1
            //     V
            //     %0 = mov $1 into %0
            //     %0 = add $1 into %0
            // We want the spill after the add, not the move.
            bool spill_after{false};
            for (auto& block : function.blocks()) {
                if (spill_after) break;
                for (auto [inst_i, inst] : vws::enumerate(block.instructions())) {
                    // for every instruction...

                    auto next_inst = block.instructions().begin() + inst_i + 1;
                    bool next_inst_spilled_vreg = next_inst < block.instructions().end()
                                              and next_inst->reg() == spilled_vreg;
                    if (next_inst_spilled_vreg)
                        continue;

                    if (inst.reg() == spilled_vreg) {
                        // This instruction result is stored into the spilled virtual register;
                        // insert the spill right after this instruction.
                        spill_after = true;
                    }
                    for (auto& op : inst.all_operands()) {
                        // for every operand...
                        if (
                            std::holds_alternative<MOperandRegister>(op)
                            and std::get<MOperandRegister>(op).value == spilled_vreg
                        ) {
                            spill_after = true;
                            break;
                        }
                    }
                    if (spill_after) {
                        // insert spill after
                        auto spill = MInst(usz(MInst::Kind::Spill), {});

                        // Copy value and size of virtual register
                        spill.add_operand(MOperandRegister(
                            spilled_vreg,
                            (uint) spilled_vreg_size
                        ));
                        // Slot
                        spill.add_operand(MOperandImmediate(spilled_vreg));

                        // Insert spill instruction after index 'inst_i'.
                        block.instructions().insert(
                            block.instructions().begin() + inst_i + 1,
                            spill
                        );
                        break;
                    }
                }
            }

            // Next, we need to replace all uses of the /virtual/ register value with
            // the use of an inserted instruction directly /before/ the use.
            // That inserted instruction is a "load from stack" instruction (unspill).
            bool done{false};
            bool unspill_before{false};
            // This is the virtual register value that we will unspill "into"; uses of
            // the spilled register regarding a particular unspill should be replaced
            // with this register (the result of the unspill).
            // TODO: Er, how do we get a (guaranteed) unique virtual register from here?
            usz new_vreg = 0x06942069;
            for (auto& block : function.blocks()) {
                if (done)
                    break;

                for (auto inst_i = block.instructions().size(); inst_i; --inst_i) {
                    auto& inst = block.instructions().at(inst_i - 1);

                    // We are done when we find the relevant spill instruction (it wouldn't
                    // make sense to unspill /before/ a spill, now would it?).
                    if (inst.opcode() == +MInst::Kind::Spill) {
                        auto reg = std::get<MOperandRegister>(inst.get_operand(0));
                        if (reg.value == spilled_vreg) {
                            done = true;
                            break;
                        }
                    }

                    // Result of this instruction is the virtual register;
                    // er, I'm not sure this counts as a "use"?
                    if (inst.reg() == spilled_vreg) {
                        unspill_before = true;
                        inst.reg(new_vreg);
                    }

                    for (auto& op : inst.all_operands()) {
                        // Operand of this instruction is the spilled virtual register.
                        if (
                            std::holds_alternative<MOperandRegister>(op)
                            and std::get<MOperandRegister>(op).value == spilled_vreg
                        ) {
                            unspill_before = true;
                            std::get<MOperandRegister>(op).value = new_vreg;
                        }
                    }

                    if (unspill_before and inst_i > 1) {
                        auto prev_inst = block.instructions().at(inst_i - 2);
                        // We've already updated references to spilled vreg with new virtual
                        // register; now we just need to keep going until we actually need to
                        // insert the spill.
                        if (prev_inst.reg() == inst.reg())
                            continue;
                    }

                    if (unspill_before) {
                        // New virtual register that this particular use of the spilled vreg will
                        // unspill into.
                        auto unspill = MInst(
                            usz(MInst::Kind::Unspill),
                            {new_vreg, (uint) spilled_vreg_size, {}, true}
                        );
                        unspill.add_use(); // unspill shouldn't be "unused"; it has side effects
                        // Slot
                        unspill.add_operand(MOperandImmediate(spilled_vreg));

                        // Inserting unspill instruction before index 'inst_i'
                        block.instructions().insert(
                            block.instructions().begin() + (isz) inst_i - 1,
                            unspill
                        );
                        // Update new_vreg to a new, unique virtual register.
                        // TODO: Guarantee uniquness
                        new_vreg++;
                        unspill_before = false;
                    }
                }
            }

            // With the new spill/unspill instructions inserted, retry register
            // allocation...
            // FIXME: Unnecessarily grows stack. We could return a "retry" value and
            // retry from the call site. But, this isn't a performance issue right now.
            return allocate_registers(desc, function);
        }

        list.color = reg_value;
        list.allocated = true;

        // Track registers used in function so that calls can properly do
        // caller-saved registers.
        function.registers_used().insert(u8(list.color));

        // fmt::print("Vreg {} mapped to HWreg {}\n", list.value, list.color);
    }

    // STEP SIX
    // Actually update all references to old virtual registers with newly
    // colored hardware registers.
    for (auto& list : lists) {
        // Skip hardware registers (no need to color them).
        if (list.value < +Module::first_virtual_register) continue;
        LCC_ASSERT(
            list.allocated,
            "AdjacencyList must have a color allocated"
        );
        usz vreg = list.value;
        usz color = list.color;
        for (auto& block : function.blocks()) {
            for (auto& instruction : block.instructions()) {
                if (instruction.reg() == vreg)
                    instruction.reg(color);

                for (auto& op : instruction.all_operands()) {
                    if (std::holds_alternative<MOperandRegister>(op)) {
                        MOperandRegister reg = std::get<MOperandRegister>(op);
                        if (reg.value == vreg) {
                            reg.value = color;
                            op = reg;
                        }
                    }
                }
            }
        }
    }

    for (auto& block : function.blocks()) {
        for (usz inst_i = 0; inst_i < block.instructions().size(); ++inst_i) {
            usz result_register{};
            {
                auto& inst = block.instructions().at(inst_i);
                auto found = rgs::find(desc.preserve_volatile_opcodes, inst.opcode());
                if (found == desc.preserve_volatile_opcodes.end())
                    continue;
                // We need to preserve volatile registers here.

                result_register = inst.reg();

                // fmt::print(
                //     "Preserving volatile registers across `{}`\n",
                //     PrintMInst(inst)
                // );
            }

            // TODO: What we should have is a snapshot of the live values at the exit
            // of this instruction's execution. If a volatile register was live before
            // but isn't after, there's no need to preserve it.
            for (auto r : function.registers_used()) {
                // If this happens to be a volatile register...
                auto interfering_is_volatile = rgs::contains(
                    desc.volatile_registers,
                    r
                );
                // fmt::print("    volatile: {}\n", interfering_is_volatile);

                if (not interfering_is_volatile)
                    continue;

                // A volatile register is in use at the time of a call instruction's
                // execution; we need to preserve it across the call (as long as it isn't
                // the result register itself).

                // Skip result register (otherwise we'd clobber our function result out of
                // existence).
                if (r == result_register)
                    continue;

                // fmt::print("  spilling...\n");

                // TODO: Is this slot okay? Should it be unique per call site?
                auto spill_slot = r;
                auto spilled_reg = r;
                // FIXME: don't hardcode register size
                uint spilled_regsize = 64;

                // Insert a spill before
                auto spill = MInst(usz(MInst::Kind::Spill), {});

                // Copy value and size of virtual register
                spill.add_operand(MOperandRegister(
                    spilled_reg,
                    spilled_regsize
                ));
                // Slot
                spill.add_operand(MOperandImmediate(spill_slot));

                // Insert spill instruction before index 'inst_i'.
                block.instructions().insert(
                    block.instructions().begin() + (isz) inst_i,
                    spill
                );
                ++inst_i;

                // Insert an unspill after
                auto unspill = MInst(
                    usz(MInst::Kind::Unspill),
                    {spilled_reg, (uint) spilled_regsize, {}, true}
                );
                unspill.add_use(); // unspill shouldn't be "unused"; it has side effects
                // Slot
                unspill.add_operand(MOperandImmediate(spill_slot));

                // Inserting unspill instruction after index 'inst_i'
                block.instructions().insert(
                    block.instructions().begin() + (isz) inst_i + 1,
                    unspill
                );
            }
        }
    }

    return {};
}

} // namespace lcc
