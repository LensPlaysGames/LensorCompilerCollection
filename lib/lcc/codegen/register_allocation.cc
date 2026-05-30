#include <lcc/codegen/register_allocation.hh>

#include <lcc/codegen/mir.hh>
#include <lcc/codegen/x86_64/x86_64.hh>
#include <lcc/ir/module.hh>
#include <lcc/typedefs.hh>
#include <lcc/utils.hh>

#include <fmt/format.h>
#include <fmt/ranges.h>

#include <algorithm>
#include <memory>
#include <ranges>
#include <string>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

namespace lcc {

constexpr bool RA_PRINT = false;

struct AdjacencyList {
    // List of live indices that interfere with this->value.
    std::vector<usz> adjacencies{};

    // Value/Id of virtual register this list is for.
    // NOTE: We don't just use an lcc::Register instance here because an LCC
    // Register has an enum for the category, and we want ISel for a
    // particular backend to extend the available categories.
    usz register_value{};
    usz register_category{};

    // This is the index within the registers list where the register
    // associated with this list (*NOT* the register it's colored with) may be
    // found.
    usz live_index{};

    // Value/Id of register that this list has been colored with.
    usz color{};
    // Whether or not this list has been allocated a register (color has been
    // set).
    bool allocated{false};

    [[nodiscard]]
    auto degree() const {
        return adjacencies.size();
    }

    [[nodiscard]]
    auto string_base() const -> std::string {
        return fmt::format("r{}", register_value);
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
                    return adj_i == l.register_value;
                }
            );
            out += lists.at(usz(found - lists.begin())).string_base();
        }
        return out;
    }
};

struct AdjacencyMatrix {
    const std::vector<Register>& registers;
    std::vector<AdjacencyList> lists{};

    std::unordered_map<usz, usz> _register_id_to_live_index{};

    // Find the list with a register value matching the given register id.
    // @return the cached index of that list.
    auto live_index(usz register_id) {
        return _register_id_to_live_index.at(register_id);
    }

    auto& list_by_live_index(usz live_index) {
        /** (!)
         * Due to how lists are constructed from the registers list, the indices
         * line up 1:1.
         **/
        return lists.at(live_index);
    }
    const auto& list_by_live_index(usz live_index) const {
        return lists.at(live_index);
    }

    auto& list_by_register_id(usz register_id) {
        return list_by_live_index(live_index(register_id));
    }

    explicit AdjacencyMatrix(const std::vector<Register>& registers_)
        : registers(registers_) {
        lists.reserve(registers.size());
        _register_id_to_live_index.reserve(registers.size());
        for (auto [i, reg] : vws::enumerate(registers)) {
            AdjacencyList list{};
            list.adjacencies.reserve(registers.size());
            // The list index is equivalent to the index where the corresponding
            // register may be found in the main list of registers.
            list.live_index = usz(i);
            // The list value is equivalent to the corresponding register's value.
            list.register_value = reg.value;
            // The list category is equivalent to the corresponding register's
            // category.
            list.register_category = +reg.category;
            // Hardware registers do not need allocated; they are already "colored
            // in".
            // NOTE: All hardware registers *must* have allocated flag set to true.
            if (list.register_value < +Module::first_virtual_register) {
                list.color = list.register_value;
                list.allocated = true;
            }
            lists.emplace_back(list);

            // Build cache
            _register_id_to_live_index[list.register_value] = list.live_index;
        }
    }

    AdjacencyMatrix(AdjacencyMatrix&& other) = delete;
    AdjacencyMatrix& operator=(const AdjacencyMatrix& other) = delete;
    AdjacencyMatrix& operator=(AdjacencyMatrix&& other) = delete;

    [[nodiscard]]
    auto at(usz x, usz y) const -> bool {
        if (x == y) Diag::ICE("alike coordinates: x and y are {}", x);
        return rgs::contains(list_by_live_index(x).adjacencies, y);
    }

    void set(usz x, usz y) {
        if (x == y) return;
        auto& x_adjacencies = list_by_live_index(x).adjacencies;
        if (not rgs::contains(x_adjacencies, y)) {
            x_adjacencies.emplace_back(y);
            list_by_live_index(y).adjacencies.emplace_back(x);
        }
    }
};

namespace {

void remove_defining(std::vector<usz>& live_values, const MInst& inst) {
    // If the defining use of a virtual register is an operand of this
    // instruction, remove it from vector of live vals.
    if (inst.is_defining()) {
        std::erase(live_values, inst.reg());
        if constexpr (RA_PRINT) {
            fmt::print(
                "{}  ; IS NO LONGER LIVE\n",
                PrintMOperand(Register(
                    inst.reg(),
                    (uint) inst.regsize(),
                    (Register::Category) inst.regcategory()
                ))
            );
        }
    }

    for (auto& op : inst.all_operands()) {
        if (std::holds_alternative<MOperandRegister>(op)) {
            auto reg = std::get<MOperandRegister>(op);
            if (reg.defining_use) {
                std::erase(live_values, reg.value);
                if constexpr (RA_PRINT) {
                    fmt::print(
                        "{}  ; IS NO LONGER LIVE\n",
                        PrintMOperand(reg)
                    );
                }
            }
        }
    }
}

void matrix_set_clobbers(
    AdjacencyMatrix& matrix,
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
            auto live_idx = matrix.live_index(reg.value);
            for (auto live : live_values) {
                matrix.set(
                    matrix.live_index(live),
                    live_idx
                );
                if constexpr (RA_PRINT) {
                    fmt::print(
                        "Clobber r{} interferes with live value r{}\n",
                        reg.value,
                        live
                    );
                }
            }
        }
    }

    for (auto r_id : inst.register_clobbers()) {
        auto live_idx = matrix.live_index(r_id);
        for (auto live : live_values) {
            matrix.set(
                matrix.live_index(live),
                live_idx
            );
            if constexpr (RA_PRINT) {
                fmt::print(
                    "Clobber r{} interferes with live value r{}\n",
                    r_id,
                    live
                );
            }
        }
    }
}

struct RegisterPlusLiveValIndex {
    Register reg;
    usz idx;
};

std::vector<RegisterPlusLiveValIndex> collect_vreg_operands(
    AdjacencyMatrix& matrix,
    const MInst& inst
) {
    std::vector<RegisterPlusLiveValIndex> vreg_operands{};
    if (inst.reg() >= +Module::first_virtual_register) {
        vreg_operands.push_back(
            {Register{
                 inst.reg(),
                 uint(inst.regsize()),
                 (Register::Category) inst.regcategory(),
                 inst.is_defining()
             },
             matrix.live_index(inst.reg())}
        );
    }
    for (auto& op : inst.all_operands()) {
        if (std::holds_alternative<MOperandRegister>(op)) {
            auto reg = std::get<MOperandRegister>(op);
            if (reg.value >= +Module::first_virtual_register)
                vreg_operands.push_back({reg, matrix.live_index(reg.value)});
        }
    }
    return vreg_operands;
}

std::vector<usz> collect_clobbered_registers(const MInst& inst) {
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
        ) {
            live_values.push_back(r.reg.value);
            if constexpr (RA_PRINT) {
                fmt::print("{}  ; IS NOW LIVE\n", PrintMOperand(r.reg));
            }
        }
    }
    // Handle the case of a non-defining register operand in use of the
    // instruction that defines that register.
    if (inst.is_defining()) {
        std::erase(live_values, inst.reg());
        if constexpr (RA_PRINT) {
            fmt::print(
                "{}  ; IS ACTUALLY NOT LIVE\n",
                PrintMOperand(Register(
                    inst.reg(),
                    (uint) inst.regsize(),
                    (Register::Category) inst.regcategory()
                ))
            );
        }
    }
}

void collect_interferences_from_instruction(
    AdjacencyMatrix& matrix,
    std::vector<usz>& live_values,
    std::vector<MBlock*>& visited,
    std::vector<MBlock*>& doubly_visited,
    MInst& inst
) {
    if constexpr (RA_PRINT) {
        fmt::print("{}\n", PrintMInstImpl(inst, x86_64::opcode_to_string));
        fmt::print("live after: {}\n", fmt::join(live_values, ", "));
    }

    // If the defining use of a virtual register is an operand of this
    // instruction, remove it from live values.
    remove_defining(live_values, inst);

    // fmt::print("live during: {}\n", fmt::join(live_values, ", "));

    // Make registers clobbered by this instruction's execution interfere with
    // all live values; this is necessary because, if any value we later
    // needed were in these registers, execution of this instruction would
    // invalidate that value.
    matrix_set_clobbers(matrix, live_values, inst);

    // Collect all register operands from this instruction that are
    // used as operands somewhere in the function (i.e. within the list of
    // registers).
    auto vreg_operands = collect_vreg_operands(matrix, inst);

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
        // If either A or B is defining or clobbered, do NOT set interference
        // between the two.
        if (
            A.reg.defining_use
            or rgs::contains(clobbered_regs, A.reg.value)
        ) continue;
        for (auto B : vreg_operands) {
            if (
                B.reg.defining_use
                or rgs::contains(clobbered_regs, B.reg.value)
            ) continue;

            // Otherwise, A and B are just "regular" virtual register operands,
            // meaning they should interfere.

            matrix.set(A.idx, B.idx);
            if constexpr (RA_PRINT) {
                fmt::print(
                    "Non-clobbered register operands {} and {} interfere\n",
                    PrintMOperand(A.reg),
                    PrintMOperand(B.reg)
                );
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
                matrix.live_index(live)
            );
            if constexpr (RA_PRINT) {
                fmt::print(
                    "Virtual register operand {} and live value r{} interfere\n",
                    PrintMOperand(r.reg),
                    live
                );
            }
        }
    }

    // If a virtual register is not live and is seen as an operand, it is
    // added to the vector of live values.
    record_newly_live_values(vreg_operands, inst, live_values);

    if constexpr (RA_PRINT)
        fmt::print("live before: {}\n", fmt::join(live_values, ", "));
}

void collect_interferences_from_block(
    AdjacencyMatrix& matrix,
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
            function,
            {},
            {},
            {},
            exit
        );
    }
}

bool instruction_references_register_id(const MInst& inst, usz register_id) {
    if (inst.reg() == register_id)
        return true;

    for (auto& op : inst.all_operands()) {
        if (
            std::holds_alternative<MOperandRegister>(op)
            and std::get<MOperandRegister>(op).value == register_id
        ) return true;
    }

    return false;
}

void insert_spill_unspill(
    MFunction& function,
    const Register& to_spill,
    usz& next_unique_register
) {
    // We need to find instruction by virtual register value (find the
    // relevant defining use).
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

            const auto next_inst = block.instructions().begin() + inst_i + 1;
            if (
                next_inst < block.instructions().end()
                and instruction_references_register_id(*next_inst, to_spill.value)
            ) continue;

            if (instruction_references_register_id(inst, to_spill.value)) {
                // fmt::print("Inserting spill after:\n");
                // fmt::print("{}\n", PrintMInst(inst));
                LCC_ASSERT(
                    not (
                        inst.kind() == MInst::Kind::Spill
                        and std::get<MOperandRegister>(inst.get_operand(0)).value == to_spill.value
                    ),
                    "spilling a spill spilling a spill spilling a spill..."
                );
                // insert spill after
                auto spill = MInst(usz(MInst::Kind::Spill), {});
                // Copy value and size of virtual register
                spill.add_operand(MOperandRegister(
                    to_spill.value,
                    (uint) to_spill.size
                ));
                // Slot
                spill.add_operand(MOperandImmediate(
                    to_spill.value
                ));

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
    auto new_vreg = next_unique_register++;
    for (auto& block : function.blocks()) {
        if (done)
            break;

        for (auto inst_i = block.instructions().size(); inst_i; --inst_i) {
            auto& inst = block.instructions().at(inst_i - 1);

            // We are done when we find the relevant spill instruction (it wouldn't
            // make sense to unspill /before/ a spill, now would it?).
            if (inst.opcode() == +MInst::Kind::Spill) {
                auto reg = std::get<MOperandRegister>(inst.get_operand(0));
                if (reg.value == to_spill.value) {
                    done = true;
                    break;
                }
            }

            unspill_before = instruction_references_register_id(inst, to_spill.value);

            if (not unspill_before) continue;

            {
                // Replace references to "old" register to the value that is produced by the unspill.
                if (inst.reg() == to_spill.value)
                    inst.reg(new_vreg);

                for (auto& op : inst.all_operands()) {
                    if (std::holds_alternative<MOperandRegister>(op)) {
                        auto& r = std::get<MOperandRegister>(op);
                        if (r.value == to_spill.value)
                            r.value = new_vreg;
                    }
                }
            }

            if (inst_i > 1) {
                auto prev_inst = block.instructions().at(inst_i - 2);
                // We've already updated references to spilled vreg with new virtual
                // register; now we just need to keep going until we actually need to
                // insert the spill.
                if (instruction_references_register_id(prev_inst, to_spill.value))
                    continue;
            }

            // New virtual register that this particular use of the spilled vreg will
            // unspill into.
            auto unspill = MInst(
                usz(MInst::Kind::Unspill),
                {new_vreg, (uint) to_spill.size, {}, true}
            );
            unspill.add_use(); // unspill shouldn't be "unused"; it has side effects
            // Slot
            unspill.add_operand(MOperandImmediate(to_spill.value));

            // Inserting unspill instruction before index 'inst_i'
            block.instructions().insert(
                block.instructions().begin() + (isz) inst_i - 1,
                unspill
            );
            // Update new_vreg to a new, unique virtual register.
            new_vreg = next_unique_register++;
            unspill_before = false;
        }
    }

    // fmt::print("After RA Spill:\n{}\n", PrintMFunction(function));
}

} // namespace

auto allocate_registers(
    const MachineDescription& desc,
    MFunction& function,
    usz& next_unique_register
) -> Result<void> {
    // Don't allocate registers for empty functions.
    if (function.blocks().empty())
        return {};

    // It's annoying, but functions may have blocks that are all completely
    // empty.
    for (auto b : function.blocks()) {
        if (b.instructions().empty())
            Diag::ICE("IR block is completely empty");
    }

    // Steps:
    //   1. Collect all existing registers, both hardware and virtual.
    //   2. Walk control flow in reverse, build adjacency matrix as you go.
    //      The adjacency matrix stores data regarding interference between
    //      registers, both hardware and virtual.
    //   3. Determine the order we will assign colors in.
    //      This matters because of "Lucky Colorings"
    //
    //        A  B  C | Live Values
    //        ---------------------
    //        O     O | (A, C)
    //        |     | | (A, C)
    //        X  O  | | (B, C)
    //           X  X | ()
    //
    //      If both A and B get assigned the same color, then all of A, B, and C
    //      may be colored with just two colors, despite the degree of C being 2.
    //      This is (part of) why there is a "degree < k" rule that means it *can*
    //      be colored, but no "degree >= k" that says a node cannot be colored.
    //
    //      Therefore, we try to order the nodes such that we assign colors to A
    //      and B first, before we ever try to color C, since coloring C
    //      disqualifies both A and B from ever using that register, but coloring A
    //      or B only disqualifies C from using that register.
    //
    //   4. Assign colors to registers, ensuring no overlap (adjacencies).
    //      Follows the order determined by STEP THREE.
    //   5. Map colors to registers, updating all register operands to the
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
        return desc.return_registers.at(category).at(0);
    };
    for (auto& block : function.blocks()) {
        for (auto& inst : block.instructions()) {
            // Replace return register in result register.
            if (inst.reg() == desc.return_register_to_replace)
                inst.reg(return_register_by_category(inst.regcategory()));

            // Replace return register in register operands.
            for (auto& op : inst.all_operands()) {
                if (std::holds_alternative<MOperandRegister>(op)) {
                    MOperandRegister reg = std::get<MOperandRegister>(op);
                    if (reg.value == desc.return_register_to_replace) {
                        reg.value = return_register_by_category(+reg.category);
                        op = reg;
                    }
                }
            }

            // Replace return register in register clobbers.
            for (auto& clobber : inst.register_clobbers()) {
                if (clobber == desc.return_register_to_replace)
                    clobber = return_register_by_category(inst.regcategory());
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
    // Populate list of registers, using both hardware and virtual registers.
    std::vector<Register> registers{4096 / sizeof(Register)};
    {
        // Helper function that ensures no duplicate registers show up in the
        // register list.
        auto add_reg = [&](usz id, usz size, usz category) {
            auto found = rgs::find_if(
                registers,
                [&](Register& r) {
                    return r.value == id;
                }
            );
            if (found == registers.end()) {
                registers.emplace_back(
                    id,
                    uint(size),
                    (Register::Category) category
                );
            }
        };

        // Hardware registers are passed in through the machine description.
        // Add all (relevant) hardware registers to the list of all registers.
        for (const auto& [register_category, register_list] : desc.registers) {
            for (auto reg : register_list)
                add_reg(reg, 0, register_category);
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
    }
    // We know registers is not going to change beyond this point.

    // STEP TWO
    // Walk control flow in reverse, build adjacency matrix as you go.
    // We walk in reverse because of how control flow tends to work; a single
    // vreg may have multiple defining uses in different predecessor blocks.
    AdjacencyMatrix matrix{registers};

    // Collect the interferences into the matrix by walking CFG in reverse.
    collect_interferences(matrix, function);

    // STEP THREE
    // A list of live indices that is sorted in the order we should assign
    // colors to those live indices in.
    // NOTE: Without this, we would never be able to find "lucky colorings".
    std::vector<usz> assignment_order{};
    assignment_order.reserve(matrix.lists.size());
    {
        std::vector<usz> assignment_order_rest{};

        for (auto& list : matrix.lists) {
            if (list.allocated) continue;
            if (list.degree() < desc.registers.at(list.register_category).size())
                assignment_order.emplace_back(list.live_index);
            else assignment_order_rest.emplace_back(list.live_index);
        }

#ifdef __cpp_lib_containers_ranges
        assignment_order.append_range(assignment_order_rest);
#else
        assignment_order.insert(
            assignment_order.end(),
            assignment_order_rest.begin(),
            assignment_order_rest.end()
        );
#endif
    }
    // fmt::print("Color Assignment Ordering (Live Index): {}\n", fmt::join(coloring_stack, ", "));

    // STEP FOUR
    // Use coloring stack to assign hardware registers (colors) to virtual
    // registers, ensuring no overlap (interferences/adjacencies).
    for (auto ordered_index : assignment_order) {
        auto& list = matrix.lists.at(ordered_index);
        // NOTE: There should never be a hardware register on the coloring stack.

        // Attempt to find a hardware register that is NOT marked as interfering
        // with the register associated with the current adjacency list.
        const auto& register_set = desc.registers.at(list.register_category);
        // Each register id here will be within the correct category, and so
        // should be a viable candidate for us to color this list with;
        // we just have to make sure they don't otherwise interfere.
        for (auto register_id : register_set) {
            bool adjacent{false};
            // If a single hardware register makes it through every adjacency without
            // finding one that is already coloured, and specifically coloured with
            // this hardware register, it means this hardware register is not adjacent,
            // and is valid to assign to this particular list.
            for (const auto& adjacency : list.adjacencies) {
                auto& adjacent_list = matrix.list_by_live_index(adjacency);
                if (
                    adjacent_list.allocated
                    and adjacent_list.color == register_id
                ) {
                    adjacent = true;
                    break;
                }
            }
            if (adjacent) continue;

            // Any hardware register that is *not* adjacent to the virtual register in
            // question may be used to color that virtual register.

            list.allocated = true;
            list.color = register_id;

            // Track registers used in function so that calls can properly do
            // caller-saved registers, or something.
            function.registers_used().insert(u8(list.color));

            if constexpr (RA_PRINT) {
                if (list.degree() >= desc.registers.at(list.register_category).size()) {
                    fmt::print("RA: Found lucky coloring :)\n");
                }
                fmt::print("Vreg {} mapped to HWreg {}\n", list.register_value, list.color);
            }

            break;
        }

        if (not list.allocated) {
            // fmt::print("Could not allocate r{}\n", list.register_value);

            // Find already-allocated (non-hardware) register with highest degree
            // Only have to check previous lists in assignment order.
            usz spill_index{-1uz};
            for (auto spill_candidate_index : assignment_order) {
                if (spill_candidate_index == ordered_index) break;
                auto& spill_candidate = matrix.list_by_live_index(spill_candidate_index);
                if (
                    spill_candidate.register_value >= +Module::first_virtual_register
                    and spill_candidate.allocated
                    and spill_candidate.register_category == list.register_category
                ) {
                    // fmt::print(
                    //     "  valid spill candidate: r{} (degree {})\n",
                    //     registers.at(spill_candidate.live_index).value,
                    //     spill_candidate.degree()
                    // );

                    // We could spill this, it's a valid candidate.
                    if (
                        spill_index == -1uz
                        or spill_candidate.degree() > matrix.list_by_live_index(spill_index).degree()
                    ) spill_index = spill_candidate_index;
                }
            }

            LCC_ASSERT(spill_index != -1uz, "RA failed to pick a spill candidate");

            insert_spill_unspill(
                function,
                registers.at(spill_index),
                next_unique_register
            );

            // fmt::print("After spill:\n{}\n", PrintMFunction(function));

            return allocate_registers(desc, function, next_unique_register);
        }
    }

    // STEP FIVE
    // Actually update all references to old virtual registers with newly
    // colored hardware registers.
    for (auto& block : function.blocks()) {
        for (auto& instruction : block.instructions()) {
            if (instruction.reg() >= +Module::first_virtual_register) {
                instruction.reg(
                    matrix.list_by_register_id(instruction.reg()).color
                );
            }

            for (auto& op : instruction.all_operands()) {
                if (std::holds_alternative<MOperandRegister>(op)) {
                    auto& reg = std::get<MOperandRegister>(op);
                    if (reg.value >= +Module::first_virtual_register)
                        reg.value = matrix.list_by_register_id(reg.value).color;
                }
            }
        }
    }

    // STEP SIX
    // Insert spills/unspills for preserving registers across instructions
    // that (may) clobber them.
    for (auto& block : function.blocks()) {
        for (usz inst_i = 0; inst_i < block.instructions().size(); ++inst_i) {
            Register result_register{};
            {
                auto& inst = block.instructions().at(inst_i);

                if (not rgs::contains(desc.preserve_volatile_opcodes, inst.opcode()))
                    continue;
                // We need to preserve volatile registers here.

                result_register = Register{
                    inst.reg(),
                    (uint) inst.regsize(),
                    (Register::Category) inst.regcategory(),
                    inst.is_defining()
                };

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
                if (r == result_register.value)
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

                // Don't clobber the result register out of existence by unspilling...
                if (result_register.value) {
                    auto return_register = return_register_by_category((usz) result_register.category);
                    if (result_register.value != return_register) {
                        auto move = MInst(
                            usz(MInst::Kind::Copy),
                            result_register
                        );
                        move.add_operand(MOperandRegister(return_register, result_register.size, result_register.category));
                        block.instructions().insert(
                            block.instructions().begin() + (isz) inst_i + 1,
                            move
                        );
                        ++inst_i;
                    }
                }

                // Insert an unspill before the next instruction that does not contain a
                // reference to the result register as a register operand.
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
                ++inst_i;
            }
        }
    }

    return {};
}

} // namespace lcc
