#include <lcc/codegen/isel.hh>
#include <lcc/codegen/isel_patterns_x86_64.hh>
#include <lcc/codegen/mir.hh>
#include <lcc/codegen/x86_64.hh>
#include <lcc/context.hh>
#include <lcc/ir/module.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>

namespace lcc {

// Mark defining uses of virtual registers within a single block.
static void mark_defining_uses(std::vector<usz>& regs_seen, MBlock* block) {
    for (auto& inst : block->instructions()) {
        if (inst.reg() >= +MInst::Kind::ArchStart and std::find(regs_seen.begin(), regs_seen.end(), inst.reg()) == regs_seen.end()) {
            inst.is_defining(true);
            regs_seen.push_back(inst.reg());
        }
        for (auto& op : inst.all_operands()) {
            if (std::holds_alternative<MOperandRegister>(op)) {
                auto reg = std::get<MOperandRegister>(op);
                if (reg.value >= +MInst::Kind::ArchStart and std::find(regs_seen.begin(), regs_seen.end(), reg.value) == regs_seen.end()) {
                    reg.defining_use = true;
                    op = reg;
                    regs_seen.push_back(reg.value);
                }
            }
        }
    }
}

/// NOTE: Pass empty vectors for visited and doubly_visited when
/// calling on entry of function.
static void calculate_defining_uses_for_block(
    MFunction& function,
    std::vector<usz> regs_seen,
    MBlock* block,
    std::vector<MBlock*> visited,
    std::vector<MBlock*> doubly_visited
) {
    LCC_ASSERT(block, "Cannot calculate defining uses for NULL block");

    /// Don't visit the same block thrice.
    if (std::find(visited.begin(), visited.end(), block) != visited.end()) {
        if (std::find(doubly_visited.begin(), doubly_visited.end(), block) != doubly_visited.end())
            return;
        doubly_visited.push_back(block);
    } else visited.push_back(block);

    // At each block of the function, starting at the entry, walk the
    // control flow. The first operand usage of a virtual register will
    // be set to a defining use (i.e. the first place that that register
    // must be classified as "in use" by the register allocator).

    mark_defining_uses(regs_seen, block);

    // To follow control flow of an exit block, we stop.
    if (not block->successors().size()) return;

    // If a block has only a single successor, we don't need to do
    // anything fancy; just follow the simple control flow for as long as
    // possible.
    while (block->successors().size() == 1) {
        block = function.block_by_name(block->successors().front());
        mark_defining_uses(regs_seen, block);
    }
    // If a block has multiple successors, we will "remember" the
    // registers that we had seen at the beginning, and reset
    // to that after following each one.
    // To follow control flow, we sometimes have to come back to a
    // block after reaching the exit, as some blocks have multiple
    // successors. We use recursion for this.
    for (auto successor : block->successors()) {
        // Copy registers seen.
        std::vector<usz> regs_seen_copy = {regs_seen};
        calculate_defining_uses_for_block(function, regs_seen_copy, function.block_by_name(successor), visited, doubly_visited);
    }
}

static void calculate_defining_uses(MFunction& function) {
    calculate_defining_uses_for_block(function, {}, &function.blocks().front(), {}, {});
}

void select_instructions(Module* mod, MFunction& function) {
    // Don't selection instructions for empty functions.
    if (function.blocks().empty()) return;

    if (mod->context()->target()->is_x64()) {
        // TODO: rewrite truncates into bitwise ands.

        function = lcc::isel::x86_64PatternList::rewrite(mod, function);
    }

    calculate_defining_uses(function);
}

} // namespace lcc
