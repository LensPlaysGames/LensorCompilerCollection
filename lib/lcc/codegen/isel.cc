#include <lcc/codegen/isel.hh>
#include <lcc/codegen/mir.hh>
#include <lcc/codegen/mir_utils.hh>
#include <lcc/codegen/x86_64/isel_patterns.hh>
#include <lcc/codegen/x86_64/x86_64.hh>
#include <lcc/context.hh>
#include <lcc/ir/module.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>
#include <variant>

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
    // Only calculate defining uses for blocks reachable from the entry point (first block).
    calculate_defining_uses_for_block(function, {}, &function.blocks().front(), {}, {});
}

void select_instructions(Module* mod, MFunction& function) {
    // Don't selection instructions for empty functions.
    if (function.blocks().empty()) return;

    if (mod->context()->target()->is_arch_x86_64()) {
        function = lcc::isel::x86_64::AllPatterns::rewrite(mod, function);

        // In-code instruction selection. Ideally, we wouldn't have to do this at
        // all.
        for (auto& block : function.blocks()) {
            for (usz index = 0; index < block.instructions().size(); ++index) {
                MInst& inst = block.instructions().at(index);
                switch (inst.kind()) {
                    // Rewrite truncates into bitwise ands.
                    //
                    // r3.1 | M.Trunc r1.64
                    // becomes the following machine code, GNU syntax
                    //     mov %r1.64, %r3.64
                    //     and $1, %r3.64
                    case MInst::Kind::Trunc: {
                        if (std::holds_alternative<MOperandImmediate>(inst.get_operand(0))) {
                            auto imm = std::get<MOperandImmediate>(inst.get_operand(0));
                            auto mov_imm = MInst(usz(x86_64::Opcode::Move), {0, 0});

                            // Set bottom inst.regsize() bits of mask.
                            usz mask = 0;
                            for (usz bit_i = 0; bit_i < inst.regsize(); ++bit_i)
                                mask |= (usz(1) << bit_i);

                            // Use mask to do truncation
                            imm.value &= mask;

                            mov_imm.add_operand(imm);
                            mov_imm.add_operand(MOperandRegister{inst.reg(), uint(inst.regsize())});
                            block.instructions()[index] = mov_imm;
                            break;
                        }

                        LCC_ASSERT(
                            std::holds_alternative<MOperandRegister>(inst.get_operand(0)),
                            "Sorry, but you can only truncate registers and immediates for right now"
                        );

                        auto mov_inst = MInst(usz(x86_64::Opcode::Move), {0, 0});
                        auto operand_reg = std::get<MOperandRegister>(inst.get_operand(0));
                        auto destination_reg = Register(inst.reg(), uint(inst.regsize()));
                        destination_reg.size = operand_reg.size;
                        mov_inst.add_operand(operand_reg);
                        mov_inst.add_operand(destination_reg);

                        // Truncating a register to a smaller register size is a no-op; we will
                        // just access the smaller portion of the register.
                        if (inst.regsize() == 8 or inst.regsize() == 16 or inst.regsize() == 32) {
                            block.instructions()[index] = mov_inst;
                            break;
                        }

                        auto and_inst = MInst(usz(x86_64::Opcode::And), {0, 0});
                        // Set bottom inst.regsize() bits of immediate.
                        usz imm = 0;
                        for (usz bit_i = 0; bit_i < inst.regsize(); ++bit_i)
                            imm |= (usz(1) << bit_i);

                        and_inst.add_operand(MOperandImmediate(imm));
                        and_inst.add_operand(destination_reg);

                        // replace truncate with move
                        block.instructions()[index] = mov_inst;
                        // insert and instruction after move, previously truncate
                        // ++index to skip newly inserted instruction.
                        block.instructions().insert(block.instructions().begin() + isz(++index), and_inst);
                    } break;

                    default: break;
                }
            }
        }
    } else LCC_ASSERT(false, "Unhandled architecture in instruction selection");

    calculate_defining_uses(function);
}

} // namespace lcc
