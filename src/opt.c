#include <opt.h>
#include <codegen.h>
#include <codegen/intermediate_representation.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <vector.h>

#ifdef _MSVC_VER
#include <intrin.h>

uint32_t ctzll(uint64_t value) {
  uint32_t zero = 0;
  return _BitScanForward64(&zero, value)
    ? zero
    : 32;
}
#else
#define ctzll __builtin_ctzll
#endif

#define IR_REDUCE_BINARY(op)                 \
  if (ipair(i)) {                            \
    IRInstruction *car = i->value.pair.car;  \
    IRInstruction *cdr = i->value.pair.cdr;  \
    i->type = IR_IMMEDIATE;                  \
    i->value.immediate = icar(i) op icdr(i); \
    ir_remove_use(car, i);                   \
    ir_remove_use(cdr, i);                   \
    changed = true;                          \
  }

static bool ipair(IRInstruction *i) {
  return i->value.pair.car->type == IR_IMMEDIATE &&
         i->value.pair.cdr->type == IR_IMMEDIATE;
}

static int64_t icar(IRInstruction *i) {
  return i->value.pair.car->value.immediate;
}

static int64_t icdr(IRInstruction *i) {
  return i->value.pair.cdr->value.immediate;
}

static bool power_of_two(int64_t value) {
  return value > 0 && (value & (value - 1)) == 0;
}

static bool has_side_effects(IRInstruction *i) {
  STATIC_ASSERT(IR_COUNT == 28, "Handle all instructions");
  switch (i->type) {
    case IR_IMMEDIATE:
    case IR_LOAD:
    case IR_ADD:
    case IR_SUBTRACT:
    case IR_MULTIPLY:
    case IR_DIVIDE:
    case IR_MODULO:
    case IR_SHIFT_LEFT:
    case IR_SHIFT_RIGHT_ARITHMETIC:
    case IR_SHIFT_RIGHT_LOGICAL:
    case IR_LOCAL_LOAD:
    case IR_LOCAL_ADDRESS:
    case IR_GLOBAL_LOAD:
    case IR_GLOBAL_ADDRESS:
    case IR_COMPARISON:
    case IR_PARAMETER_REFERENCE:
      return false;

    default:
      return true;
  }
}

static bool opt_const_folding_and_strengh_reduction(IRFunction *f) {
  bool changed = false;
  DLIST_FOREACH (IRBlock*, b, f->blocks) {
    DLIST_FOREACH (IRInstruction*, i, b->instructions) {
      switch (i->type) {
        case IR_ADD: IR_REDUCE_BINARY(+) break;
        case IR_SUBTRACT: IR_REDUCE_BINARY(-) break;
        case IR_MULTIPLY: IR_REDUCE_BINARY(*) break;

        /// TODO: Division by 0 should be a compile error.
        case IR_DIVIDE:
          IR_REDUCE_BINARY(/)
          else {
            IRInstruction *cdr = i->value.pair.cdr;
            if (icdr(i)) {
              /// Division by 1 does nothing.
              if (cdr->value.immediate == 1) {
                ir_remove_use(i->value.pair.car, i);
                ir_remove_use(cdr, i);
                ir_replace_uses(i, i->value.pair.car);
              }

              /// Replace division by a power of two with a shift.
              else if (power_of_two(cdr->value.immediate)) {
                i->type = IR_SHIFT_RIGHT_ARITHMETIC;
                cdr->value.immediate = ctzll((uint64_t)cdr->value.immediate);
                changed = true;
              }
            }
          }
          break;
        case IR_MODULO: IR_REDUCE_BINARY(%) break;

        case IR_SHIFT_LEFT: IR_REDUCE_BINARY(<<) break;
        case IR_SHIFT_RIGHT_ARITHMETIC: IR_REDUCE_BINARY(>>) break;
        case IR_SHIFT_RIGHT_LOGICAL:
          if (ipair(i)) {
            IRInstruction *car = i->value.pair.car;
            IRInstruction *cdr = i->value.pair.cdr;
            i->type = IR_IMMEDIATE;
            i->value.immediate = (int64_t) ((uint64_t) icar(i) >> (uint64_t) icdr(i));
            ir_remove_use(car, i);
            ir_remove_use(cdr, i);
            changed = true;
          }
          break;
        default: break;
      }
    }
  }
  return changed;
}

static bool opt_dce(IRFunction *f) {
  bool changed = false;
  DLIST_FOREACH (IRBlock*, b, f->blocks) {
    for (IRInstruction *i = b->instructions.first; i;) {
      if (!i->users.size && !has_side_effects(i)) {
        IRInstruction *next = i->next;
        ir_remove(i);
        changed = true;
        i = next;
      } else {
        i = i->next;
      }
    }
  }
  return changed;
}

typedef struct {
  IRInstruction *call;
  VECTOR(IRInstruction *) phis;
} tail_call_info;

/// See opt_tail_call_elim() for more info.
static bool tail_call_possible_iter(tail_call_info *tc, IRBlock *b) {
  for (IRInstruction *i = b == tc->call->block ? tc->call->next : b->instructions.first; i; i = i->next) {
    if (i->type == IR_PHI) {
      /// If this is a phi node, then the call or a previous phi
      /// must be an argument of the phi.
      VECTOR_FOREACH (IRPhiArgument, arg, i->value.phi_arguments) {
        if (arg->value == tc->call) { goto phi; }
        VECTOR_FOREACH_PTR (IRInstruction *, a, tc->phis) {
          if (a == arg->value) { goto phi; }
        }
      }
      return false;

    phi:
      VECTOR_PUSH(tc->phis, i);
      continue;
    }

    /// If we encounter a return instruction, then a tail call
    /// is only possible if the return value is the call, or
    /// any of the PHIs.
    if (i->type == IR_RETURN) {
      VECTOR_FOREACH_PTR (IRInstruction *, a, tc->phis) { if (a == i->value.reference) { return true; } }
      return i->value.reference == tc->call;
    }

    if (i->type == IR_BRANCH) { return tail_call_possible_iter(tc, i->value.block); }
    if (i->type == IR_BRANCH_CONDITIONAL) {
      return tail_call_possible_iter(tc, i->value.conditional_branch.true_branch) &&
             tail_call_possible_iter(tc, i->value.conditional_branch.false_branch);
    }

    /// Any other instruction means that the call is not the last
    /// relevant instruction before a return.
    return false;
  }
  UNREACHABLE();
}

static bool tail_call_possible(IRInstruction *i) {
  tail_call_info tc_info = {0};
  tc_info.call = i;
  bool possible = tail_call_possible_iter(&tc_info, i->block);
  VECTOR_DELETE(tc_info.phis);
  return possible;
}

static bool opt_tail_call_elim(IRFunction *f) {
  bool changed = false;
  DLIST_FOREACH (IRBlock*, b, f->blocks) {
    DLIST_FOREACH (IRInstruction*, i, b->instructions) {
      if (i->type != IR_CALL) { continue; }

      /// An instruction is a tail call iff there are no other instruction
      /// between it and the next return instruction other than branches
      /// and phis.
      if (tail_call_possible(i)) {
        /// The actual tail call optimisation takes place in the code generator.
        i->value.call.tail_call = true;
        ir_mark_unreachable(b);
        changed = true;

        /// We can’t have more than two tail calls in a single block.
        goto next_block;
      }
    }
  next_block:;
  }
  return changed;
}

static bool opt_mem2reg(IRFunction *f) {
  bool changed = false;

  /// A stack variable.
  typedef struct {
    IRInstruction *alloca;
    IRInstruction *store;
    VECTOR(IRInstruction *) loads;
    bool unoptimisable;
  } stack_var;
  VECTOR(stack_var) vars = {0};

  /// Collect all stack variables that are stored into once, and
  /// whose address is never taken.
  DLIST_FOREACH (IRBlock*, b, f->blocks) {
    DLIST_FOREACH (IRInstruction*, i, b->instructions) {
      switch (i->type) {
        /// New variable.
        case IR_STACK_ALLOCATE: {
          stack_var v = {0};
          v.alloca = i;
          VECTOR_PUSH(vars, v);
        } break;

        /// Record the first store into a variable.
        case IR_LOCAL_STORE: {
          VECTOR_FOREACH (stack_var, a, vars) {
            if (!a->unoptimisable && a->alloca == i->value.pair.car) {
              /// If there are multiple stores, mark the variable as unoptimisable.
              if (a->store) a->unoptimisable = true;
              else a->store = i;
              break;
            }
          }
        } break;

        /// Record all loads; also check for loads before the first store.
        case IR_LOCAL_LOAD: {
          VECTOR_FOREACH (stack_var, a, vars) {
            if (!a->unoptimisable && a->alloca == i->value.reference) {
              /// Load before store.
              if (!a->store) {
                a->unoptimisable = true;
                fprintf(stderr, "Warning: Load of uninitialised variable in function %s", f->name);
              } else {
                VECTOR_PUSH(a->loads, i);
              }
              break;
            }
          }
        } break;

        /// If the address of a variable is taken, mark it as unoptimisable.
        case IR_LOCAL_ADDRESS: {
          VECTOR_FOREACH (stack_var, a, vars) {
            if (a->alloca == i->value.reference) {
              a->unoptimisable = true;
              break;
            }
          }
        } break;
      }
    }
  }

  /// Optimise all optimisable variables.
  VECTOR_FOREACH (stack_var, a, vars) {
    if (a->unoptimisable) {
      VECTOR_DELETE(a->loads);
      continue;
    }

    changed = true;

    /// Replace all loads with the stored value.
    VECTOR_FOREACH_PTR (IRInstruction*, i, a->loads) {
      ir_replace_uses(i, a->store->value.pair.cdr);
      ir_remove(i);
    }
    VECTOR_DELETE(a->loads);

    /// Remove the store.
    ASSERT(a->store->users.size <= 1);
    VECTOR_CLEAR(a->store->users);
    ir_remove(a->store);

    /// Remove the alloca.
    ir_remove(a->alloca);
  }

  VECTOR_DELETE(vars);
  return changed;
}

/// Keep track of stores to global variables, and if there is only
/// one, inline the value if it is also global.
void opt_inline_global_vars(CodegenContext *ctx) {
  /// A global variable.
  typedef struct {
    const char* name;
    IRInstruction *store;
    VECTOR(IRInstruction *) loads;
    bool unoptimisable;
  } global_var;
  VECTOR(global_var) vars = {0};

  /// Since loads from global variables before the first store
  /// are possible, we only check if the first store occurs
  /// before any loads and in main() for now.
  IRFunction *main = ir_get_function(ctx, "main");
  ASSERT(main, "No main() function!");

  FOREACH_INSTRUCTION (ctx) {
    switch (instruction->type) {
      /// Record the first store into a variable.
      case IR_GLOBAL_STORE: {
        VECTOR_FOREACH (global_var, a, vars) {
          if (!a->unoptimisable && !strcmp(a->name, instruction->value.global_assignment.name)) {
            /// If there are multiple stores, mark the variable as unoptimisable.
            if (a->store || function != main) a->unoptimisable = true;
            else a->store = instruction;
            goto next_instruction;
          }
        }

        /// Add a new variable.
        global_var v = {0};
        v.name = instruction->value.global_assignment.name;
        v.store = function == main ? instruction : NULL;
        v.unoptimisable = function != main;
        VECTOR_PUSH(vars, v);
      } break;

      /// Record all loads; also check for loads before the first store.
      case IR_GLOBAL_LOAD: {
        VECTOR_FOREACH (global_var, a, vars) {
          if (!a->unoptimisable && !strcmp(a->name, instruction->value.name)) {
            /// Load before store.
            if (!a->store) a->unoptimisable = true;
            else VECTOR_PUSH(a->loads, instruction);
            goto next_instruction;
          }
        }

        /// Unoptimisable because the variable is loaded before it is stored to.
        global_var v = {0};
        v.name = instruction->value.name;
        v.unoptimisable = true;
        VECTOR_PUSH(vars, v);
      } break;
    }
  next_instruction:;
  }

  /// Optimise all optimisable variables.
  VECTOR_FOREACH (global_var, a, vars) {
    if (a->unoptimisable) {
      VECTOR_DELETE(a->loads);
      continue;
    }

    /// Replace all loads with the stored value.
    VECTOR_FOREACH_PTR (IRInstruction*, i, a->loads) {
      ir_replace_uses(i, a->store->value.global_assignment.new_value);
      ir_remove(i);
    }
    VECTOR_DELETE(a->loads);

    /// Remove the store.
    ASSERT(a->store->users.size <= 1);
    VECTOR_CLEAR(a->store->users);
    ir_remove(a->store);
  }

  /// Convert indirect calls to a global address to direct calls.
  FOREACH_INSTRUCTION (ctx) {
    switch (instruction->type) {
      case IR_CALL: {
        if (instruction->value.call.type == IR_CALLTYPE_INDIRECT &&
            instruction->value.call.value.callee->type == IR_GLOBAL_ADDRESS) {
          const char* name = instruction->value.call.value.callee->value.name;
          ir_remove_use(instruction->value.call.value.callee, instruction);

          instruction->value.call.type = IR_CALLTYPE_DIRECT;
          instruction->value.call.value.name = name;
        }
      } break;
    }
  }
}

/// A node in the dominator tree.
typedef struct DomTreeNode {
  IRBlock *block;
  VECTOR(struct DomTreeNode*) dominators;
  VECTOR(struct DomTreeNode*) children;
} DomTreeNode;

/// Structure used to store dominator information such as the
/// dominator tree and the list of dominators of each block.
///
/// The following definitions may be useful in understanding
/// the concept of dominance and dominator trees:
///
///   *dominates*:
/// A block B1 *dominates* another block B2, iff all paths from the
/// entry block to B2 go through B1. That is, when control flow
/// reaches B2, it must have come from B1. By definition, every
/// block dominates itself.
///
///   *strictly dominates*:
/// A block B1 *strictly dominates* another block B2, iff B1 dominates
/// B2 and B1 != B2.
///
///   *immediately dominates*:
/// A block B1 *immediately dominates* another block B2, iff B1 strictly
/// dominates B2 and there is no other block B3 such that B1 strictly
/// dominates B3 and B3 strictly dominates B2. To put it differently, the
/// immediate dominator of a block B2 is the closest block B1 that strictly
/// dominates it, such that there is no other block in between that is
/// strictly dominated by B1 and strictly dominates B2. Every block (except
/// the entry block) has exactly one immediate dominator.
///
///   *dominator tree*:
/// The *dominator tree* of a control flow graph is a tree containing a node
/// for each block in the CFG such that each node’s children are the blocks
/// that it immediately dominates.
///
/// By way of illustration, consider the following CFG:
///
///                   B0
///                  ╱  ╲
///                 B1  B3
///                ╱  ╲ ╱
///               B2  B4
///               |   |
///               B5  B6
///
/// In the graph above,
///    - B0 dominates all blocks since it is the root.
///    - B1 dominates B2 and B5, but *not* e.g. B4 since B4 can
///      also be reached from B3.
///    - B2 dominates B5.
///    - B4 dominates B6.
///
/// The dominator tree for this CFG is:
///
///                    B0
///                 ╱  |  ╲
///                B1  B3  B4
///                |       |
///                B2      B6
///                |
///                B5
typedef struct DominatorInfo {
  /// Nodes that make up the dominator tree. The first
  /// node is the root of the tree and corresponds to
  /// the entry block.
  VECTOR(DomTreeNode) nodes;
  DomTreeNode *dominator_tree;
} DominatorInfo;

typedef VECTOR(IRBlock*) BlockVector;

/// Perform DFS on the control flow graph to find blocks
/// that are reachable from `block`. If `ignore` is encountered,
/// it is ignored. This allows us to effectively ‘remove’ blocks
/// from the CFG without having to actually remove them.
static BlockVector collect_reachable_blocks(IRBlock *block, IRBlock *ignore) {
  BlockVector reachable = {0};
  VECTOR_PUSH(reachable, block);

  /// Stack for DFS.
  VECTOR(IRBlock*) dfs_stack = {0};
  VECTOR_PUSH(dfs_stack, block);
  while (dfs_stack.size) {
    IRBlock *b = VECTOR_POP(dfs_stack);
    if (b == ignore) continue;

    STATIC_ASSERT(IR_COUNT == 28, "Handle all branch types");
    bool out = false;
    IRInstruction *i = b->instructions.last;
    switch (i->type) {
      case IR_BRANCH:
        VECTOR_CONTAINS(reachable, i->value.block, out);
        if (!out) {
          VECTOR_PUSH(reachable, i->value.block);
          VECTOR_PUSH(dfs_stack, i->value.block);
        }
        break;

      case IR_BRANCH_CONDITIONAL:
        VECTOR_CONTAINS(reachable, i->value.conditional_branch.true_branch, out);
        if (!out) {
          VECTOR_PUSH(reachable, i->value.conditional_branch.true_branch);
          VECTOR_PUSH(dfs_stack, i->value.conditional_branch.true_branch);
        }
        VECTOR_CONTAINS(reachable, i->value.conditional_branch.false_branch, out);
        if (!out) {
          VECTOR_PUSH(reachable, i->value.conditional_branch.false_branch);
          VECTOR_PUSH(dfs_stack, i->value.conditional_branch.false_branch);
        }
        break;
    }
  }
  VECTOR_DELETE(dfs_stack);
  return reachable;
}

static void free_dominator_info(DominatorInfo* info) {
  VECTOR_FOREACH (DomTreeNode, n, info->nodes) {
    VECTOR_DELETE(n->dominators);
    VECTOR_DELETE(n->children);
  }
  VECTOR_DELETE(info->nodes);
}

/// Check if a node dominates another node.
static bool dominates(DomTreeNode *dominator, DomTreeNode *node) {
  bool out = false;
  VECTOR_CONTAINS(node->dominators, dominator, out);
  return out;
}

/// Check if a node strictly dominates another node.
static bool strictly_dominates(DomTreeNode *dominator, DomTreeNode *node) {
  return dominator != node && dominates(dominator, node);
}

/// Build the dominator tree for a function and remove unused blocks.
/// \see struct DominatorInfo
static void build_and_prune_dominator_tree(IRFunction *f, DominatorInfo* info) {
  /// Determine all blocks that are reachable from the entry block.
  BlockVector reachable = collect_reachable_blocks(f->blocks.first, NULL);

  /// Remove any unreachable blocks.
  DLIST_FOREACH (IRBlock*, b, f->blocks) {
    bool out = false;
    VECTOR_CONTAINS(reachable, b, out);
    if (!out) ir_remove_and_free_block(b);
  }

  /// We no longer need this.
  VECTOR_DELETE(reachable);

  /// Free old dominator tree.
  VECTOR_FOREACH (DomTreeNode, n, info->nodes) {
    VECTOR_DELETE(n->dominators);
    VECTOR_DELETE(n->children);
  }
  VECTOR_CLEAR(info->nodes);

  /// Add a node for each block.
  ASSERT(f->blocks.first);
  DLIST_FOREACH (IRBlock*, b, f->blocks) {
    DomTreeNode node = {0};
    node.block = b;
    VECTOR_PUSH(info->nodes, node);
  }

  /// The only dominator of the root is the root itself.
  /// We assume that the first block in a function is
  /// the entry block.
  VECTOR_PUSH(info->nodes.data[0].dominators, info->nodes.data);
  info->dominator_tree = info->nodes.data;

  /// To find all dominators of a block, remove that block
  /// from the function; then, find all blocks that are still
  /// reachable from the root. Any unreachable are dominated
  /// by the removed block.
  for (DomTreeNode *dominator = (info->nodes).data + 1;
       dominator < (info->nodes).data + (info->nodes).size;
       dominator++) {
    /// Find all blocks that are still reachable from the root.
    BlockVector still_reachable = collect_reachable_blocks(f->blocks.first, dominator->block);

    /// Find all blocks that are no longer reachable.
    VECTOR_FOREACH (DomTreeNode, d, info->nodes) {
      bool out = false;
      VECTOR_CONTAINS(still_reachable, d->block, out);
      if (!out) {
        /// Add the block to the dominators of the current node.
        VECTOR_PUSH(d->dominators, dominator);

        /// Add the current node to the children of the block.
        /// This is used to build the dominator tree below.
        VECTOR_PUSH(dominator->children, d);
      }
    }

    VECTOR_DELETE(still_reachable);
  }

  /// Now that we know the dominators of each block, we can
  /// build the dominator tree. Currently, the children of each
  /// node contains all nodes that are dominated by that node.
  ///
  /// However, we only want all nodes that are immediately
  /// dominated by that node. The algorithm for this is simple:
  /// For each node N, remove from N’s children any nodes that are
  /// also strictly dominated by another child of N.
  VECTOR (DomTreeNode*) to_remove = {0};
  VECTOR_FOREACH (DomTreeNode, n, info->nodes) {
    VECTOR_CLEAR(to_remove);

    /// For each child of N, check if it is strictly dominated by another child.
    VECTOR_FOREACH_PTR (DomTreeNode*, c, n->children) {
      VECTOR_FOREACH_PTR (DomTreeNode*, c2, n->children) {
        if (strictly_dominates(c, c2)) {
          VECTOR_PUSH(to_remove, c2);
          break;
        }
      }
    }

    /// Remove the nodes.
    VECTOR_FOREACH_PTR (DomTreeNode*, c, to_remove) {
      VECTOR_REMOVE_ELEMENT(n->children, c);
    }
  }
  VECTOR_DELETE(to_remove);
}

/// Rearrange the blocks in a function according to the dominator tree.
static void opt_reorder_blocks(IRFunction *f, DominatorInfo* info) {
  /// Clear the block list.
  f->blocks.first = NULL;
  f->blocks.last = NULL;

  /// Perform a preorder traversal of the dominator tree
  /// and reorder the blocks so that we can avoid jumps.
  VECTOR(DomTreeNode*) stack = {0};
  VECTOR(DomTreeNode*) visited = {0};
  VECTOR_PUSH(stack, info->dominator_tree);
  while (stack.size) {
    DomTreeNode *node = VECTOR_POP(stack);
    DLIST_PUSH_BACK(f->blocks, node->block);

    /// If a block contains a direct branch or a conditional branch,
    /// we want to put the target block at the top of the stack so
    /// that it gets inserted directly after this block.
    IRBlock *next = NULL;
    IRInstruction *last = node->block->instructions.last;
    DomTreeNode *next_node = NULL;
    if (last->type == IR_BRANCH) next = last->value.block;
    else if (last->type == IR_BRANCH_CONDITIONAL) next = last->value.conditional_branch.true_branch;

    /// Insert all children except for the next node.
    VECTOR_FOREACH_PTR (DomTreeNode*, child, node->children) {
      if (child->block == next) {
        next_node = child;
        continue;
      }
      bool out = false;
      VECTOR_CONTAINS(visited, child, out);
      if (!out) VECTOR_PUSH(stack, child);
    }

    /// Insert the next node if there is one.
    if (next_node) {
      bool out = false;
      VECTOR_CONTAINS(visited, next_node, out);
      if (!out) VECTOR_PUSH(stack, next_node);
    }
  }
  VECTOR_DELETE(stack);
}


static bool opt_jump_threading(IRFunction *f, DominatorInfo *info) {
  bool changed = false;

  /// Remove blocks that consist of a single direct branch.
  DLIST_FOREACH (IRBlock*, b, f->blocks) {
    IRInstruction *last = b->instructions.last;
    if (last == b->instructions.first && last->type == IR_BRANCH) {
      /// Update any blocks that branch to this to branch to our
      /// target instead.
      DLIST_FOREACH (IRBlock*, b2, f->blocks) {
        if (b == b2) continue;

        STATIC_ASSERT(IR_COUNT == 28, "Handle all branch instructions");
        IRInstruction *branch = b2->instructions.last;
        if (branch->type == IR_BRANCH && branch->value.block == b) {
          branch->value.block = last->value.block;
          changed = true;
        } else if (branch->type == IR_BRANCH_CONDITIONAL) {
          if (branch->value.conditional_branch.true_branch == b) {
            branch->value.conditional_branch.true_branch = last->value.block;
            changed = true;
          }
          if (branch->value.conditional_branch.false_branch == b) {
            branch->value.conditional_branch.false_branch = last->value.block;
            changed = true;
          }
        }

        /// Also update PHIs.
        DLIST_FOREACH (IRInstruction*, i, b2->instructions) {
          if (i->type == IR_PHI) {
            VECTOR_FOREACH (IRPhiArgument, arg, i->value.phi_arguments) {
              if (arg->block == b) {
                arg->block = last->value.block;
                changed = true;
              }
            }
          }
        }
      }

      ir_remove_and_free_block(b);
      changed = true;
    }
  }

  return changed;
}

/// Mark comparisons as `dont_emit` if they’re immediately
/// used by a conditional branch.
static bool opt_inline_comparisons(IRFunction *f) {
  bool changed = false;
  FOREACH_INSTRUCTION_IN_FUNCTION (f) {
    if (instruction->dont_emit) continue;
    if (instruction->type != IR_COMPARISON) continue;
    if (instruction->next->type != IR_BRANCH_CONDITIONAL) continue;
    if (instruction->users.size != 1 || instruction->users.data[0] != instruction->next) continue;
    instruction->dont_emit = changed = true;
  }
  return changed;
}

static void optimise_function(CodegenContext *ctx, IRFunction *f) {
  DominatorInfo dom = {0};
  do {
    build_and_prune_dominator_tree(f, &dom);
    opt_reorder_blocks(f, &dom);
  } while (
    opt_const_folding_and_strengh_reduction(f) ||
    opt_dce(f) ||
    opt_mem2reg(f) ||
    opt_jump_threading(f, &dom) ||
    opt_tail_call_elim(f) ||
    opt_inline_comparisons(f)
  );
  free_dominator_info(&dom);
}

void codegen_optimise(CodegenContext *ctx) {
  opt_inline_global_vars(ctx);
  VECTOR_FOREACH_PTR (IRFunction*, f, *ctx->functions) {
    optimise_function(ctx, f);
  }
}

/// Called after RA.
void codegen_optimise_blocks(CodegenContext *ctx) {
  VECTOR_FOREACH_PTR (IRFunction*, f, *ctx->functions) {
    DominatorInfo dom = {0};
    do {
      build_and_prune_dominator_tree(f, &dom);
      opt_reorder_blocks(f, &dom);
    } while (opt_jump_threading(f, &dom));
    free_dominator_info(&dom);
  }
}
