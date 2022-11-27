#include <opt.h>
#include <codegen.h>
#include <codegen/intermediate_representation.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <vector.h>

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

static bool has_side_effects(IRInstruction *i) {
  STATIC_ASSERT(IR_COUNT == 27, "Handle all instructions");
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

static bool opt_fold_constants(CodegenContext *ctx, IRFunction *f) {
  bool changed = false;
  (void) ctx;

  DLIST_FOREACH (IRBlock*, b, f->blocks) {
    DLIST_FOREACH (IRInstruction*, i, b->instructions) {
      switch (i->type) {
        case IR_ADD: IR_REDUCE_BINARY(+) break;
        case IR_SUBTRACT: IR_REDUCE_BINARY(-) break;
        case IR_MULTIPLY: IR_REDUCE_BINARY(*) break;

        /// TODO: Division by 0 should be a compile error.
        case IR_DIVIDE: IR_REDUCE_BINARY(/) break;
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

static bool opt_dce(CodegenContext* ctx, IRFunction *f) {
  bool changed = false;
  (void) ctx;

  DLIST_FOREACH (IRBlock*, b, f->blocks) {
    DLIST_FOREACH (IRInstruction*, i, b->instructions) {
      if (!i->users.size && !has_side_effects(i)) {
        ir_remove(i);
        changed = true;
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

static void opt_tail_call_elim(CodegenContext *ctx, IRFunction *f) {
  (void) ctx;
  DLIST_FOREACH (IRBlock*, b, f->blocks) {
    DLIST_FOREACH (IRInstruction*, i, b->instructions) {
      if (i->type != IR_CALL) { continue; }

      /// An instruction is a tail call iff there are no other instruction
      /// between it and the next return instruction other than branches
      /// and phis.
      if (tail_call_possible(i)) {
        /// The actual tail call optimisation takes place in the code generator.
        i->value.call.tail_call = true;

        /// We can’t have more than two tail calls in a single block.
        goto next_block;
      }
    }
  next_block:;
  }
}

#if 0
static bool opt_merge_blocks(CodegenContext *ctx, IRFunction *f) {
  bool changed = false;
  (void) ctx;

  typedef struct {
    IRBlock *first;
    IRBlock *second;
    bool unmergeable;
  } merge_candidate;
  VECTOR(merge_candidate) candidates = {0};

  /// Compute merge candidates.
  FOREACH (IRBlock*, b1, f->first) {
    FOREACH (IRBlock*, b2, f->first) {
      if (b1 == b2) { continue; }
      if (b1->branch->type == IR_BRANCH && b1->branch->value.block == b2) {
        /// Check if the target block already exists.
        VECTOR_FOREACH (merge_candidate, c, &candidates) {
          if (c->second == b2) {
            c->unmergeable = true;
            goto next;
          }
        }

        merge_candidate c = {b1, b2};
        VECTOR_PUSH(&candidates, c);
      }
    next:;
    }
  }

  /// Merge blocks.
  VECTOR_FOREACH (merge_candidate, c, &candidates) {
    if (c->unmergeable) { continue; }

    /// Move all instructions from the second block to the first.
    FOREACH (IRInstruction*, i, c->second->instructions) {
      insert_instruction_after(i, c->first->last_instruction);
    }

    /// Keep the branch of the second block.
    c->first->branch = c->second->branch;

    /// Remove the second block.
    ir_remove_block(c->second);
    changed = true;
  }
  VECTOR_DELETE(&candidates);
  return changed;
}

static bool opt_jump_threading(CodegenContext *ctx, IRFunction *f) {
  bool changed = false;
  (void) ctx;

  /// Predecessor ‘map’.
  VECTOR(IRBlock *) predecessors = {0};

  /// Compute predecessor map.
  FOREACH (IRBlock*, b, f->first) {
    if (b == f->first || b->instructions) { continue; }

    VECTOR_CLEAR(&predecessors);
    FOREACH (IRBlock*, p, f->first) {
      if (b->branch->type == IR_BRANCH &&
          ((p->branch->type == IR_BRANCH && p->branch->value.block == b) ||
           (p->branch->type == IR_BRANCH_CONDITIONAL &&
            (p->branch->value.conditional_branch.true_branch == b ||
             p->branch->value.conditional_branch.false_branch == b)))) {
        VECTOR_PUSH(&predecessors, p);
      }
    }

    /// Check if we can thread the jump.
    size_t sz = predecessors.size;
    DLIST_FOREACH (IRBlock *, pred, &predecessors) {
      if (pred->branch->type == IR_BRANCH) {
        pred->branch->value.block = b->branch->value.block;
        VECTOR_REMOVE_ELEMENT_UNORDERED(&predecessors, pred);
      } else if (pred->branch->type == IR_BRANCH_CONDITIONAL) {
        if (pred->branch->value.conditional_branch.true_branch == b) {
          pred->branch->value.conditional_branch.true_branch = b->branch->value.block;
          VECTOR_REMOVE_ELEMENT_UNORDERED(&predecessors, pred);
        }
        if (pred->branch->value.conditional_branch.false_branch == b) {
          pred->branch->value.conditional_branch.false_branch = b->branch->value.block;
          VECTOR_REMOVE_ELEMENT_UNORDERED(&predecessors, pred);
        }
      }
    }
    if (sz > 0 && predecessors.size == 0) {
      /// Up
      ir_remove_block(b);
      changed = true;
    }
  }

  VECTOR_DELETE(&predecessors);
  return changed;
}
#endif

static bool opt_mem2reg(CodegenContext *ctx, IRFunction *f) {
  bool changed = false;
  (void) ctx;
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
    ir_remove(a->store);

    /// Remove the alloca.
    ir_remove(a->alloca);
  }

  VECTOR_DELETE(vars);
  return changed;
}

static void optimise_function(CodegenContext *ctx, IRFunction *f) {
  /// Sanity check.

  while (opt_fold_constants(ctx, f) ||
         opt_dce(ctx, f) ||
         opt_mem2reg(ctx, f));
  opt_tail_call_elim(ctx, f);
}

void codegen_optimise(CodegenContext *ctx) {
  VECTOR_FOREACH_PTR (IRFunction*, f, *ctx->functions) {
    optimise_function(ctx, f);
  }
}
