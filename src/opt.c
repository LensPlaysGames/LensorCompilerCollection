#include <opt.h>
#include <codegen.h>
#include <codegen/intermediate_representation.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <vector.h>

#define FOREACH(type, it, list) \
  for (type it = list; it; it = it->next)

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

    case IR_CALL:
    case IR_RETURN:
    case IR_BRANCH:
    case IR_BRANCH_CONDITIONAL:
    case IR_PHI:
    case IR_COPY:
    case IR_LOCAL_STORE:
    case IR_GLOBAL_STORE:
    case IR_STORE:
      return true;
  }
}

static bool opt_fold_constants(CodegenContext *ctx, IRFunction *f) {
  bool changed = false;
  (void) ctx;

  FOREACH (IRBlock*, b, f->first) {
    FOREACH (IRInstruction*, i, b->instructions) {
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

  FOREACH (IRBlock*, b, f->first) {
    FOREACH (IRInstruction*, i, b->instructions) {
      if (!i->uses && !has_side_effects(i)) {
        // Don’t optimise away the return value of a function.
        if (i == i->block->function->return_value) { continue; }

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
/// TODO: This function contains a horrendous amount of code duplication, which
///       could be avoided by actually inserting branches and returns into the IR list.
static bool tail_call_possible(tail_call_info *tc, IRInstruction *i) {
  ASSERT(i);
  FOREACH (IRInstruction*, next, i) {
    if (next->type == IR_PHI) {
      /// If this is a phi node, then the call or a previous phi
      /// must be an argument of the phi.
      FOREACH (IRPhiArgument *, arg, next->value.phi_argument) {
        if (arg->value == tc->call) { goto phi; }
        VECTOR_FOREACH_PTR (IRInstruction *, a, &tc->phis) {
          if (a == arg->value) { goto phi; }
        }
      }
      return false;

    phi:
      VECTOR_PUSH(&tc->phis, next);
      continue;
    }
    if (i->block->branch->type == IR_RETURN) {
      VECTOR_FOREACH_PTR (IRInstruction *, a, &tc->phis) {
        if (a == i->block->function->return_value) { return true; }
      }
      return i->block->function->return_value == tc->call;
    }
    if (i->block->branch->type == IR_BRANCH) {
      return tail_call_possible(tc, i->block->branch->value.block->instructions);
    }
    if (i->block->branch->type == IR_BRANCH_CONDITIONAL) {
      return tail_call_possible(tc, i->block->branch->value.conditional_branch.true_branch->instructions) &&
             tail_call_possible(tc, i->block->branch->value.conditional_branch.false_branch->instructions);
    }
    return false;
  }
  if (i->block->branch->type == IR_RETURN) {
    VECTOR_FOREACH_PTR (IRInstruction *, a, &tc->phis) {
      if (a == i->block->function->return_value) { return true; }
    }
    return i->block->function->return_value == tc->call;
  }
  if (i->block->branch->type == IR_BRANCH) {
    return tail_call_possible(tc, i->block->branch->value.block->instructions);
  }
  if (i->block->branch->type == IR_BRANCH_CONDITIONAL) {
    return tail_call_possible(tc, i->block->branch->value.conditional_branch.true_branch->instructions) &&
           tail_call_possible(tc, i->block->branch->value.conditional_branch.false_branch->instructions);
  }
  UNREACHABLE();
}

static void opt_tail_call_elim(CodegenContext *ctx, IRFunction *f) {
  (void) ctx;
  tail_call_info tc_info = {0};
  FOREACH (IRBlock*, b, f->first) {
    FOREACH (IRInstruction*, i, b->instructions) {
      if (i->type != IR_CALL) { continue; }

      /// An instruction is a tail call iff there are no other instruction
      /// between it and the next return instruction other than branches
      /// and phis.
      tc_info.call = i;
      VECTOR_CLEAR(&tc_info.phis);
      if (tail_call_possible(&tc_info, i->next ? i->next : i->block->branch)) {
        /// The actual tail call optimisation takes place in the code generator.
        i->value.call.tail_call = true;

        /// We can’t have more than two tail calls in a single block.
        goto next_block;
      }
    }
  next_block:;
  }
  VECTOR_DELETE(&tc_info.phis);
}

static void optimise_function(CodegenContext *ctx, IRFunction *f) {
  while (opt_fold_constants(ctx, f) ||
         opt_dce(ctx, f));
  opt_tail_call_elim(ctx, f);
}

void codegen_optimise(CodegenContext *ctx) {
  FOREACH (IRFunction*, f, ctx->all_functions) {
    optimise_function(ctx, f);
  }
}


