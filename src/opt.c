#include <opt.h>
#include <codegen.h>
#include <codegen/intermediate_representation.h>
#include <stdbool.h>
#include <stdint.h>

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

static void optimise_function(CodegenContext *ctx, IRFunction *f) {
  while (opt_fold_constants(ctx, f) ||
         opt_dce(ctx, f));
}

void codegen_optimise(CodegenContext *ctx) {
  FOREACH (IRFunction*, f, ctx->all_functions) {
    optimise_function(ctx, f);
  }
}


