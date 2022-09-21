#include <codegen.h>
#include <codegen/ir.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#define INITIAL_VALUES_CAPACITY (16)
#define MIN_VIRT_REG (1024)

enum {
  NODES_INTERFERE = 0,
  NODES_DONT_INTERFERE = 1,
};


/// Whether an instruction returns a value.
static char needs_register(Value *value) {
  switch (value->type) {
    case IR_INSTRUCTION_ALLOCA:
    case IR_INSTRUCTION_COMMENT:
    case IR_INSTRUCTION_BRANCH:
    case IR_INSTRUCTION_BRANCH_IF:
    case IR_INSTRUCTION_RETURN:
    case IR_INSTRUCTION_STORE_GLOBAL:
    case IR_INSTRUCTION_STORE_LOCAL:
    case IR_INSTRUCTION_STORE:
      return 0;
    default: return 1;
  }
}


/// TODO: Optimize this.
/*typedef struct VariableBinding {
  Value* alloca;
  Value* last_def;
} VariableBinding;

typedef struct VariableBindings {
  VariableBinding *bindings;
  size_t capacity;
  size_t size;
} VariableBindings;

static void convert_locals_to_regs(CodegenContext *context, Function *f, VariableBindings *vars) {
  for (BasicBlock *bb = f->entry; bb; bb = bb->next) {
    for (Value *val = bb->values; val; val = val->next) {
      // Add a variable binding.
      if (val->type == IR_INSTRUCTION_ALLOCA) {
        if (vars->size == vars->capacity) {
          vars->capacity *= 2;
          vars->bindings = realloc(vars->bindings, vars->capacity * sizeof(VariableBinding));
        }
        vars->bindings[vars->size++] = (VariableBinding) {
            .alloca = val,
            .last_def = NULL,
        };
      } else if (val->type == IR_INSTRUCTION_STORE_LOCAL) {

      }
    }
  }
}*/

typedef struct ControlFlowIterationContext {
  BasicBlock **blocks_visited;
  size_t blocks_visited_count;
  size_t blocks_visited_capacity;
} ControlFlowIterationContext;

void vfollow_control_flow(CodegenContext *context, ControlFlowIterationContext *ctx, BasicBlock *block, void callback(BasicBlock *block, va_list ap), va_list ap) {
  for (;;) {
    // Check if we have already visited this block.
    for (size_t i = 0; i < ctx->blocks_visited_count; i++) {
      if (ctx->blocks_visited[i] == block) {
        return;
      }
    }

    // Add this block to the list of visited blocks.
    if (ctx->blocks_visited_count == ctx->blocks_visited_capacity) {
      ctx->blocks_visited_capacity *= 2;
      ctx->blocks_visited = realloc(ctx->blocks_visited, ctx->blocks_visited_capacity * sizeof(BasicBlock*));
    }
    ctx->blocks_visited[ctx->blocks_visited_count++] = block;

    va_list copy;
    va_copy(copy, ap);
    callback(block, copy);

    if (block->end->type == IR_INSTRUCTION_RETURN) return;
    else if (block->end->type == IR_INSTRUCTION_BRANCH) block = block->end->branch_target;
    else if (block->end->type == IR_INSTRUCTION_BRANCH_IF) {
      vfollow_control_flow(context, ctx, block->end->cond_branch_value.true_branch, callback, ap);
      vfollow_control_flow(context, ctx, block->end->cond_branch_value.false_branch, callback, ap);
      return;
    } else UNREACHABLE();
  }
}

/// Follow the control flow graph starting at a block and call a callback for each block.
/// Arguments passed after the callback are forwarded to the callback as a va_list.
/// No block is visited more than once.
void follow_control_flow(CodegenContext *context, BasicBlock *block, void callback(BasicBlock *block, va_list ap), ...) {
  ControlFlowIterationContext ctx = {0};
  va_list ap;
  va_start(ap, callback);
  vfollow_control_flow(context, &ctx, block, callback, ap);
  va_end(ap);
}

/// A web is a set of overlapping values. Each web is assigned a register.
typedef struct Web {
  Value **values;
  size_t value_count;
  size_t value_capacity;
  unsigned allocated_register;
  struct Web *next;
  struct Web *prev;
} Web;

void allocate_registers(CodegenContext *context, Function *f, size_t num_regs) {
  ASSERT(num_regs, "Need at least one register");

  // Collect all values that need registers.
  Value **values = calloc(INITIAL_VALUES_CAPACITY, sizeof(Value *));
  size_t values_allocated = INITIAL_VALUES_CAPACITY;
  size_t value_count = 0;
  size_t virt_regs = MIN_VIRT_REG;

  for (BasicBlock *block = f->entry; block; block = block->next) {
    for (Value *value = block->values; value; value = value->next) {
      value->instruction_index = value_count++;
      if (value_count > values_allocated) {
        values_allocated *= 2;
        values = realloc(values, values_allocated * sizeof(Value *));
      }
      values[value->instruction_index] = value;
      if (needs_register(value) && value->virt_reg == 0) {
        value->virt_reg = virt_regs++;
      }
    }
  }
  f->value_count = value_count;

  // Special cases.
  if (value_count == 0) { return; }
  if (value_count == 1) {
    values[0]->virt_reg = 1;
    return;
  }

  // We don't need to assign a register for each value. Certain values overlap
  // and can share a register. Two values A and B can share a register iff
  //     - A is a PHI node and B is an argument of A, or
  //     - B is a PHI node and A is an argument of B, or
  //     - A and B are both arguments of the same PHI node.
  //     - A and B are both arguments of a COPY instruction.
  Web *web = NULL;

  // For each PHI and COPY instruction, construct a web consisting of the
  // instruction value and its arguments.
  for (Value **value = values; value < values + value_count; value++) {
    Web *phi_web = (*value)->web;
    Value* argument = NULL;

    if ((*value)->type == IR_INSTRUCTION_PHI) {
      for (PHINodeEntry *e = (*value)->phi_entries; e; e = e->next) {
        argument = e->value;

        // If either value is not in a web, add it to the other value's web.
      combine:
        if (!phi_web || !argument->web) {
          if (!phi_web && !argument->web) {
            // Neither the phi node nor the argument is in a web. Create a new web.
            phi_web = calloc(1, sizeof *phi_web);
            phi_web->next = web;
            if (web) web->prev = phi_web;
            web = phi_web;

            phi_web->values = calloc(2, sizeof *phi_web->values);
            phi_web->values[0] = *value;
            phi_web->values[1] = argument;
            phi_web->value_count = 2;
            phi_web->value_capacity = 2;
          } else {
            // One of the values is in a web; add the other one to it.
            phi_web = phi_web ? phi_web : argument->web;
            if (phi_web->value_count == phi_web->value_capacity) {
              phi_web->value_capacity *= 2;
              phi_web->values = realloc(phi_web->values, phi_web->value_capacity * sizeof *phi_web->values);
            }
            phi_web->values[phi_web->value_count++] = argument->web ? *value : argument;
          }
          (*value)->web = argument->web = phi_web;
        }

        // Both values are in two different webs; merge them
        else if (phi_web != e->value->web) {
          if (phi_web->value_capacity < phi_web->value_count + e->value->web->value_count) {
            phi_web->value_capacity = (phi_web->value_count + e->value->web->value_count) * 2;
            phi_web->values = realloc(phi_web->values, phi_web->value_capacity * sizeof *phi_web->values);
          }
          for (size_t i = 0; i < e->value->web->value_count; i++) {
            phi_web->values[phi_web->value_count++] = e->value->web->values[i];
            e->value->web->values[i]->web = phi_web;
          }

          // Remove the merged web from the list of webs.
          Web *old_web = e->value->web;
          if (old_web->prev) old_web->prev->next = old_web->next;
          if (old_web->next) old_web->next->prev = old_web->prev;
          if (old_web == web) web = old_web->next;
          free(old_web->values);
          free(old_web);
        }

        // This is possible because of the goto.
        if ((*value)->type == IR_INSTRUCTION_COPY) break;
      }
    } else if ((*value)->type == IR_INSTRUCTION_COPY) {
      argument = (*value)->operand;
      phi_web = (*value)->web;
      goto combine;
    }
  }

  // Now that all PHI nodes have been taken care of, create a web for each value
  // that isn't already in a web.
  for (Value **value = values; value < values + value_count; value++) {
    if (!(*value)->web && needs_register(*value) && (*value)->virt_reg >= MIN_VIRT_REG) {
      Web *new_web = calloc(1, sizeof *new_web);
      new_web->next = web;
      if (web) web->prev = new_web;
      web = new_web;

      new_web->values = calloc(1, sizeof *new_web->values);
      new_web->values[0] = *value;
      new_web->value_count = 1;
      new_web->value_capacity = 1;
      (*value)->web = new_web;
    }
  }

  /// Print webs.
  for (Web *w = web; w; w = w->next) {
    printf("Web: ");
    for (size_t i = 0; i < w->value_count; i++) {
      printf("%%r%zu ", w->values[i]->virt_reg);
    }
    printf("\n");
  }

  for (BasicBlock *block = f->entry; block; block = block->next) {
    printf("bb%zu:\n", block->id);
    for (Value *val = block->values; val; val = val->next) {
      codegen_dump_value(context, val);
      printf("\n");
    }
  }
}