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

/// Definition-use entry.
typedef struct DUEntry {
  size_t index;
  BasicBlock *block;
  struct DUEntry *next;
} DUEntry;

/// Definition-use chain.
typedef struct DUChain {
  BasicBlock *definition_block;
  Value *definition;
  DUEntry *uses;
} DUChain;

/// Add an entry for value in the current basic block to its du chain.
void du_chains_update(DUChain *chains, BasicBlock *block, Value* value, size_t index) {
  DUChain *chain = chains + value->instruction_index;

  // If there already is an entry for this value in the current block, extend it.
  DUEntry *entry = chain->uses;
  while (entry) {
    if (entry->block == block) {
      entry->index = index;
      return;
    }
    entry = entry->next;
  }

  // Otherwise, create a new entry.
  DUEntry *new_entry = calloc(1, sizeof *new_entry);
  new_entry->index = index;
  new_entry->block = block;
  new_entry->next = chain->uses;
  chain->uses = new_entry;
}

/// Update the liveness of values contained in a value.
void du_chains_add(DUChain *chain, BasicBlock *block, Value *value) {
  switch (value->type) {
    case IR_INSTRUCTION_ADD:
    case IR_INSTRUCTION_SUB:
    case IR_INSTRUCTION_MUL:
    case IR_INSTRUCTION_DIV:
    case IR_INSTRUCTION_MOD:
    case IR_INSTRUCTION_SHL:
    case IR_INSTRUCTION_SAR:
      du_chains_update(chain, block, value->lhs, value->instruction_index);
      du_chains_update(chain, block, value->rhs, value->instruction_index);
      return;

    case IR_INSTRUCTION_CALL:
      for (FunctionCallArg *arg = value->call_value.args; arg; arg = arg->next) {
        du_chains_update(chain, block, arg->value, value->instruction_index);
      }
      if (value->call_value.type == FUNCTION_CALL_TYPE_INTERNAL) {
        du_chains_update(chain, block, value->call_value.callee, value->instruction_index);
      }
      return;

    case IR_INSTRUCTION_COMPARISON:
      du_chains_update(chain, block, value->comparison.lhs, value->instruction_index);
      du_chains_update(chain, block, value->comparison.rhs, value->instruction_index);
      return;

    case IR_INSTRUCTION_BRANCH_IF:
      du_chains_update(chain, block, value->cond_branch_value.condition, value->instruction_index);
      return;

    case IR_INSTRUCTION_PHI:
      for (PHINodeEntry *e = value->phi_entries; e; e = e->next) {
        du_chains_update(chain, block, e->value, value->instruction_index);
      }
      return;

    case IR_INSTRUCTION_STORE_GLOBAL:
      du_chains_update(chain, block, value->global_store.value, value->instruction_index);
      return;

    case IR_INSTRUCTION_STORE_LOCAL:
      du_chains_update(chain, block, value->lhs, value->instruction_index);
      return;

    case IR_INSTRUCTION_STORE:
      du_chains_update(chain, block, value->lhs, value->instruction_index);
      du_chains_update(chain, block, value->rhs, value->instruction_index);
      return;

    case IR_INSTRUCTION_COPY:
      du_chains_update(chain, block, value->operand, value->instruction_index);
      return;

    case IR_INSTRUCTION_ALLOCA:
    case IR_INSTRUCTION_COMMENT:
    case IR_INSTRUCTION_BRANCH:
    case IR_INSTRUCTION_IMMEDIATE:
    case IR_INSTRUCTION_RETURN:
    case IR_INSTRUCTION_FUNCTION_REF:
    case IR_INSTRUCTION_GLOBAL_REF:
    case IR_INSTRUCTION_GLOBAL_VAL:
    case IR_INSTRUCTION_LOCAL_REF:
    case IR_INSTRUCTION_LOCAL_VAL:
    case IR_INSTRUCTION_PARAM_REF:
      return;

    default: TODO("Unhandled value type: %d", value->type);
  }
}

/// Build du chains for all values in a block.
void du_chains_build(BasicBlock *block, va_list ap) {
  DUChain *chains = va_arg(ap, DUChain*);
  for (Value *value = block->values; value; value = value->next) {
    if (needs_register(value) && value->virt_reg != 0) {
      // Add the definition to the chain.
      DUChain *chain = chains + value->instruction_index;
      chain->definition_block = block;
      chain->definition = value;
    }
    // Also update the du chains values that are used by this value.
    du_chains_add(chains, block, value);
  }
}

void allocate_registers(CodegenContext *context, Function *f, size_t num_regs) {
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

  // Build du chains.
  DUChain *chains = calloc(value_count, sizeof *chains);
  follow_control_flow(context, f->entry, du_chains_build, chains);

/*  // Rename registers. While there are two chain that don't overlap,
  // rename the register of the second entry to the register of the first.
  size_t rename = MIN_VIRT_REG;
  for (size_t i = 0; i < value_count; i++) {
    if (values[i]->virt_reg == 0) {
      values[i]->virt_reg = rename++;
    }
  }*/

  /// PRINT IR
  for (BasicBlock *block = f->entry; block; block = block->next) {
    printf("bb%zu:\n", block->id);
    for (Value *val = block->values; val; val = val->next) {
      codegen_dump_value(context, val);

      size_t l[128];
      size_t l_count = 0;
      for (size_t i = 0; i < value_count; i++) {
        for (DUEntry *entry = chains[i].uses; entry; entry = entry->next) {
          if (entry->index == val->instruction_index && values[i]->virt_reg) {
            for (size_t j = 0; j < l_count; j++) {
              if (l[j] == values[i]->virt_reg) goto next;
            }
            l[l_count++] = values[i]->virt_reg;
          }
        next:;
        }
      }

      if (l_count) {
        printf("\033[55G{ ");
        for (size_t i = 0; i < l_count; i++) {
          printf("%%r%zu", l[i]);
          if (i < l_count - 1) {
            printf(", ");
          }
        }
        printf(" }\n");
      } else printf("\n");
    }
  }
}

/*
/// Determine the live entry of each value in a function.
static DUChain liveness_perform_analysis(CodegenContext *context, Value **values, size_t value_count) {
  // Determine the live entry of each value.
  // TODO(Sirraide): Control flow analysis
  DUChain *chain = calloc(value_count, sizeof(LiveRange));
  for (Value **value = values; value < values + value_count; value++) {
    chain[(*value)->id].begin = (*value)->id;
    du_chains_add(entrys, *value);
  }
  return entrys;
}

/// Perform liveness analysis and register allocation.
static void allocate_registers(CodegenContext *context, Function *f, size_t num_regs) {
  // Collect all values that need registers.
  Value **values = calloc(INITIAL_VALUES_CAPACITY, sizeof(Value*));
  size_t values_allocated = INITIAL_VALUES_CAPACITY;
  size_t value_count = 0;

  for (BasicBlock *block = f->entry; block; block = block->next) {
    for (Value *value = block->values; value; value = value->next_in_block) {
      if (needs_register(value)) {
        value->id = value_count++;
        if (value_count > values_allocated) {
          values_allocated *= 2;
          values = realloc(values, values_allocated * sizeof(Value*));
        }
        values[value->id] = value;
      }
    }
  }
  f->value_count = value_count;


  // Perform live entry analysis.
  LiveRange  *live_entrys = liveness_perform_analysis(context, values, value_count);

  // Build the interference graph. The interference graph is represented as an
  // adjacency matrix of all values in the function. The value at index (i, j)
  // indicates whether the values i and j interfere with each other.
  signed char *interference_graph = calloc(value_count * value_count, sizeof(signed char));
  for (size_t i = 0; i < value_count; i++) {
    if (live_entrys[i].end == 0) { continue; }
    for (size_t j = 0; j < value_count; j++) {
      if (i == j) continue;
      // Two values interfere if they are live at the same time.
      if (live_entrys[i].end > live_entrys[j].begin ||
          live_entrys[j].end > live_entrys[i].begin) {
        interference_graph[i * value_count + j] = NODES_INTERFERE;
      }
    }
  }

  // Assign registers to values.
  for (;;) {
    size_t maybe_spill = -1;
    size_t maybe_spill_interference_count = 0;
    for (size_t i = 0; i < value_count; i++) {
      // If the value is already coloured, skip it.
      if (values[i]->arch_emitted_value > -1) continue;

      // TODO(Sirraide): SIMD?
      size_t interference_count = 0;
      for (size_t j = 0; j < value_count; j++) {
        if (i == j) continue;
        if (interference_graph[i * value_count + j] == NODES_INTERFERE) { interference_count++; }
      }

      // This value interferes with too many other values to be assigned a register.
      // We may need to spill it later (see below).
      if (interference_count >= num_regs) {
        // Only spill the value that interferes with the most other values.
        if (interference_count > maybe_spill_interference_count) {
          maybe_spill = i;
          maybe_spill_interference_count = interference_count;
        }
        continue;
      }

      // Colour the node and remove it from the graph.
      values[i]->arch_emitted_value = num_regs - 1 - interference_count;
      for (size_t j = 0; j < value_count; j++) {
        if (i == j) continue;
        interference_graph[i * value_count + j] = NODES_DONT_INTERFERE;
        interference_graph[j * value_count + i] = NODES_DONT_INTERFERE;
      }
    }

    // If the interference graph is empty, we're done.
    if (maybe_spill == -1) {
      free(interference_graph);
      free(values);
      free(live_entrys);
      return;
    }

    TODO("Out of registers! Register spilling is not implemented yet.");
  }
}*/
