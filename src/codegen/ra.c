#include <codegen.h>
#include <codegen/ir.h>
#include <stdlib.h>
#include <string.h>

#define INITIAL_VALUES_CAPACITY (16)

enum {
  NODES_INTERFERE = 0,
  NODES_DONT_INTERFERE = 1,
};

struct LiveRange {
  size_t begin;
  size_t end;
};

struct DefUse {
  size_t block_id;
  size_t value_id;
};

/// Whether an instruction returns a value.
static char needs_register(Value *value) {
  return value->type == IR_INSTRUCTION_ALLOCA       ||
         value->type == IR_INSTRUCTION_COMMENT      ||
         value->type == IR_INSTRUCTION_BRANCH       ||
         value->type == IR_INSTRUCTION_BRANCH_IF    ||
         value->type == IR_INSTRUCTION_RETURN       ||
         value->type == IR_INSTRUCTION_STORE_GLOBAL ||
         value->type == IR_INSTRUCTION_STORE_LOCAL  ||
         value->type == IR_INSTRUCTION_STORE;
}

/// Update the liveness of values contained in a value.
void liveness_check_contained(LiveRange *ranges, Value *value) {
  switch (value->type) {
    case IR_INSTRUCTION_ADD:
    case IR_INSTRUCTION_SUB:
    case IR_INSTRUCTION_MUL:
    case IR_INSTRUCTION_DIV:
    case IR_INSTRUCTION_MOD:
    case IR_INSTRUCTION_SHL:
    case IR_INSTRUCTION_SAR:
      ranges[value->lhs->id].end = value->id;
      ranges[value->rhs->id].end = value->id;
      return;

    case IR_INSTRUCTION_CALL:
      for (Value *arg = value->call_value.args; arg; arg = arg->next) {
        ranges[arg->id].end = value->id;
      }
      return;

    case IR_INSTRUCTION_COMPARISON:
      ranges[value->comparison.lhs->id].end = value->id;
      ranges[value->comparison.rhs->id].end = value->id;
      return;

    case IR_INSTRUCTION_BRANCH_IF:
      ranges[value->cond_branch_value.condition->id].end = value->id;
      return;

    case IR_INSTRUCTION_PHI:
      for (PHINodeEntry *e = value->phi_entries; e; e = e->next) {
        ranges[e->value->id].end = value->id;
      }
      return;

    case IR_INSTRUCTION_STORE_GLOBAL:
      ranges[value->global_store.value->id].end = value->id;
      return;

    case IR_INSTRUCTION_STORE_LOCAL:
      ranges[value->lhs->id].end = value->id;
      return;

    case IR_INSTRUCTION_STORE:
      ranges[value->lhs->id].end = value->id;
      ranges[value->rhs->id].end = value->id;
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

    case IR_INSTRUCTION_COUNT: UNREACHABLE();
  }

  UNREACHABLE();
}


void allocate_registers(CodegenContext *context, Function *f, size_t num_regs) {
  // Build du chains.

}









/*
/// Determine the live range of each value in a function.
static LiveRange* liveness_perform_analysis(CodegenContext *context, Value **values, size_t value_count) {
  // Determine the live range of each value.
  // TODO(Sirraide): Control flow analysis
  LiveRange *ranges = calloc(value_count, sizeof(LiveRange));
  for (Value **value = values; value < values + value_count; value++) {
    ranges[(*value)->id].begin = (*value)->id;
    liveness_check_contained(ranges, *value);
  }
  return ranges;
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


  // Perform live range analysis.
  LiveRange  *live_ranges = liveness_perform_analysis(context, values, value_count);

  // Build the interference graph. The interference graph is represented as an
  // adjacency matrix of all values in the function. The value at index (i, j)
  // indicates whether the values i and j interfere with each other.
  signed char *interference_graph = calloc(value_count * value_count, sizeof(signed char));
  for (size_t i = 0; i < value_count; i++) {
    if (live_ranges[i].end == 0) { continue; }
    for (size_t j = 0; j < value_count; j++) {
      if (i == j) continue;
      // Two values interfere if they are live at the same time.
      if (live_ranges[i].end > live_ranges[j].begin ||
          live_ranges[j].end > live_ranges[i].begin) {
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
      free(live_ranges);
      return;
    }

    TODO("Out of registers! Register spilling is not implemented yet.");
  }
}*/
