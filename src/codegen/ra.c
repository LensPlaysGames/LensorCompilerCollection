#include <codegen.h>
#include <codegen/ir.h>
#include <codegen/ra.h>
#include <codegen/ir_backend.h>
#include <codegen/x86_64/arch_x86_64.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <vector.h>

#ifdef _MSVC_VER
#include <intrin.h>
#pragma intrinsic(_BitScanForward64)
#endif

#define INITIAL_VALUES_CAPACITY (16)

extern char ra_debug_flag;

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

/// Helper struct to hold references to all values in a function.
typedef VECTOR(Value*) Values;

/// Helper struct to hold references to all basic blocks that have already been visited.
typedef VECTOR(BasicBlock*) ControlFlowIterationContext;

/// Adjacency matrix for the interference graph.
///
/// This matrix indicates which values interfere with one another.
///
/// A value of 1 indicates that the two corresponding values/registers interfere
/// with one another. A value of 0 indicates that they do not interfere.
///
/// For example, assuming there are three values v1--w3, if values v1 and v2, values
/// v2 and w3, interfere, then the matrix looks like this (blank values are never used):
///    v1 v2 w3
/// v1 0
/// v2 1  0
/// w3 0  1  0
typedef struct AdjacencyMatrix {
  char *data;
  size_t rows; /// Also the number of columns.
} AdjacencyMatrix;

/// Adjacency list for the interference graph.
typedef struct AdjacencyList {
  char spill;
  size_t colour;
  size_t spill_offset;
  size_t spill_cost;
  size_t interferences;
  regmask_t interfering_regs;
  VECTOR(size_t) interferences_list;
  VECTOR(size_t) removed_interferences_list;
} AdjacencyList;

typedef struct InterferenceGraph {
  AdjacencyMatrix mtx;
  VECTOR(AdjacencyList) lists;
  VECTOR(size_t) stack;
  size_t num_regs;
} InterferenceGraph;

/// Find the first bit set in a word.
static size_t find_first_set(regmask_t n) {
#ifndef _MSVC_VER
  return __builtin_ffsll(n);
#else
  unsigned long index;
  _BitScanForward64(&index, n);
  return index + 1;
#endif
}

/// Check if a register is a physical register.
char physreg_p(unsigned reg, size_t num_regs) {
  return reg != 0 && reg <= num_regs;
}

/// Do not call this directly. Use `follow_control_flow` instead.
char vfollow_control_flow(ControlFlowIterationContext *ctx, BasicBlock *block, char callback(BasicBlock *block, va_list ap), va_list ap) {
  for (;;) {
    // Check if we have already visited this block.
    VECTOR_FOREACH (bb, ctx) {
      if (*bb == block) { return 0; }
    }

    // Add this block to the list of visited blocks.
    VECTOR_PUSH(ctx, block);

    va_list copy;
    va_copy(copy, ap);
    char result = callback(block, copy);
    if (result) return result;

    // Follow branches.
    if (block->end->type == IR_INSTRUCTION_RETURN) return 0;
    else if (block->end->type == IR_INSTRUCTION_BRANCH) block = block->end->branch_target;
    else if (block->end->type == IR_INSTRUCTION_BRANCH_IF) {
      result = vfollow_control_flow(ctx, block->end->cond_branch_value.true_branch, callback, ap);
      if (result) return result;
      block = block->end->cond_branch_value.false_branch;
    } else UNREACHABLE();
  }
}

/// Follow the control flow graph starting at a block and call a callback for each block.
///
/// Arguments passed after the callback are forwarded to the callback as a va_list.
/// No block is visited more than once. If the callback returns a nonzero value,
/// the iteration stops.
///
/// Returns 0 if the iteration completed, or the return value of the callback
/// if the iteration was stopped.
char follow_control_flow(BasicBlock *block, char callback(BasicBlock *block, va_list ap), ...) {
  ControlFlowIterationContext ctx = {0};
  va_list ap;
  va_start(ap, callback);
  char result = vfollow_control_flow(&ctx, block, callback, ap);
  va_end(ap);
  return result;
}

/// Prepare IR for RA.
///
/// Converts PHIs to copies and collects all values for which we need to allocate
/// registers.
///
/// PHI instructions are special in that no CPU has an instruction that corresponds
/// to a PHI, and yet they are pretty much required for an SSA IR. This also means
/// that we cannot simply assign a physical register to each PHI node and be done
/// with it.
///
/// Instead we convert each PHI node to a series of copy instructions. For instance
/// given the PHI node `phi [bb1, %v1], [bb2, %v2]`, we insert copy instructions at
/// the end of `bb1` and `bb2` that copy `%v1` and `%v2` into the result register
/// of the phi node. We then replace the arguments `%v1` and `%v2` in the PHI node
/// with those copy instructions (only in the PHI node of course; the instructions
/// that generate `%v1` and `%v2` are not modified).
///
/// The reason why we do this here instead of just allocating a register for each
/// PHI node and letting the backend insert the copies is because doing this here
/// means that the register coalescer might be able to optimise away some of the
/// copies.
static Values lower_ir(CodegenContext *ctx, Function *f, size_t num_regs) {
  Values values = {0};

  // Lower PHIs. We need to do this first because it introduces new values.
  VALUE_FOREACH_TYPE (value, block, f, value->type) {
    LIST_FOREACH (entry, value->phi_entries) {
      Value *copy = create_copy(ctx, entry->value);
      copy->phi_arg = 1;
      insert_after(entry->value, copy);

      entry->value = copy;
    }
  }

  // Collect values.
  size_t virt_reg = num_regs + 1;
  VALUE_FOREACH(value, block, f) {
    value->instruction_index = values.count;
    if (needs_register(value)) {
      value->id = values.count;
      VECTOR_PUSH(&values, value);

      // Assign a virtual register to the value for debugging.
      if (!physreg_p(value->reg, num_regs)) {
        value->reg = virt_reg++;
      }
    }
  }

  return values;
}

/// Lambda used by `values_interfere()`. Returns 1 if the two values interfere
/// with one another, 0 if they do not interfere in the current block, and -1
/// if they cannot interfere.
char values_interfere_callback(BasicBlock *block, va_list ap) {
  Value *use_value = va_arg(ap, Value*);
  Value *def_value = va_arg(ap, Value*);

  // def/def_value is the definition of v2
  // use/use_value is the use of v1

  // The use is in this block.
  if (use_value->parent == block) {
    // If the def is in this block, and precedes or is the same as the use,
    // then the use and def interfere.
    if (def_value->parent == block && def_value->instruction_index <= use_value->instruction_index) {
      return 1;
    }

    // If the def is not in this block, then the use and def do not interfere.
    return -1;
  }

  // The def is in this block. Since we haven't seen the use yet, we know that
  // it is somewhere after the def, and therefore, the use and def interfere.
  if (def_value->parent == block) {
    return 1;
  }

  // Neither the use nor the definition is in the current block. Move
  // on to the next block.
  return 0;
}

/// Check if v1 and v2 interfere.
///
/// To fully check if two values interfere, call this function twice, swapping
/// the arguments on the second call.
///
/// This iterates over each use in v1 and check if the definition of v2 lies
/// within the interval between the definition of v1 and the use.
///
/// If the definition and use are in the same basic block, the interval
/// to check comprises all values between and including the definition and use.
///
/// Otherwise, v1 is live
///   - for the rest of the block in which it is defined, and
///   - for the entirety of every block that maybe reached from the definition
///     upto, but not including, the block containing use, and
///   - in all instructions up to an including the use in the block containing
///     the use.
///
/// If the definition of v2 is contained within any of these intervals, v1 and v2
/// interfere. If so, return 1, and 0 otherwise.
/// TODO: If the uses are NULL, then that value doesn't interfere w/ anything.
/// TODO: Fix uses when creating nodes in the backend.
char values_interfere(Value *v1, Value *v2) {
  LIST_FOREACH (use, v1->uses) {
    // The definition and use of v1 are in the same block.
    if (v1->parent == use->parent->parent) {
      // If the definition of v2 is also in that block, and lies between the
      // definition of v1 and its use, then v1 and v2 interfere.
        return v2->parent == v1->parent &&
         v1->instruction_index < v2->instruction_index &&
         v2->instruction_index <= use->parent->instruction_index;
    }

    // The definition and use are in different blocks.
    else {
      // Check if the definition of v2 lies between the definition of v1 and the use.
      // The use of v1 HAS to follow the definition of v1, not necessarily in terms
      // of the instruction index, but certainly in terms of control flow, for which
      // reason we only need to scan forward from the definition of v1 to its use.
      if (follow_control_flow(v1->parent, values_interfere_callback, use->parent, v2) == 1) return 1;
    }
  }
  return 0;
}

/// Get the entry for values (v1, v2) in the adjacency matrix.
char* adji(AdjacencyMatrix *mtx, size_t v1, size_t v2) {
  static char ignored;
  if (v1 == v2) return &ignored;
  if (v1 < v2) return adji(mtx, v2, v1);
  return mtx->data + v1 * mtx->rows + v2;
}

/// Get the entry for values (v1, v2) in the adjacency matrix.
char* adj(AdjacencyMatrix *mtx, Value *v1, Value *v2) {
  return adji(mtx, v1->id, v2->id);
}

/// Compute the adjacency matrix for the interference graph.
void build_adjacency_matrix(AdjacencyMatrix *m, Values *values) {
  memset(m->data, 0, m->rows * m->rows);

  // Add edges between values that interfere.
  VECTOR_FOREACH_PTR (v1, values) {
    VECTOR_FOREACH_PTR (v2, values) {
      if (v1 == v2) { continue; }
      if (v2 > v1)  { break; }
      if (values_interfere(v1, v2) || values_interfere(v2, v1)) {
        printf("Value %zu interferes with value %zu\n", v1->reg, v2->reg);
        *adj(m, v1, v2) = 1;
      }
    }
  }
}

/// Perform register coalescing.
void coalesce_registers(InterferenceGraph *g, Function *f, Values *values) {

}

void build_adjacency_lists
(const CodegenContext *context,
 InterferenceGraph *g,
 Values *values,
 regmask_t platform_interfering_regs(const CodegenContext *context, const Value *value)) {
  VECTOR_FOREACH_INDEX(i, values) {
    VECTOR_FOREACH_INDEX(j, values) {
      if (*adji(&g->mtx, i, j)) {
        g->lists.data[i].interferences++;
        g->lists.data[j].interferences++;
        VECTOR_PUSH(&g->lists.data[i].interferences_list, j);
        VECTOR_PUSH(&g->lists.data[j].interferences_list, i);
      }
    }
  }

  // Determine the registers that interfere with each value.
  VECTOR_FOREACH_PTR(value, values) {
      g->lists.data[value->id].interfering_regs |= platform_interfering_regs(context, value);
  }
}

/// Remove a vertex from the interference graph.
void remove_vertex(InterferenceGraph *g, size_t index) {
  VECTOR_FOREACH(i, &g->lists.data[index].interferences_list) {
    AdjacencyList *list= g->lists.data + *i;
    list->interferences--;
    VECTOR_FOREACH(j, &list->interferences_list) {
      if (list->interferences_list.data[*j] == index) {
        VECTOR_REMOVE_UNORDERED(&list->interferences_list, *j);
        VECTOR_PUSH(&list->removed_interferences_list, index);
        break;
      }
    }
  }

  AdjacencyList *list = &g->lists.data[index];
  list->interferences = 0;
  VECTOR_APPEND(&list->removed_interferences_list, &list->interferences_list);
  VECTOR_CLEAR(&list->interferences_list);
}

/// Perform initial graph colouring.
void prune_interference_graph(InterferenceGraph *g, Values *values) {
  size_t value_count = g->mtx.rows;
  while (value_count) {
    // Apply the degree < k rule: A graph G is k-colourable if for every vertex v
    // in G, deg(v) < k. Furthermore, given a graph G containing a vertex v with
    // deg(v) < k, G is k-colourable, iff G - v is k-colourable.
    //
    // Thus, if a vertex v has deg(v) < k, then we know it can be coloured, and
    // that removing it doesn't affect the rest of the graph, so we can just remove
    // it and push it onto the stack; this means we won't iterate over it again.
    char done = 0;
    do {
      done = 1;
      VECTOR_FOREACH_INDEX (i, values) {
        if (!values->data[i]->allocated && g->lists.data[i].interferences < g->num_regs) {
          done = 0;
          VECTOR_PUSH(&g->stack, i);
          remove_vertex(g, i);
          values->data[i]->allocated = 1;
          value_count--;
        }
      }
    } while (!done && value_count);

    // Apply the degree >= k rule: At this point, the graph is either empty or
    // contains only vertices with deg(v) >= k. Chaitin's algorithm would spill
    // here, but we can do better.
    //
    // Briggs' improvement to Chaitin's algorithm is to select the vertex with the
    // lowest spill cost, but instead of spilling it, it too is pushed onto the stack.
    //
    // This works because a graph G is k-colourable if, not iff, for every vertex
    // v in G, deg(v) < k. This means that even if there is a vertex v in G with
    // deg(v) >= k, there may still be a k-colouring of G, and spilling may not be
    // necessary, but we don't have enough information to determine that at this
    // point, so we optimistically push the vertex onto the stack and defer spilling
    // as much as possible.
    //
    // A classic example of this is the graph below, which is 2-colourable, but
    // Chaitin's algorithm would spill a vertex:
    //
    //       A ------ B
    //       |        |
    //       |        |
    //       C ------ D
    //
    if (value_count) {
      // Determine the node with the minimal spill cost.
      size_t min_cost = -1;
      size_t node_to_spill = 0;
      VECTOR_FOREACH_INDEX (i, values) {
        size_t cost = g->lists.data[i].interferences
            ? g->lists.data[i].spill_cost / g->lists.data[i].interferences
            : 0 /* TODO: Should this be 0? */;
        if (!values->data[i]->allocated && g->lists.data[i].interferences && cost < min_cost) {
          min_cost = cost;
          node_to_spill = i;
          values->data[i]->allocated = 1;
          if (!min_cost) break;
        }
      }

      // Remove it from the graph.
      VECTOR_PUSH(&g->stack, node_to_spill);
      remove_vertex(g, node_to_spill);
      value_count--;
    }
  }
}

/// Return the smallest register that can be assigned to the vertex
/// at the given index. Returns 0 if no register can be assigned.
unsigned min_register(InterferenceGraph *g, size_t index) {
  ASSERT(g->num_regs <= sizeof(regmask_t) * 8, "RA currently does not support more than 64 registers");

  // The register mask is a bitset where each bit represents a register.
  // If the bit is set, then the register is in use.
  regmask_t regmask = g->lists.data[index].interfering_regs;

  VECTOR_FOREACH (i, &g->lists.data[index].interferences_list) {
    unsigned reg = g->lists.data[*i].colour;
    if (reg) regmask |= 1 << (reg - 1);
  }
  VECTOR_FOREACH (i, &g->lists.data[index].removed_interferences_list) {
    unsigned reg = g->lists.data[*i].colour;
    if (reg) regmask |= 1 << (reg - 1);
  }

  // To find the smallest register that can be assigned, we simply
  // find the first bit that is not set in the register mask.
  unsigned first_set = find_first_set(~regmask);
  return first_set > g->num_regs ? 0 : first_set;
}

/// Assign registers to each vertex in the interference graph.
/// Returns 1 if the graph was coloured successfully, 0 otherwise.
char assign_registers(InterferenceGraph *g, Values *values) {
  char coloured = 1;
  do {
    // Pop a vertex from the stack and assign it a register.
    size_t val = VECTOR_POP(&g->stack);
    unsigned reg = min_register(g, val);
    if (reg) {
      g->lists.data[val].colour = reg;
      values->data[val]->reg = reg;

      // If the value is a PHI node, then we need to assign the same register
      // to all of its operands (which are guaranteed to be copies).
      if (values->data[val]->type == IR_INSTRUCTION_PHI) {
        LIST_FOREACH (entry, values->data[val]->phi_entries) {
          entry->value->reg = reg;
        }
      }
    } else {
      // If we couldn't assign a register, then we have to spill the value.
      g->lists.data[val].spill = 1;
      coloured = 0;
    }
  } while (g->stack.count);
  return coloured;
}

/// Print debug info.
void ra_debug_before_allocation(Function *f) {
  printf("============================================\n");
  printf(" Function: %s\n", f->name);
  printf("============================================\n");
  codegen_dump_function(f);
}

void ra_debug(Function *f, InterferenceGraph *g) {
  printf("\nMatrix: rows: %zu\n   ", g->mtx.rows);
  FOR (i, g->mtx.rows) { printf("%%%zu ", i);  }
  printf("\n");

  FOR (i, g->mtx.rows) {
    { printf("%%%zu ", i); }
    for (size_t j = 0; j <= i; j++) {
      printf("%d  ", g->mtx.data[i * g->mtx.rows + j]);
    }
    printf("\n");
  }

  printf("\n");
  codegen_dump_function(f);
}

void allocate_registers(RAInfo *info) {
  ASSERT(info->num_regs, "Need at least one register");

  // Print IR before allocation.
  if (ra_debug_flag) ra_debug_before_allocation(info->function);

  // Convert PHIs to copies.
  Values values = lower_ir(info->context, info->function, info->num_regs);
  if (values.count == 0) { goto free_values; }

  // Create the adjacency matrix for the interference graph.
  InterferenceGraph g = {0};
  g.num_regs = info->num_regs;
  g.mtx.rows = values.count;
  g.mtx.data = calloc(g.mtx.rows * g.mtx.rows, sizeof(char));
  build_adjacency_matrix(&g.mtx, &values);

  // Perform register coalescing.
  coalesce_registers(&g, info->function, &values);

  // Build the adjacency lists for the interference graph.
  VECTOR_RESERVE(&g.lists, g.mtx.rows);
  build_adjacency_lists(info->context, &g, &values, info->platform_interfering_regs);

  // Prune the interference graph.
  prune_interference_graph(&g, &values);

  // Assign registers to the values.
  if (!assign_registers(&g, &values)) { TODO("Spill values"); }

  // Print matrix and IR after allocation.
  if (ra_debug_flag) { ra_debug(info->function, &g); }

  // Free memory.
  free(g.mtx.data);
  VECTOR_DELETE(&g.stack);
  VECTOR_FOREACH (list, &g.lists) {
    VECTOR_DELETE(&list->interferences_list);
    VECTOR_DELETE(&list->removed_interferences_list);
  }
  VECTOR_DELETE(&g.lists);

free_values:
  VECTOR_DELETE(&values);
}