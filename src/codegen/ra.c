#include <codegen.h>
#include <codegen/ir.h>
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

/// A web is a set of overlapping values. Each web is assigned a register.
typedef struct Web {
  Values values;
  char allocated;
  size_t index;
} Web;

typedef VECTOR(Web*) Webs;

/// Adjacency matrix for the interference graph.
///
/// This matrix indicates which webs interfere with one another.
///
/// A value of 1 indicates that the two corresponding webs/registers interfere
/// with one another. A value of 0 indicates that they do not interfere.
///
/// For example, assuming there are three webs w1--w3, if webs w1 and w2, webs
/// w2 and w3, interfere, then the matrix looks like this (blank values are never used):
///    w1 w2 w3
/// w1 0
/// w2 1  0
/// w3 0  1  0
typedef struct AdjacencyMatrix {
  char *data;
  size_t rows; /// Also the number of columns.
  size_t num_regs;
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
} InterferenceGraph;

/// Find the first bit set in a word.
static size_t find_first_set(intmax_t n) {
#ifndef _MSVC_VER
  return __builtin_ffsll(n);
#else
  unsigned long index;
  _BitScanForward64(&index, n);
  return index + 1;
#endif
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

/// We don't need to assign a register for each value. Certain values overlap
/// and can share a register. Two values A and B can share a register iff
///   - A is a PHI node and B is an argument of A, or
///   - B is a PHI node and A is an argument of B, or
///   - A and B are both arguments of the same PHI node.
///   - A and B are both arguments of a COPY instruction.
Webs build_webs(Values *values) {
  Webs webs = {0};

  // For each PHI and COPY instruction, construct a web consisting of the
  // instruction value and its arguments.
  VECTOR_FOREACH (value, values) {
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
            VECTOR_PUSH(&webs, calloc(1, sizeof(Web)));
            phi_web = VECTOR_BACK(&webs);

            VECTOR_RESERVE(&phi_web->values, 2);
            VECTOR_PUSH(&phi_web->values, *value);
            VECTOR_PUSH(&phi_web->values, argument);
          } else {
            // One of the values is in a web; add the other one to it.
            phi_web = phi_web ? phi_web : argument->web;
            VECTOR_PUSH(&phi_web->values, argument->web ? *value : argument);
          }
          (*value)->web = argument->web = phi_web;
        }

        // Both values are in two different webs; merge them
        else if (phi_web != argument->web) {
          VECTOR_RESERVE(&phi_web->values, argument->web->values.count);
          VECTOR_FOREACH (val, &argument->web->values) {
            VECTOR_PUSH(&phi_web->values, *val);
            (*val)->web = phi_web;
          }
          VECTOR_PUSH(&phi_web->values, argument);

          // Remove the merged web from the list of webs.
          Web *old_web = argument->web;
          argument->web = phi_web;
          VECTOR_REMOVE_ELEMENT_UNORDERED(&webs, old_web);
          VECTOR_DELETE(&old_web->values);
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
  VECTOR_FOREACH (value, values) {
    if (!(*value)->web && needs_register(*value) && (*value)->reg == 0) {
      Web *new_web = calloc(1, sizeof *new_web);
      VECTOR_PUSH(&webs, new_web);
      VECTOR_PUSH(&new_web->values, *value);
      (*value)->web = new_web;
    }
  }

  // Set the indices for each web.
  VECTOR_FOREACH_INDEX (i, &webs) { webs.data[i]->index = i; }
  return webs;
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
  for (Use *use = v1->uses; use; use = use->next) {
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

/// Check if two webs interfere. This is the case if any of the values in either
/// web is live at the definition point of any of the values in the other web.
char webs_interfere(Web* w1, Web* w2) {
  // A web never interferes with itself.
  if (w1 == w2) return 0;

  VECTOR_FOREACH (v1, &w1->values) {
    VECTOR_FOREACH (v2, &w2->values) {
      if (values_interfere(*v1, *v2)) return 1;
      if (values_interfere(*v2, *v1)) return 1;
    }
  }

  return 0;
}

/// Get the entry for webs (w1, w2) in the adjacency matrix.
char* adji(AdjacencyMatrix *mtx, size_t w1, size_t w2) {
  static char ignored;
  if (w1 == w2) return &ignored;
  if (w1 < w2) return adji(mtx, w2, w1);
  return mtx->data + w1 * mtx->rows + w2;
}

/// Get the entry for webs (w1, w2) in the adjacency matrix.
char* adj(AdjacencyMatrix *mtx, Web *w1, Web *w2) {
  return adji(mtx, w1->index, w2->index);
}

/// Compute the adjacency matrix for the interference graph.
void build_adjacency_matrix(AdjacencyMatrix *m, Webs *webs) {
  memset(m->data, 0, m->rows * m->rows);

  // Add edges between webs that interfere.
  VECTOR_FOREACH_PTR (w1, webs) {
    VECTOR_FOREACH_PTR (w2, webs) {
      if (w1 == w2) { continue; }
      if (w2 > w1)  { break; }
      if (webs_interfere(w1, w2)) {
        printf("Web %zu interferes with web %zu\n", w1->index, w2->index);
        *adj(m, w1, w2) = 1;
      }
    }
  }
}

/// Perform register coalescing.
void coalesce_registers(InterferenceGraph *g, Web *webs) {
  (void) g;
  (void) webs;
  TODO();
}

void build_adjacency_lists
(InterferenceGraph *g,
 Webs *webs,
 regmask_t platform_interfering_regs(const Value *value)) {
  VECTOR_FOREACH_INDEX(i, webs) {
    VECTOR_FOREACH_INDEX(j, webs) {
      if (*adji(&g->mtx, i, j)) {
        g->lists.data[i].interferences++;
        g->lists.data[j].interferences++;
        VECTOR_PUSH(&g->lists.data[i].interferences_list, j);
        VECTOR_PUSH(&g->lists.data[j].interferences_list, i);
      }
    }
  }

  // Determine the registers that interfere with each web.
  VECTOR_FOREACH_PTR(web, webs) {
    VECTOR_FOREACH_PTR (value, &web->values) {
      g->lists.data[web->index].interfering_regs |= platform_interfering_regs(value);
    }
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
void prune_interference_graph(InterferenceGraph *g, Webs *webs) {
  size_t web_count = g->mtx.rows;
  while (web_count) {
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
      VECTOR_FOREACH_INDEX (i, webs) {
        if (!webs->data[i]->allocated && g->lists.data[i].interferences < g->mtx.num_regs) {
          done = 0;
          VECTOR_PUSH(&g->stack, i);
          remove_vertex(g, i);
          webs->data[i]->allocated = 1;
          web_count--;
        }
      }
    } while (!done && web_count);

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
    if (web_count) {
      // Determine the node with the minimal spill cost.
      size_t min_cost = -1;
      size_t node_to_spill = 0;
      VECTOR_FOREACH_INDEX (i, webs) {
        size_t cost = g->lists.data[i].interferences
            ? g->lists.data[i].spill_cost / g->lists.data[i].interferences
            : 0 /* TODO: Should this be 0? */;
        if (!webs->data[i]->allocated && g->lists.data[i].interferences && cost < min_cost) {
          min_cost = cost;
          node_to_spill = i;
          webs->data[i]->allocated = 1;
          if (!min_cost) break;
        }
      }

      // Remove it from the graph.
      VECTOR_PUSH(&g->stack, node_to_spill);
      remove_vertex(g, node_to_spill);
      web_count--;
    }
  }
}

/// Return the smallest register that can be assigned to the vertex
/// at the given index. Returns 0 if no register can be assigned.
unsigned min_register(InterferenceGraph *g, size_t index) {
  ASSERT(g->mtx.num_regs <= sizeof(regmask_t) * 8, "RA currently does not support more than 64 registers");

  // The register mask is a bitset where each bit represents a register.
  // If the bit is set, then the register is in use.
  regmask_t regmask = g->lists.data[index].interfering_regs;

  VECTOR_FOREACH (i, &g->lists.data[index].interferences_list) {
    unsigned reg = g->lists.data[*i].colour;
    regmask |= 1 << (reg - 1);
  }
  VECTOR_FOREACH (i, &g->lists.data[index].removed_interferences_list) {
    unsigned reg = g->lists.data[*i].colour;
    regmask |= 1 << (reg - 1);
  }

  // To find the smallest register that can be assigned, we simply
  // find the first bit that is not set in the register mask.
  unsigned first_set = find_first_set(~regmask);
  return first_set > g->mtx.num_regs ? 0 : first_set;
}

/// Assign registers to each vertex in the interference graph.
/// Returns 1 if the graph was coloured successfully, 0 otherwise.
char assign_registers(InterferenceGraph *g, Webs *webs) {
  char coloured = 1;
  do {
    // Pop a vertex from the stack and assign it a register.
    size_t wi = VECTOR_POP(&g->stack);
    unsigned reg = min_register(g, wi);
    if (reg) {
      g->lists.data[wi].colour = reg;
      // Assign the register to each value in the web.
      VECTOR_FOREACH_PTR(v, &webs->data[wi]->values) { v->reg = reg; }
    } else {
      // If we couldn't assign a register, then we have to spill the web.
      g->lists.data[wi].spill = 1;
      coloured = 0;
    }
  } while (g->stack.count);
  return coloured;
}

/// Print debug info.
void ra_debug
(CodegenContext *context,
 Function *f,
 Webs *webs,
 InterferenceGraph *g) {
  // Print webs.
  printf("\nWebs: %zu\n", webs->count);
  {
    VECTOR_FOREACH_INDEX (web, webs) {
      printf("Web %zu: ", web);
      VECTOR_FOREACH_PTR (value, &webs->data[web]->values) {
        printf("%%r%zu ", value->reg);
      }
      printf("\n");
    }
  }

  // Print the matrix.
  //    r1 r2 r3 w1 w2
  // r2 1
  // r3 1  1
  // w1 0  0  0
  // w2 0  0  1  1
  // w3 1  0  0  0  1
  printf("\nMatrix: num_regs: %zu, rows: %zu\n   ", g->mtx.num_regs, g->mtx.rows);
  FOR (i, g->mtx.rows) { printf("w%zu ", i);  }
  printf("\n");

  FOR (i, g->mtx.rows) {
    printf("w%zu ", i);

    for (size_t j = 0; j <= i; j++) {
      printf("%d  ", g->mtx.data[i * g->mtx.rows + j]);
    }
    printf("\n");
  }

  codegen_dump_function(context, f);
}

void allocate_registers
(CodegenContext *context,
 Function *f,
 size_t num_regs,
 regmask_t platform_interfering_regs(const Value *value)) {
  ASSERT(num_regs, "Need at least one register");

  // Collect all values that need registers.
  Values values = {0};
  for (BasicBlock *block = f->entry; block; block = block->next) {
    for (Value *value = block->values; value; value = value->next) {
      value->instruction_index = values.count;
      VECTOR_PUSH(&values, value);
    }
  }
  f->value_count = values.count;

  // Special cases.
  if (values.count == 0) { goto free_values; }
  if (values.count == 1) {
    values.data[0]->reg = 1;
    goto free_values;
  }

  // Combine values into webs.
  Webs webs = build_webs(&values);
  size_t matrix_size = webs.count + num_regs - 1;

  // Create the adjacency matrix for the interference graph.
  InterferenceGraph g = {0};
  g.mtx.rows = matrix_size;
  g.mtx.num_regs = num_regs;
  g.mtx.data = calloc(g.mtx.rows * g.mtx.rows, sizeof(char));
  build_adjacency_matrix(&g.mtx, &webs);

  // Perform register coalescing.
  //coalesce_registers(&g.mtx, web, &values);

  // Build the adjacency lists for the interference graph.
  VECTOR_RESERVE(&g.lists, g.mtx.rows);
  build_adjacency_lists(&g, &webs, platform_interfering_regs);

  // Prune the interference graph.
  prune_interference_graph(&g, &webs);

  // Assign registers to the webs.
  if (!assign_registers(&g, &webs)) { TODO("Spill values"); }

  ra_debug(context, f, &webs, &g);

  // Free memory.
  free(g.mtx.data);
  VECTOR_DELETE(&g.stack);
  VECTOR_FOREACH (list, &g.lists) {
    VECTOR_DELETE(&list->interferences_list);
    VECTOR_DELETE(&list->removed_interferences_list);
  }
  VECTOR_DELETE(&g.lists);
  VECTOR_FOREACH_PTR (web, &webs) { VECTOR_DELETE(&web->values); }
  VECTOR_DELETE(&webs);

free_values:
  VECTOR_DELETE(&values);
}