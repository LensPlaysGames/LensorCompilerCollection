#include <codegen.h>
#include <codegen/ir.h>
#include <codegen/x86_64/arch_x86_64.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define INITIAL_VALUES_CAPACITY (16)

/// Ensure that there is space for at least (vector->count + elements) many elements.
#define VECTOR_RESERVE(vector, elements)                                                     \
  do {                                                                                       \
    if ((vector)->capacity < (vector)->count + (elements)) {                                 \
      (vector)->capacity += (elements);                                                      \
      (vector)->capacity *= 2;                                                               \
      (vector)->data = realloc((vector)->data, (vector)->capacity * sizeof *(vector)->data); \
    }                                                                                        \
  } while (0)

/// Push an element onto the vector.
#define VECTOR_PUSH(vector, element)               \
  do {                                             \
    VECTOR_RESERVE((vector), 1);                   \
    (vector)->data[(vector)->count++] = (element); \
  } while (0)

/// Pop an element from the vector.
#define VECTOR_POP(vector) ((vector)->data[--(vector)->count])

/// Remove an element from a vector. This may change the order of elements in the vector.
#define VECTOR_REMOVE_UNORDERED(vector, index)                 \
  do {                                                         \
    (vector)->data[index] = (vector)->data[--(vector)->count]; \
  } while (0)

/// Append a vector to another vector
#define VECTOR_APPEND(to, from) \
  do {                          \
    VECTOR_RESERVE((to), (from)->count); \
    memcpy((to)->data + (to)->count, (from)->data, (from)->count * sizeof *(from)->data); \
    (to)->count += (from)->count; \
  } while (0)

/// Remove all elements from a vector.
#define VECTOR_CLEAR(vector) \
  do {                       \
    (vector)->count = 0;     \
  } while (0)

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
    case IR_INSTRUCTION_ADD_TWO_ADDRESS:
    case IR_INSTRUCTION_SUB_TWO_ADDRESS:
    case IR_INSTRUCTION_MUL_TWO_ADDRESS:
    case IR_INSTRUCTION_DIV_ONE_ADDRESS:
    case IR_INSTRUCTION_SHL_TWO_ADDRESS:
    case IR_INSTRUCTION_SAR_TWO_ADDRESS:
    case IR_INSTRUCTION_CMP_TWO_ADDRESS:
      return 0;
    default: return 1;
  }
}

/// Helper struct to hold references to all values in a function.
typedef struct Values {
  Value **data;
  size_t capacity;
  size_t count;
} Values;

typedef struct ControlFlowIterationContext {
  BasicBlock **data;
  size_t count;
  size_t capacity;
} ControlFlowIterationContext;

/// A web is a set of overlapping values. Each web is assigned a register.
typedef struct Web {
  Values values;
  unsigned allocated_register;
  struct Web *next;
  struct Web *prev;
  size_t id;
  char allocated;
} Web;

/// Adjacency matrix for the interference graph.
///
/// This matrix indicates which webs and registers interfere with one another.
/// See below for a description of how registers and webs are mapped to indices
/// in this matrix.
///
/// A value of 1 indicates that the two corresponding webs/registers interfere
/// with one another. A value of 0 indicates that they do not interfere.
///
/// For example, assuming there are three webs w1--w3 and three registers r1--r3;
/// if webs w1 and w2, webs w2 and w3, and register r1 and web w3 interfere, then
/// the matrix looks like this (blank values are never used):
///    r1 r2 r3 w1 w2
/// r2 1
/// r3 1  1
/// w1 0  0  0
/// w2 0  0  0  1
/// w3 1  0  0  0  1
typedef struct AdjacencyMatrix {
  char *data;
  size_t rows; /// Also the number of columns.
  size_t num_regs;
} AdjacencyMatrix;

/// Vector of registers.
typedef struct RegisterVector {
  size_t *data;
  size_t count;
  size_t capacity;
} RegisterVector;

/// Adjacency list for the interference graph.
typedef struct AdjacencyList {
  char spill;
  size_t colour;
  size_t spill_offset;
  size_t spill_cost;
  size_t interferences;
  RegisterVector interferences_list;
  RegisterVector removed_interferences_list;
} AdjacencyList;

typedef struct AdjacencyLists {
  AdjacencyList *data;
  size_t count;
  size_t capacity;
} AdjacencyLists;

typedef struct NodeStack {
  size_t *data;
  size_t count;
  size_t capacity;
} NodeStack;

typedef struct InterferenceGraph {
  AdjacencyLists lists;
  AdjacencyMatrix mtx;
  NodeStack stack;
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
    for (size_t i = 0; i < ctx->count; i++) {
      if (ctx->data[i] == block) {
        return 0;
      }
    }

    // Add this block to the list of visited blocks.
    VECTOR_PUSH(ctx, block);

    va_list copy;
    va_copy(copy, ap);
    char result = callback(block, copy);
    if (result) return result;

    if (block->end->type == IR_INSTRUCTION_RETURN) return 0;
    else if (block->end->type == IR_INSTRUCTION_BRANCH) block = block->end->branch_target;
    else if (block->end->type == IR_INSTRUCTION_BRANCH_IF) {
      result = vfollow_control_flow(ctx, block->end->cond_branch_value.true_branch, callback, ap);
      if (result) return result;

      result = vfollow_control_flow(ctx, block->end->cond_branch_value.false_branch, callback, ap);
      if (result) return result;
      return 0;
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
Web *build_webs(Values *values, size_t num_regs) {
  Web *web = NULL;

  // For each PHI and COPY instruction, construct a web consisting of the
  // instruction value and its arguments.
  for (Value **value = values->data; value < values->data + values->count; value++) {
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

            phi_web->values.data = calloc(2, sizeof *phi_web->values.data);
            phi_web->values.data[0] = *value;
            phi_web->values.data[1] = argument;
            phi_web->values.count = 2;
            phi_web->values.capacity = 2;
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
          for (size_t i = 0; i < argument->web->values.count; i++) {
            phi_web->values.data[phi_web->values.count++] = argument->web->values.data[i];
            argument->web->values.data[i]->web = phi_web;
          }

          // Remove the merged web from the list of webs.
          Web *old_web = argument->web;
          argument->web = phi_web;
          phi_web->values.data[phi_web->values.count++] = argument;
          if (old_web->prev) old_web->prev->next = old_web->next;
          if (old_web->next) old_web->next->prev = old_web->prev;
          if (old_web == web) web = old_web->next;
          free(old_web->values.data);
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
  for (Value **value = values->data; value < values->data + values->count; value++) {
    if (!(*value)->web && needs_register(*value) && (*value)->reg > num_regs) {
      Web *new_web = calloc(1, sizeof *new_web);
      new_web->next = web;
      if (web) web->prev = new_web;
      web = new_web;

      new_web->values.data = calloc(1, sizeof *new_web->values.data);
      new_web->values.data[0] = *value;
      new_web->values.count = 1;
      new_web->values.capacity = 1;
      (*value)->web = new_web;
    }
  }

  return web;
}

/// Helpers to check for interference between webs and registers in the adjacency matrix.
///
/// Example matrix:
///    r1 r2 r3 w1 w2
/// r2 1
/// r3 1  1
/// w1 0  0  0
/// w2 0  0  1  1
/// w3 1  0  0  0  1
///
/// The paragraphs below explain how the index of a pair---(web, web), (web, reg),
/// (reg, web), or (reg, reg)---is calculated. As an example, we show the calculation
/// for (reg3, web2), assuming that there are 3 registers.
///
/// First, given a pair (a, b), if both operands are registers, or both operands
/// are webs, swap a and b if a < b, i.e. the greater of the two is placed first.
/// If one operand is a register and the other is a web, the web is placed first;
/// this is because webs are placed after registers in the matrix.
///
///     (reg3, web2) = (reg3, web2) -> (web2, reg3)
///
/// Next, add the number of registers to each element of the pair that is a web.
/// After this, we no longer care whether the operands are webs or registers.
///
///     (web2, reg3) -> (web2 + REG_COUNT, reg3) = (web2 + 3, reg3) = (5, 3)
///
/// Next, subtract 1 from each element of the pair; we do this because registers
/// start at 1 (0 is used to indicate no register), but the matrix starts at 0.
///
/// Note that in the functions below, we do not subtract 1 from elements that are
/// webs because webs, unlike registers, start at 0 in our implementation.
///
///     (5, 3) -> (4, 2)
///
/// Finally, subtract 1 from the first element of the pair; we do this because the
/// first row of the matrix is missing (see the illustration above), because there
/// is no reason to ever check if a web or register interferes with itself.
///
///     (4, 2) -> (3, 2)
///
/// If you check the matrix above, you will see that (3, 2) is indeed the index
/// for (reg3, web2). The fact that the numbers are the same (we started with
/// (reg3, web2) and arrived at (3, 2)) is just a coincidence. The index for
/// (reg1, web3), for instance, would be (reg1, web3) -> (web3, reg1) -> (6, 1)
/// -> (5, 0) -> (4, 0).
///
char* rr(AdjacencyMatrix *mtx, size_t r1, size_t r2) {
  ASSERT(r1 != r2, "Cannot check for or set interference between a register and itself.");
  if (r1 < r2) return rr(mtx, r2, r1);
  return mtx->data + (r1 - 2) * mtx->rows + (r2 - 1);
}
char* wr(AdjacencyMatrix *mtx, size_t web, size_t reg) {
  return mtx->data + (web + mtx->num_regs - 1) * mtx->rows + (reg - 1);
}
char* rw(AdjacencyMatrix *mtx, size_t reg, size_t web) {
  return wr(mtx, web, reg);
}
char* ww(AdjacencyMatrix *mtx, size_t w1, size_t w2) {
  if (w1 < w2) return ww(mtx, w2, w1);
  return mtx->data + (w1 + mtx->num_regs - 1) * mtx->rows + w2 + mtx->num_regs;
}

/// Check whether a register is a physical register.
char physreg_p(Value* v, size_t num_regs) {
  return v->reg >= 1 && v->reg <= num_regs;
}

/// Access the entry in the adjacency matrix for the given pair of values.
char* adj(AdjacencyMatrix *mtx, Value *a, Value *b) {
  char a_is_physical = physreg_p(a, mtx->num_regs);
  char b_is_physical = physreg_p(b, mtx->num_regs);
  if (a_is_physical || b_is_physical) {
    if (a_is_physical && b_is_physical) return rr(mtx, a->reg, b->reg);
    if (a_is_physical) return rw(mtx, a->reg, b->web->id);
  }
  return ww(mtx, a->web->id, b->web->id);
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

  for (size_t i = 0; i < w1->values.count; i++) {
    for (size_t j = 0; j < w2->values.count; j++) {
      Value *v1 = w1->values.data[i];
      Value *v2 = w2->values.data[j];
      if (values_interfere(v1, v2)) return 1;
      if (values_interfere(v2, v1)) return 1;
    }
  }
  return 0;
}

/// Compute the adjacency matrix for the interference graph.
void build_adjacency_matrix
(AdjacencyMatrix *m,
 Web *web,
 char platform_interfere_p(const Value*, unsigned)) {
  memset(m->data, 0, m->rows * m->rows);

  // Registers always interfere with each other.
  for (size_t i = 1; i <= m->num_regs; i++) {
    for (size_t j = 1; j <= i;  j++) {
      if (i == j) continue;
      *rr(m, i, j) = 1;
    }
  }

  // Add edges between webs and registers that interfere.
  size_t wi = 0;
  for (Web *w = web; w; w = w->next, wi++) {
    size_t vi = 0;
    for (Value **v = w->values.data; v < w->values.data + w->values.count; v++, vi++) {
      for (size_t i = 0; i < m->num_regs; i++) {
        if (platform_interfere_p(*v, i)) {
          printf("Web %zu interferes with register %zu (platform)\n", wi + 1, i);
          /// TODO: Was `(*v)->reg`, but is now `i`. Is this right.
          *wr(m, wi, i) = 1;
        }
      }
    }
  }

  wi = 0;
  // Add edges between webs that interfere.
  for (Web *w = web; w; w = w->next, wi++) {
    size_t vi = 0;
    for (Web *v = web; v; v = v->next, vi++) {
      if (wi == vi) { continue; }
      if (vi > wi) { break; }
      if (webs_interfere(w, v)) {
        printf("Web %zu interferes with web %zu\n", wi + 1, vi + 1);
        *ww(m, wi, vi) = 1;
      }
    }
  }
}

/// Perform register coalescing.
void coalesce_registers(AdjacencyMatrix *mtx, Web *webs, Values *values) {
  /// TODO: Implement.
}

void build_adjacency_lists(InterferenceGraph *g, Web *webs) {
  size_t wi = 0;
  for (Web *w = webs; w; w = w->next, wi++) {
    size_t vi = 0;
    for (Web *v = webs; v && vi < wi; v = v->next, vi++) {
      if (*ww(&g->mtx, wi, vi)) {
        g->lists.data[wi].interferences++;
        g->lists.data[vi].interferences++;
        VECTOR_PUSH(&g->lists.data[wi].interferences_list, vi);
        VECTOR_PUSH(&g->lists.data[vi].interferences_list, wi);
      }
    }
  }
}

/// Remove a vertex from the interference graph.
void remove_vertex(InterferenceGraph *g, size_t index) {
  for (size_t i = 0; i < g->lists.data[index].interferences_list.count; i++) {
    AdjacencyList *list = &g->lists.data[i];
    for (size_t j = 0; j < list->interferences_list.count; j++) {
      if (list->interferences_list.data[j] == index) {
        VECTOR_REMOVE_UNORDERED(&list->interferences_list, j);
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
void prune_interference_graph(InterferenceGraph *g, Web *webs, size_t web_count) {
  while (web_count) {
    // Apply the degree < k rule: A graph G is k-colourable if for every vertex v
    // in G, deg(v) < k. Thus, if a vertex v has deg(v) < k, then we know it can
    // be coloured, so we remove it from the graph and push it onto the stack.
    char done = 0;
    do {
      done = 1;
      size_t wi = 0;
      for (Web *w = webs; w; w = w->next, wi++) {
        if (!w->allocated && g->lists.data[wi].interferences < g->mtx.num_regs) {
          done = 0;
          VECTOR_PUSH(&g->stack, wi);
          remove_vertex(g, wi);
          w->allocated = 1;
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
      size_t wi = 0;
      size_t node_to_spill = 0;
      for (Web *w = webs; w; w = w->next, wi++) {
        size_t cost = g->lists.data[wi].interferences
            ? g->lists.data[wi].spill_cost / g->lists.data[wi].interferences
            : 0 /* TODO: Should this be 0? */;
        if (!w->allocated && g->lists.data[wi].interferences && cost < min_cost) {
          min_cost = cost;
          node_to_spill = wi;
          w->allocated = 1;
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
  ASSERT(g->mtx.num_regs <= sizeof(intmax_t) * 8, "RA currently does not support more than 64 registers");

  // The register mask is a bitset where each bit represents a register.
  // If the bit is set, then the register is in use.
  intmax_t regmask = 0;

  for (size_t i = 0; i < g->lists.data[index].interferences_list.count; i++) {
    unsigned reg = g->lists.data[g->lists.data[index].interferences_list.data[i]].colour;
    regmask &= 1 << (reg - 1);
  }
  for (size_t i = 0; i < g->lists.data[index].removed_interferences_list.count; i++) {
    unsigned reg = g->lists.data[g->lists.data[index].removed_interferences_list.data[i]].colour;
    regmask &= 1 << (reg - 1);
  }

  // To find the smallest register that can be assigned, we simply
  // find the first bit that is not set in the register mask.
  unsigned first_set = find_first_set(~regmask);
  return first_set > g->mtx.num_regs ? 0 : first_set;
}

/// Assign registers to each vertex in the interference graph.
/// Returns 1 if the graph was coloured successfully, 0 otherwise.
char assign_registers(InterferenceGraph *g, Web *webs) {
  char coloured = 1;
  do {
    // Pop a vertex from the stack and assign it a register.
    size_t wi = VECTOR_POP(&g->stack);
    unsigned reg = min_register(g, wi);
    if (reg) {
      g->lists.data[wi].colour = reg;

      Web *web = webs;
      size_t web_id = 0;
      while (web_id < wi) {
        web = web->next;
        web_id++;
      }
      ASSERT(web);

      // Assign the register to each value in the web.
      for (size_t i = 0; i < web->values.count; i++) {
        web->values.data[i]->reg = reg;
        web->allocated_register = reg;
      }
    } else {
      // If we couldn't assign a register, then we have to spill the web.
      g->lists.data[wi].spill = 1;
      coloured = 0;
    }
  } while (g->stack.count);
  return coloured;
}

void allocate_registers
(CodegenContext *context,
 Function *f,
 size_t num_regs,
 char platform_interfere_p(const Value *value, unsigned reg)) {
  ASSERT(num_regs, "Need at least one register");

  // Collect all values that need registers.
  Values values = {0};
  values.data = calloc(INITIAL_VALUES_CAPACITY, sizeof(Value *));
  values.capacity = INITIAL_VALUES_CAPACITY;
  values.count = 0;
  size_t regs = num_regs + 1;

  for (BasicBlock *block = f->entry; block; block = block->next) {
    for (Value *value = block->values; value; value = value->next) {
      value->instruction_index = values.count;
      VECTOR_PUSH(&values, value);
      if (needs_register(value) && value->reg == 0) {
        value->reg = regs++;
      }
    }
  }
  f->value_count = values.count;

  // Special cases.
  if (values.count == 0) { return; }
  if (values.count == 1) {
    values.data[0]->reg = 1;
    return;
  }

  // Combine values into webs.
  Web *web = build_webs(&values, num_regs);
  size_t web_count = 0;
  for (Web *w = web; w; w = w->next) web_count++;
  size_t matrix_size = web_count + num_regs - 1;

/*
  // Print webs.
  printf("\nWebs: %zu\n", web_count);
  {
    size_t web_cnt = 0;
    for (Web *w = web; w; w = w->next) {
      printf("Web %zu: ", ++web_cnt);
      for (size_t i = 0; i < w->values.count; i++) {
        printf("%%r%zu ", w->values.data[i]->reg);
      }
      printf("\n");
    }
  }
*/

  // Create the adjacency matrix for the interference graph.
  InterferenceGraph g = {0};
  g.mtx.rows = matrix_size;
  g.mtx.num_regs = num_regs;
  g.mtx.data = calloc(g.mtx.rows * g.mtx.rows, sizeof(char));
  build_adjacency_matrix(&g.mtx, web, platform_interfere_p);

  // Perform register coalescing.
  coalesce_registers(&g.mtx, web, &values);

  // Build the adjacency lists for the interference graph.
  g.lists.data = calloc(web_count, sizeof(AdjacencyList));
  g.lists.count = web_count;
  g.lists.capacity = web_count;
  build_adjacency_lists(&g, web);

  // Prune the interference graph.
  prune_interference_graph(&g, web, web_count);

  // Assign registers to the webs.
  if (!assign_registers(&g, web)) {
    TODO("Spill values");
  }

  codegen_dump_function(context, f);

/*  // Print the matrix.
  ///    r1 r2 r3 w1 w2
  /// r2 1
  /// r3 1  1
  /// w1 0  0  0
  /// w2 0  0  1  1
  /// w3 1  0  0  0  1
  printf("\nMatrix: num_regs: %zu, rows: %zu\n   ", g.mtx.num_regs, g.mtx.rows);
  for (size_t i = 0; i < g.mtx.rows; i++) {
    if (i + 1 > g.mtx.num_regs) { printf("w%zu ", i - g.mtx.num_regs + 1); }
    else { printf("r%zu ", i + 1); }
  }
  printf("\n");

  for (size_t i = 0; i < g.mtx.rows; i++) {
    if (i + 2 > g.mtx.num_regs) { printf("w%zu ", i - g.mtx.num_regs + 2); }
    else { printf("r%zu ", i + 2); }

    for (size_t j = 0; j < g.mtx.rows; j++) {
      printf("%d  ", g.mtx.data[i * g.mtx.rows + j]);
    }
    printf("\n");
  }*/
}