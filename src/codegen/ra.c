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


/// Print debug info.
void ra_debug_before_allocation(Function *f) {
  printf("============================================\n");
  printf(" Function: %s\n", f->name);
  printf("============================================\n");
  codegen_dump_function(f);
}

/// Whether an instruction returns a value.
static char needs_register(Value *value) {
  STATIC_ASSERT(IR_INSTRUCTION_COUNT == 27, "needs_register must exhaustively handle all IR instructions.");
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
  regmask_t *registers;
  size_t dimension;
} AdjacencyMatrix;

/// Adjacency list for the interference graph.
typedef struct AdjacencyList {
  char spill;
  size_t colour;
  size_t spill_offset;
  size_t spill_cost;
  size_t interferences;
  regmask_t interfering_regs;
  Register preferred_reg;
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
char physreg_p(Register reg, size_t num_regs) {
  return reg != 0 && reg <= num_regs;
}

/// Do not call this directly. Use `follow_control_flow` instead.
size_t vfollow_control_flow(ControlFlowIterationContext *ctx, BasicBlock *block, size_t callback(BasicBlock *block, va_list ap), va_list ap) {
  for (;;) {
    // Check if we have already visited this block.
    VECTOR_FOREACH (bb, ctx) {
      if (*bb == block) { return 0; }
    }

    // Add this block to the list of visited blocks.
    VECTOR_PUSH(ctx, block);

    va_list copy;
    va_copy(copy, ap);
    size_t result = callback(block, copy);
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
size_t follow_control_flow(BasicBlock *block, size_t callback(BasicBlock *block, va_list ap), ...) {
  ControlFlowIterationContext ctx = {0};
  va_list ap;
  va_start(ap, callback);
  size_t result = vfollow_control_flow(&ctx, block, callback, ap);
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
/// with it; we also have to insert copies in the right places to make sure that
/// the values involved end up in the right registers.
///
/// The reason why we do this here instead of just allocating a register for each
/// PHI node and letting the backend insert the copies is because doing this here
/// means that the register coalescer might be able to optimise away some of the
/// copies.
///
/// For each entry in a PHI node, we generate a copy instruction. For instance,
/// given the PHI node `phi [bb1, %1], [bb2, %2]`, we insert copy instructions at
/// the end of `bb1` and `bb2` that copy `%1` and `%2` into the result register
/// of the phi node. We then replace the arguments `%1` and `%2` in the PHI node
/// with those copy instructions (only in the PHI node of course; the instructions
/// that generate `%1` and `%2` are not modified).
///
/// If an entry of a PHI node is at a critical edge, we need to insert an extra
/// block to avoid clobbering a register on a different control flow path. A
/// critical edge is an edge from a block with multiple successors to a block
/// with multiple predecessors.
///
/// In the example below, there is a critical edge between bb1 and bb3:
///
///   bb1:
///     %cond = ...
///     %a = ...
///     branch on %cond to bb2 else bb3
///   bb2:
///     ...
///     %b = %c + %d
///     branch to bb3
///   bb3:
///     %phi = phi [bb1, %a], [bb2, %b]
///     ...
///
/// The trivial lowering strategy of inserting a copy instruction at the end of
/// each basic block that branches (or can branch) to the block that contains the
/// PHI node does not work in this case:
///
///   bb1:
///     %cond = ...
///     %a = ...
///     %phi = %a
///     branch on %cond to bb2 else bb3
///   bb2:
///     ...
///     %b = %c + %d
///     %phi = %b
///     branch to bb3
///   bb3:
///     ...
///
/// If we try to lower the PHI on bb3 by inserting a copy (`%phi = %a`) before the
/// branch, that copy will execute irrespective of whether control flow branches
/// to bb2 or bb3, even though the copy is only needed in the latter case.
///
/// Furthermore, the target of the copy cannot not be a register that is used in bb2,
/// since that would clobber that register and thus change the semantics of the
/// program should control flow branch to bb2.
///
/// This effectively means that the liveness range of the copy instruction spans
/// *any* successors of its parent block that can be reached between it and the PHI
/// node.
///
/// This is an unnecessarily large liveness range and, depending on the number of
/// phi nodes in the program, may lead to various registers remaining live for much
/// longer than necessary.
///
/// This problem exists iff there is a critical edge. If the branch in bb1
/// were non-conditional and always branched to bb3, then we could just insert
/// the copy and call it a day.
///
/// However, if there is a critical edge, the usual solution is to insert
/// another block containing the copy instruction(s) between bb1 and bb3 to
/// ensure that they are only executed when control flow branches to bb3:
///
///   bb1:
///     %cond = ...
///     %a = ...
///     branch on %cond to bb1-copy else bb3
///   bb1-copy:
///     %phi = %a
///     branch to bb3
///   bb2:
///     ...
///     %b = %c + %d
///     %phi = b
///     branch to bb3
///   bb3:
///     ...
///
static void lower_phis(CodegenContext *ctx, Function *f) {
  BasicBlock *last_block = NULL;

  // Lower PHIs. We need to do this first because it introduces new values.
  VALUE_FOREACH_TYPE (phi, block, f, IR_INSTRUCTION_PHI) {
    ASSERT(last_block != phi->parent, "RA currently does not support more than one PHI per block");
    LIST_FOREACH (entry, phi->phi_entries) {
      Value *copy = create_copy(ctx, entry->value);
      copy->phi_arg = 1;

      // Insert another block if the entry is at a critical edge.
      Value *branch = entry->block->end;
      if (branch->type == IR_INSTRUCTION_BRANCH_IF) {
        BasicBlock *insert_point = ctx->insert_point;

        // Create the block that holds the copy.
        BasicBlock *bb_copy = codegen_basic_block_create_detached(ctx);
        ctx->insert_point = bb_copy;
        insert(ctx, copy);
        codegen_branch(ctx, phi->parent);
        codegen_basic_block_attach_to(ctx, bb_copy, phi->parent->parent);

        // Change the branch instruction to branch to the copy block.
        if (branch->cond_branch_value.true_branch == phi->parent) {
          branch->cond_branch_value.true_branch = bb_copy;
        } else {
          branch->cond_branch_value.false_branch = bb_copy;
        }

        ctx->insert_point = insert_point;
      }

      // Otherwise, insert the copy in the same block as the predecessor.
      else { insert_after(entry->value, copy); }

      entry->value = copy;
      mark_used_by(copy, phi);
    }
    last_block = phi->parent;
  }
}

/// Ensure that function call arguments are in the right registers.
void fixup_calls(RAInfo *info) {
  VALUE_FOREACH_TYPE (call, block, info->function, IR_INSTRUCTION_CALL) {
    size_t arg_index = 0;
    LIST_FOREACH (arg, call->call_value.args) {
      // Remove the use of the argument in the call.
      Use *prev = NULL;
      LIST_FOREACH (use, arg->value->uses) {
        if (use->parent == call) {
          if (prev) prev->next = use->next;
          else arg->value->uses = use->next;
          free(use);
          break;
        }
        prev = use;
      }

      // Copy the argument into the right register.
      Value *copy = create_copy(info->context, arg->value);
      insert_before(call, copy);
      mark_used_by(copy, call);
      arg->value = copy;

      // Set the target register.
      if (arg_index >= info->num_arg_regs) PANIC("Too many arguments to function");
      copy->reg = info->arg_regs[arg_index++];
    }
  }
}

/// Collect all values that need to be assigned a register.
Values collect_values(Function *f, size_t num_regs) {
  Values values = {0};
  size_t virt_reg = num_regs + 1;

  VALUE_FOREACH(value, block, f) {
    value->instruction_index = values.count;
    if (needs_register(value)) {
      value->id = values.count;
      VECTOR_PUSH(&values, value);

      // Assign a virtual register to the value.
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
size_t values_interfere_callback(BasicBlock *block, va_list ap) {
  // Use of v1.
  Value *use_value = va_arg(ap, Value*);

  // Definition of v2.
  Value *def_value = va_arg(ap, Value*);

  // The use is in this block.
  if (use_value->parent == block) {
    // If the def is in this block, and precedes or is the same as the use,
    // then the use and def interfere.
    if (def_value->parent == block && def_value->instruction_index <= use_value->instruction_index) {
      // def and use interfere if def isn't a copy (see also values_interfere()).
      return def_value->type == IR_INSTRUCTION_COPY ? -1 : 1;
    }

    // If the def is not in this block, then the use and def do not interfere.
    return -1;
  }

  // The def is in this block. Since we haven't seen the use yet, we know that
  // it is somewhere after the def, and therefore, the use and def interfere.
  if (def_value->parent == block) {
    // def and use interfere if def isn't a copy.
    return def_value->type == IR_INSTRUCTION_COPY ? -1 : 1;
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
size_t values_interfere(Value *v1, Value *v2) {
  LIST_FOREACH (use, v1->uses) {
    // Copies aren't considered for the purpose of interference checking since
    // we don't care if the target and source of a copy instruction are assigned
    // the same register. In fact, that would actually be a good thing.
    if (use->parent->type == IR_INSTRUCTION_COPY) continue;

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
  return mtx->data + v1 * mtx->dimension + v2;
}

/// Get the entry for values (v1, v2) in the adjacency matrix.
char* adj(AdjacencyMatrix *mtx, Value *v1, Value *v2) {
  return adji(mtx, v1->id, v2->id);
}

/// Compute the adjacency matrix for the interference graph.
void build_adjacency_matrix(AdjacencyMatrix *m, Values *values) {
  memset(m->data, 0, m->dimension * m->dimension);

  // Add edges between values that interfere.
  VECTOR_FOREACH_PTR (v1, values) {
    VECTOR_FOREACH_PTR (v2, values) {
      if (v1 == v2) { continue; }
      if (v2 > v1)  { break; }
      if (values_interfere(v1, v2) || values_interfere(v2, v1)) {
        printf("Value %u interferes with value %u\n", v1->reg, v2->reg);
        *adj(m, v1, v2) = 1;
      }
    }
  }
}

static void replace_use(Value *value, Use *use, Value *replacement) {
  STATIC_ASSERT(IR_INSTRUCTION_COUNT == 27, "replace_use must exhaustively handle all IR instructions");
  if (use->parent == replacement) return;

  switch (use->parent->type) {
    case IR_INSTRUCTION_ADD:
    case IR_INSTRUCTION_SUB:
    case IR_INSTRUCTION_MUL:
    case IR_INSTRUCTION_DIV:
    case IR_INSTRUCTION_MOD:
    case IR_INSTRUCTION_SHL:
    case IR_INSTRUCTION_SAR:
    case IR_INSTRUCTION_STORE_LOCAL:
    case IR_INSTRUCTION_STORE:
      if (use->parent->lhs == value) { use->parent->lhs = replacement; }
      if (use->parent->rhs == value) { use->parent->rhs = replacement; }
      break;

    case IR_INSTRUCTION_CALL:
      LIST_FOREACH(entry, use->parent->call_value.args) {
        if (entry->value == value) { entry->value = replacement; }
      }
      break;

    case IR_INSTRUCTION_COMPARISON:
      if (use->parent->comparison.lhs == value) { use->parent->comparison.lhs = replacement; }
      if (use->parent->comparison.rhs == value) { use->parent->comparison.rhs = replacement; }
      break;

    case IR_INSTRUCTION_BRANCH_IF:
      if (use->parent->cond_branch_value.condition == value) {
        use->parent->cond_branch_value.condition = replacement;
      }
      break;

    case IR_INSTRUCTION_PHI:
      LIST_FOREACH(entry, use->parent->phi_entries) {
        if (entry->value == value) { entry->value = replacement; }
      }
      break;

    case IR_INSTRUCTION_STORE_GLOBAL:
      if (use->parent->global_store.value == value) {
        use->parent->global_store.value = replacement;
      }
      break;

    case IR_INSTRUCTION_LOCAL_REF:
    case IR_INSTRUCTION_LOCAL_VAL:
      if (use->parent->local_ref == value) { use->parent->local_ref = replacement; }
      break;

    case IR_INSTRUCTION_COPY:
      if (use->parent->operand == value) { use->parent->operand = replacement; }
      break;

    case IR_INSTRUCTION_RETURN:
    case IR_INSTRUCTION_ALLOCA:
    case IR_INSTRUCTION_COMMENT:
    case IR_INSTRUCTION_BRANCH:
    case IR_INSTRUCTION_IMMEDIATE:
    case IR_INSTRUCTION_FUNCTION_REF:
    case IR_INSTRUCTION_GLOBAL_REF:
    case IR_INSTRUCTION_GLOBAL_VAL:
    case IR_INSTRUCTION_PARAM_REF:
    case IR_INSTRUCTION_REGISTER:
    case IR_INSTRUCTION_COUNT:
      UNREACHABLE();
  }
}

/// Perform register coalescing.
void coalesce_registers(InterferenceGraph *g, Function *f, Values *values) {
  // I don't want to have to deal with iterator invalidation when removing
  // instructions, so we store pointers to the instructions to be removed
  // here and then remove them at the end in one fell swoop.
  VECTOR(Value*) values_to_remove = {0};
  size_t last_removed = 0;

  do {
    VECTOR_CLEAR(&values_to_remove);
    last_removed = values_to_remove.count;

    // Iterate over all copy instructions and remove them if possible.
    VALUE_FOREACH_TYPE (v, block, f, IR_INSTRUCTION_COPY) {
      // If the source and target of the copy instruction are the same or if
      // they don't interfere, then we can remove the copy instruction.
      if (v == v->operand ||
          (physreg_p(v->reg, g->num_regs) && v->reg == v->operand->reg) ||
          !*adj(&g->mtx, v, v->operand)) {
        if (v->reg == v->operand->reg) {
          VECTOR_PUSH(&values_to_remove, v);
          continue;
        }
      } else { continue; }

      // Replace all uses of the target with the source. If the register we're
      // copying to is a physical register, then we record that register as the
      // preferred register for the operand.
      //
      // Otherwise, we simply replace the target with the source.
      if (physreg_p(v->reg, g->num_regs)) {
        // If the operand already has a preferred physical register, then we
        // can't remove the copy. Otherwise, set the register as the preferred
        // physical register.
        //
        // Either way, we don't remove the copy since either it will end up being
        // required or its target and source will be the same and the backend will
        // optimise it away.
        if (v->operand->preferred_reg) { continue; }
        v->operand->preferred_reg = v->reg;
      } else {
        LIST_FOREACH (use, v->uses) { replace_use(v, use, v->operand); }
        VECTOR_PUSH(&values_to_remove, v);
      }


      // Fix the adjacency matrix.
      VECTOR_FOREACH_PTR (v2, values) {
        if (v2 == v) { continue; }
        *adj(&g->mtx, v, v2) = *adj(&g->mtx, v->operand, v2);
      }
    }

    // Remove all copy instructions that were marked for removal.
    VECTOR_FOREACH_PTR (v, &values_to_remove) { delete_value(v); }
  } while (values_to_remove.count != last_removed);
  VECTOR_DELETE(&values_to_remove);


  // Rebuild the adjacency matrix.
  *values = collect_values(f, g->num_regs);
  build_adjacency_matrix(&g->mtx, values);
}

/*size_t value_interfering_regs_callback(BasicBlock *block, va_list ap) {
  regmask_t *mask = va_arg(ap, regmask_t*);
  Value *use_value = va_arg(ap, Value*);
  Value *value = va_arg(ap, Value*);

  // The use is in this block. Check all instructions up to the use.
  if (use_value->parent == block) {
    LIST_FOREACH (v, block->values) {
      if (v == use_value) { return 1; }
      if (v->reg == value->reg && v != value) { *mask |= 1 << (v->reg - 1); }
    }
    return 1;
  }

  // Continue until we find the use.
  return 0;
}

/// Determine the registers that interfere with a value.
regmask_t value_interfering_regs(InterferenceGraph *g, Value *v) {
  // If the result register is not a physical register, then, by default, no
  // registers interfere with it.
  if (!physreg_p(v->reg, g->num_regs)) { return 0; }

  // Otherwise, check if the result register is assigned to anywhere else between
  // the definition of the value and its last use. If so, the value interferes
  // with that register.
  regmask_t mask = 0;
  LIST_FOREACH (use, v->uses) {
    follow_control_flow(v->parent, value_interfering_regs_callback, &mask, use->parent, v);
  }
  *//*if (!mask)  {
    codegen_dump_function(v->parent->parent);
    PANIC("Spilling not implemented");
  }*//*
  return mask;
}*/

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
      // g->lists.data[value->id].interfering_regs |= value_interfering_regs(g, value);
      g->lists.data[value->id].preferred_reg = value->preferred_reg;
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
  size_t value_count = g->mtx.dimension;
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
Register min_register(InterferenceGraph *g, size_t index) {
  ASSERT(g->num_regs <= sizeof(regmask_t) * 8, "RA currently does not support more than 64 registers");

  // The register mask is a bitset where each bit represents a register.
  // If the bit is set, then the register is in use.
  regmask_t regmask = g->lists.data[index].interfering_regs;

  VECTOR_FOREACH (i, &g->lists.data[index].interferences_list) {
    Register reg = g->lists.data[*i].colour;
    if (reg) regmask |= 1 << (reg - 1);
  }
  VECTOR_FOREACH (i, &g->lists.data[index].removed_interferences_list) {
    Register reg = g->lists.data[*i].colour;
    if (reg) regmask |= 1 << (reg - 1);
  }

  // If this value has a preferred register, use it if we can.
  if (g->lists.data[index].preferred_reg) {
    Register reg = g->lists.data[index].preferred_reg;
    if ((regmask & (1 << (reg - 1))) == 0) return reg;
  }

  // To find the smallest register that can be assigned, we simply
  // find the first bit that is not set in the register mask.
  Register first_set = find_first_set(~regmask);
  return first_set > g->num_regs ? 0 : first_set;
}

/// Assign registers to each vertex in the interference graph.
/// Returns 1 if the graph was coloured successfully, 0 otherwise.
char assign_registers(InterferenceGraph *g, Values *values) {
  char coloured = 1;
  do {
    // Pop a vertex from the stack and assign it a register.
    size_t val = VECTOR_POP(&g->stack);

    // Don't overwrite precoloured physical registers.
    if (physreg_p(values->data[val]->reg, g->num_regs)) continue;

    Register reg = min_register(g, val);
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


void ra_debug(Function *f, InterferenceGraph *g) {
  printf("\nMatrix: rows: %zu\n   ", g->mtx.dimension);
  FOR (i, g->mtx.dimension) { printf("%%%zu ", i);  }
  printf("\n");

  FOR (i, g->mtx.dimension) {
    { printf("%%%zu ", i); }
    for (size_t j = 0; j <= i; j++) {
      printf("%d  ", g->mtx.data[i * g->mtx.dimension + j]);
    }
    printf("\n");
  }

  printf("\n");
  codegen_dump_function(f);
}

void allocate_registers(RAInfo *info) {
  ASSERT(info->num_regs, "Need at least one register");

  // Sanity checks to make sure we're compiling valid IR.
  typecheck_ir(info->context, info->function);

  // Convert PHIs to copies.
  lower_phis(info->context, info->function);

  // Move call arguments into the right registers.
  fixup_calls(info);

  // Collect values.
  Values values = collect_values(info->function, info->num_regs);
  if (values.count == 0) { goto free_values; }

  // Create the adjacency matrix for the interference graph.
  InterferenceGraph g = {0};
  g.num_regs = info->num_regs;
  g.mtx.dimension = values.count;
  g.mtx.data = calloc(g.mtx.dimension * g.mtx.dimension, sizeof(char));
  g.mtx.registers = calloc(g.mtx.dimension, sizeof(Register));
  build_adjacency_matrix(&g.mtx, &values);

  // Perform register coalescing.
  coalesce_registers(&g, info->function, &values);

  // Build the adjacency lists for the interference graph.
  VECTOR_RESERVE(&g.lists, g.mtx.dimension);
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