#include <codegen.h>
#include <codegen/dom.h>
#include <codegen/codegen_forward.h>
#include <codegen/intermediate_representation.h>
#include <codegen/machine_ir.h>
#include <codegen/register_allocation.h>
#include <error.h>
#include <opt.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <vector.h>

//#define DEBUG_RA

#ifdef DEBUG_RA
#  define IR_FEMIT_INST(file, inst) ir_femit_instruction(file, inst)
#  define IR_FEMIT_BLOCK(file, bb) ir_femit_block(file, bb)
#  define IR_FEMIT(file, f) ir_femit_function(file, f)
#  define MIR_PRINT_INST(inst) print_mir_instruction(inst)
#  define MIR_PRINT_BLOCK(bb) print_mir_block(bb)
#  define MIR_PRINT(f) print_mir_function(f)
#  define DEBUG(...) print(__VA_ARGS__)
CodegenContext *debug_context = NULL;
#else
#  define IR_FEMIT_INST(file, inst)
#  define IR_FEMIT_BLOCK(file, bb)
#  define IR_FEMIT(file, context)
#  define MIR_PRINT_INST(inst)
#  define MIR_PRINT_BLOCK(bb)
#  define MIR_PRINT(f)
#  define DEBUG(...)
#endif

/// Used all over the place.
typedef Vector(IRBlock *) BlockVector;

typedef Vector(usz) VRegVector;

/// Return non-zero iff given instruction needs a register.
bool needs_register(IRInstruction *instruction) {
  STATIC_ASSERT(IR_COUNT == 38, "Exhaustively handle all instruction types");
  ASSERT(instruction);
  switch (instruction->kind) {
    case IR_LOAD:
    case IR_PHI:
    case IR_COPY:
    case IR_IMMEDIATE:
    case IR_CALL:
    case IR_REGISTER:
    case IR_NOT:
    case IR_ZERO_EXTEND:
    case IR_SIGN_EXTEND:
    case IR_TRUNCATE:
    case IR_BITCAST:
    ALL_BINARY_INSTRUCTION_CASES()
      return true;

    case IR_PARAMETER:
      ICE("Unlowered parameter instruction in register allocator");

    /// Allocas and static refs need a register iff they are actually used.
    case IR_ALLOCA:
    case IR_STATIC_REF:
    case IR_FUNC_REF:
      return instruction->users.size;

    default:
      return false;
  }
}

//==== BEG INSTRUCTION LIST ====

typedef Vector(IRInstruction *) IRInstructions;

/// If needs_register is non-zero, return only instructions that need a register allocated.
void collect_instructions_into(IRFunction *f, IRInstructions *instructions, char needs_register_filter) {
  vector_clear(*instructions);
  FOREACH_INSTRUCTION_IN_FUNCTION(f) {
    // Add instruction to flat list iff instruction needs register
    // allocated.
    if (!needs_register_filter || needs_register(instruction)) {
      instruction->index = (u32) instructions->size;
      vector_push(*instructions, instruction);
    }
  }
}

IRInstructions collect_instructions(IRFunction *f, char needs_register_filter) {
  IRInstructions instructions = {0};
  collect_instructions_into(f, &instructions, needs_register_filter);
  return instructions;
}

void print_instruction_list(IRInstructions *list) {
  foreach_ptr (IRInstruction *, instruction, *list) {
    ir_femit_instruction(stdout, instruction);
  }
}

#ifdef DEBUG_RA
#  define PRINT_INSTRUCTION_LIST(list) print_instruction_list(list)
#else
#  define PRINT_INSTRUCTION_LIST(list)
#endif

//==== END INSTRUCTION LIST ====

typedef struct IRBlockList {
  IRBlock *block;
  struct IRBlockList *next;
} IRBlockList;

//==== BEG ADJACENCY MATRIX ====

typedef struct AdjacencyMatrix {
  usz size;
  bool *data;
} AdjacencyMatrix;

bool *adjm_entry(AdjacencyMatrix m, usz x, usz y) {
  if (x > m.size) {
    ICE("Can not access adjacency matrix because X is out of bounds.");
  }
  if (y > m.size) {
    ICE("Can not access adjacency matrix because Y is out of bounds.");
  }
  // Coordinate translation for lower left triangle matrix
  if (y > x) {
    usz old_x = x;
    x = y;
    y = old_x;
  }
  return &m.data[y * m.size + x];
}

void adjm_set(AdjacencyMatrix m, usz x, usz y) {
  *adjm_entry(m, x, y) = 1;
}

void adjm_clear(AdjacencyMatrix m, usz x, usz y) {
  *adjm_entry(m, x, y) = 0;
}

bool adjm(AdjacencyMatrix m, usz x, usz y) {
  return *adjm_entry(m, x, y);
}

typedef struct AdjacencyList AdjacencyList;
typedef Vector(AdjacencyList*) AdjacencyLists;

typedef struct AdjacencyGraph {
  usz order;
  AdjacencyMatrix matrix;
  usz *regmasks;
  AdjacencyLists lists;
} AdjacencyGraph;

void allocate_adjacency_graph(AdjacencyGraph *G, usz size) {
  if (G->matrix.data) { free(G->matrix.data); }
  if (G->regmasks) { free(G->regmasks); }
  G->matrix.size = size;
  G->matrix.data = calloc(1, size * size + 1);
  G->regmasks = calloc(1, size * sizeof(usz));
}

/// Callback called by `ir_for_each_child()` in `build_adjacency_graph()`.
static void collect_interferences(IRInstruction *inst, IRInstruction **child, void* data) {
  (void) inst;
  IRInstructions *live_vals = data;
  if (needs_register(*child)) {
    if (!vector_contains(*live_vals, *child)) vector_push(*live_vals, *child);
  }
}

/// Walk over all possible paths in the control flow graph upwards from
/// given block, computing instruction interferences based on the values
/// that are currently live.
static void collect_interferences_from_block
(const MachineDescription *desc,
 MIRBlock *b,
 VRegVector *live_vals,
 MIRBlockVector *visited,
 MIRBlockVector *doubly_visited,
 AdjacencyGraph *G
 )
{
  /// Don't visit the same block twice.
  if (vector_contains(*visited, b)) {
    if (vector_contains(*doubly_visited, b))
      return;
    else vector_push(*doubly_visited, b);
  } else vector_push(*visited, b);


  DEBUG("  from block...\n");

  // TODO: Collect interferences for virtual registers in this block.
  // Basically, walk over the instructions of the block backwards, keeping
  // track of the latest (earliest when not backwards) occurence of any
  // virtual register operand.

  /* TODO: This is important, it needs reenabled
  /// Collect interferences for instructions in this block.
  foreach_ptr_rev (MIRInstruction*, inst, b->instructions) {
    /// Make this value interfere with all values that are live at this point.

    usz mask = desc->instruction_register_interference(inst);
    foreach (usz, live_val, *live_vals) {

      if (needs_register(inst))
        adjm_set(G->matrix, inst->index, live_val->index);

      /// Also take special interferences into account.
      G->regmasks[live_val->index] |= mask;
    }

    /// Remove its result from the set of live variables;
    vector_remove_element_unordered(*live_vals, inst);

    /// Add its operands to the set of live variables.
    ir_for_each_child(inst, collect_interferences, live_vals);
  }
  */

  // The entry block has no parents.
  if (b == b->function->blocks.data[0])
    return;

#ifdef DEBUG_RA
  foreach_ptr (MIRBlock*, parent, b->predecessors) {
    print("  Parent block:\n");
    ir_femit_block(stdout, parent);
  }
#endif

  foreach_ptr (MIRBlock*, parent, b->predecessors) {
    // Copy live vals
    VRegVector live_vals_copy = {0};
    foreach (usz, lv, *live_vals)
      vector_push(live_vals_copy, *lv);

    collect_interferences_from_block(desc, parent, &live_vals_copy, visited, doubly_visited, G);

    vector_delete(live_vals_copy);
  }
}

/// For each exit block `b` in given function, collect interferences
/// from exit to entrance. While doing so, the AdjacencyGraph G (the
/// matrix and regmasks, specifically) is updated to reflect interferences.
///
/// "exit block": a block that ends with an instruction that exits the
/// function (i.e. return or unreachable).
static void collect_interferences_for_function
(const MachineDescription *desc,
 MIRFunction *function,
 AdjacencyGraph *G
 )
{
  // Collect all blocks that end with `ret` or `unreachable`.
  MIRBlockVector exits = {0};
  // TODO: Use instruction_is_exit(opcode) (make part of
  // MachineDescription) instead of explicitly checking opcode.
  foreach_ptr (MIRBlock*, b, function->blocks) {
    if (b->is_exit) vector_push(exits, b);
  }

#ifdef DEBUG_RA
  foreach_ptr (MIRBlock*, b, exits) {
    print("Exit block:\n");
    print_mir_block(b);
  }
#endif

  VRegVector live_vals = {0};
  MIRBlockVector visited = {0};
  MIRBlockVector doubly_visited = {0};

  // From each exit block (collected above), follow control flow to the
  // root of the function (entry block), or to a block already visited.
  foreach_ptr (MIRBlock*, b, exits) {
    vector_clear(live_vals);
    vector_clear(visited);
    vector_clear(doubly_visited);
    collect_interferences_from_block(desc, b, &live_vals, &visited, &doubly_visited, G);
  }

  vector_delete(doubly_visited);
  vector_delete(visited);
  vector_delete(live_vals);
  vector_delete(exits);
}

/// Build the adjacency graph for the given function.
static void build_adjacency_graph(MIRFunction *f, const MachineDescription *desc, VRegVector *registers, AdjacencyGraph *G) {
  ASSERT(f, "Can not build adjacency matrix of NULL MIR function.");
  ASSERT(registers, "Can not build adjacency matrix of NULL register list.");
  ASSERT(registers, "Can not build adjacency matrix of NULL adjacency graph.");

  allocate_adjacency_graph(G, registers->size);

  /*
  /// Build the dominator tree.
  DominatorInfo dom_info = {0};
  build_dominator_tree(f, &dom_info, false);

  /// Collect the leaves of the dominator tree.
  Vector(DomTreeNode*) leaves = {0};
  foreach (DomTreeNode, node, dom_info.nodes) {
    if (node->children.size == 0) {
      vector_push(leaves, node);
    }
  }

  /// Collect the interferences for each leaf.
  foreach_ptr (DomTreeNode*, node, leaves) {
    vector_clear(live_vals);
    vector_clear(visited);
    collect_interferences_for_node(desc, node, &live_vals, &visited, G);
  }

  /// Free dominator and liveness info.
  free_dominator_info(&dom_info);
  vector_delete(leaves);
  */

  /// Collect the interferences from CFG
  collect_interferences_for_function(desc, f, G);

  /* TODO: Reenable?
  /// While were at it, also check for interferences with physical registers.
  /// For that, we need to iterate over all values that interfere with A and
  /// collect the physical registers that are clobbered by or precoloured to
  /// those values.
  foreach_ptr (IRInstruction *, A, *instructions) {
    usz regmask = 0;
    foreach_ptr (IRInstruction *, B, *instructions) {
      if (A == B) { break; } /// (!)
      if (!adjm(G->matrix, A->index, B->index)) { continue; }
      regmask |= desc->instruction_register_interference(B);
      if (B->result) regmask |= 1 << (B->result - 1);
    }
    G->regmasks[A->index] |= regmask;
    // Mask RCX in both sides of bitwise shift binary operator.
    // FIXME: This is a horrid fix, we should probably deal with this
    // in some generic way.
    if (A->parent_block->function->context->format == CG_FMT_x86_64_GAS
    && (A->kind == IR_SHL || A->kind == IR_SHR || A->kind == IR_SAR)) {
      usz regmask_rcx = 1 << (2 - 1);
      G->regmasks[A->lhs->index] |= regmask_rcx;
      G->regmasks[A->rhs->index] |= regmask_rcx;
    }
  }
  */
}

void print_adjacency_matrix(AdjacencyMatrix m) {
  for (usz y = 0; y < m.size; ++y) {
    printf("%6zu|", y);
    for (usz x = 0; x < y; ++x) {
      bool adj = adjm(m, x, y);
      adj ? printf("%4u", adj) : printf("    ");
    }
    printf("\n");
  }
  printf("      |");
  for (usz x = 0; x < m.size; ++x) {
    printf("%4zu", x);
  }
  printf("\n\n");
}

#ifdef DEBUG_RA
#  define PRINT_ADJACENCY_MATRIX(m) print_adjacency_matrix(m)
#else
#  define PRINT_ADJACENCY_MATRIX(m)
#endif

//==== END ADJACENCY MATRIX ====

//==== BEG ADJACENCY LISTS ====

typedef struct AdjacencyList {
  // Degree refers to how many adjacencies this vertex/node has.
  usz degree;

  Vector(usz) adjacencies;

  IRInstruction *instruction;

  // Unique integer index.
  usz index;

  usz regmask;

  Register color;

  char allocated;

  // Spill handling.
  char spill_flag;
  usz spill_offset;
  usz spill_cost;
} AdjacencyList;

void build_adjacency_lists(IRInstructions *instructions, AdjacencyGraph *G) {
  /// Free old lists. This could be more efficient, but we’ve
  /// had bugs where we were trying to use out-of-date lists,
  /// so we’re keeping this for now.
  foreach_ptr (AdjacencyList*, list, G->lists) {
    if (list) vector_delete(list->adjacencies);
    free(list);
  }
  vector_delete(G->lists);

  /// Allocate memory for the new lists.
  vector_reserve(G->lists, instructions->size);
  G->lists.size = instructions->size;

  foreach_ptr (IRInstruction *, i, *instructions) {
    if (G->lists.data[i->index]) vector_delete(G->lists.data[i->index]->adjacencies);
    free(G->lists.data[i->index]);
    AdjacencyList *list = calloc(1, sizeof(AdjacencyList));
    G->lists.data[i->index] = list;
    list->index = i->index;
    list->color = i->result;
    list->instruction = i;
    list->regmask = G->regmasks[i->index];
  }

  foreach_ptr (IRInstruction *, A, *instructions) {
    foreach_ptr (IRInstruction *, B, *instructions) {
      if (A == B) { break; } /// (!)
      if (adjm(G->matrix, A->index, B->index)) {
        AdjacencyList *a_list = G->lists.data[A->index];
        AdjacencyList *b_list = G->lists.data[B->index];
        vector_push(a_list->adjacencies, B->index);
        vector_push(b_list->adjacencies, A->index);
      }
    }
  }
}

void print_adjacency_lists(AdjacencyLists *array) {
  print("[RA]: Adjacency lists\n"
        "   id    idx\n");
  foreach_ptr (AdjacencyList*, list, *array) {
    printf("%6u::%4u: ", list->instruction->id, list->instruction->index);
    if (list->color) print("(r%u) ", list->color);

    /// Print the adjacent nodes.
    foreach_index(i, list->adjacencies) {
      if (i) { print(", "); }
      print("%Z", list->adjacencies.data[i]);
    }
    print("\n");
  }
  print("\n");
}

#ifdef DEBUG_RA
#  define PRINT_ADJACENCY_LISTS(arr) print_adjacency_lists(arr)
#else
#  define PRINT_ADJACENCY_LISTS(arr)
#endif

//==== END ADJACENCY LISTS ====

/// Determine whether an instruction interferes with a register.
bool check_register_interference(usz regmask, IRInstruction *instruction) {
  return (regmask & (1 << (instruction->result - 1))) != 0;
}

/*
/// The aim of register coalescing is to eliminate register-to-register
/// copies by merging the source and destination into a single value.
///
/// Whether copy elimination is possible depends whether the values in
/// question interfere, both with each other and with hardware registers.
void coalesce(IRFunction *f, const MachineDescription *desc, IRInstructions *instructions, AdjacencyGraph *G) {
  Vector(IRInstruction *) removed_instructions = {0};
  Vector(IRInstruction *) phis = {0};

  for (;;) {
    vector_clear(removed_instructions);
    foreach_ptr (IRInstruction *, to, *instructions) {
      if (to->kind != IR_COPY) { continue; }
      IRInstruction *from = to->operand;

      /// From and to are precoloured: eliminate the copy if they
      /// are the same, replacing all uses of to w/ from.
      if (from->result && from->result == to->result) {
        DEBUG("From AND to are coloured the same.\n");
        goto eliminate;
      }

      /// To is precoloured, from is not, and from does not interfere
      /// with the precoloured register: assign the precoloured register to
      /// from, replace all uses of to with from, and eliminate the copy.
      if (to->result && !from->result && !adjm(G->matrix, to->index, from->index) && !check_register_interference(G->regmasks[from->index], to)) {
        from->result = to->result;
        DEBUG("To is coloured, from is not, and from doesn't interfere.\n");
        goto eliminate;
      }

      /// From is precoloured, to is not, and to nor any of it's uses
      /// interfere with the precoloured register: assign the
      /// precoloured register to any PHI nodes that use to if those
      /// PHI nodes are uncoloured or precoloured with the same
      /// register; if there are any PHI nodes that are precoloured with
      /// a different register, then the copy cannot be eliminated.
      /// Otherwise, replace all uses of to with from and eliminate the
      /// copy.
      // TODO: Lens_r added this without full confidence of whether or
      // not it is actually correct. :)
      bool to_use_from_interference = false;
      foreach_ptr (IRInstruction *, user, to->users) {
        if (user->result == to->result) {
          to_use_from_interference = true;
          break;
        }
      }

      if (!to_use_from_interference && !to->result && from->result && !adjm(G->matrix, to->index, from->index) && !check_register_interference(G->regmasks[to->index], from)) {
        /// Collect all PHI nodes that use to.
        vector_clear(phis);
        foreach_ptr (IRInstruction *, phi, *instructions) {
          if (phi->kind != IR_PHI) { continue; }
          foreach_ptr (IRPhiArgument *, arg, phi->phi_args) {
            if (arg->value == to) {
              /// If a PHI node that uses to is already precoloured with
              /// a different register, then we need to keep the copy.
              if (phi->result && phi->result != from->result) { goto keep_copy; }
              vector_push(phis, phi);
              break;
            }
          }
        }

        /// If we get here, then we know that all PHI node that use to
        /// can be precoloured with the same register as from.
        foreach_ptr (IRInstruction *, phi, phis) phi->result = from->result;
        DEBUG("All PHI nodes that use `to` can be precouloured with the same register as from\n");
        goto eliminate;
      }

      /// Neither from nor to are precoloured, and they do not interfere.
      /// Replace all uses of to with from, and eliminate the copy.
      //if (!to->result && !from->result && !adjm(G->matrix, to->index, from->index)) {
      //  DEBUG("Neither from nor to are coloured, and no interference\n");
      //  goto eliminate;
      //}

    /// Otherwise, keep the copy.
    keep_copy:
      continue;

    /// Eliminate the copy.
    eliminate:

      DEBUG("Eliminating copy\n");
      IR_FEMIT_INST(stdout, to);

      /// First, replace all uses of to with from.
      ir_replace_uses(to, from);

      /// Yeet to.
      vector_push(removed_instructions, to);
    }

    /// Remove all instructions that were marked for removal.
    foreach_ptr (IRInstruction *, instruction, removed_instructions) {
      ir_remove(instruction);
    }

    /// Collect the remaining instructions.
    if (removed_instructions.size) {
      collect_instructions_into(f, instructions, 0);

      /// TODO: Ideally, we don’t want to rebuild the entire matrix on every iteration.
      build_adjacency_graph(f, desc, instructions, G);
    } else {
      break;
    }
  }

  // Find precoloured phis and attempt to apply the color to each of it's arguments.
  foreach_ptr (IRInstruction *, precoloured_phi, *instructions) {
    // Skip non-PHI instructions, PHIs with no arguments (no-op), or PHIs that have yet-to-be coloured.
    if (precoloured_phi->kind != IR_PHI || !precoloured_phi->phi_args.size || !precoloured_phi->result) continue;
    foreach_ptr (IRPhiArgument*, phi_arg, precoloured_phi->phi_args) {
      if (phi_arg->value->result)
        ICE("[RA]: It seems that a phi has had a copy removed that was necessary. TODO: warn but add it back.");
      phi_arg->value->result = precoloured_phi->result;
    }
  }

  /// Cleanup.
  vector_delete(removed_instructions);
  vector_delete(phis);
}
*/

typedef Vector(usz) NumberStack;

void print_number_stack(NumberStack *stack) {
  const usz per_line = 16;
  print("Number stack (coloring order):\n"
        "  ");
  foreach_index(i, *stack) {
    if (i) {
      print(", ");
      if (i % per_line == 0)
        print("\n  ");
    }
    print("%Z", stack->data[i]);
  }
  print("\n");
}

#ifdef DEBUG_RA
#  define PRINT_NUMBER_STACK(stack) print_number_stack(stack)
#else
#  define PRINT_NUMBER_STACK(stack)
#endif

NumberStack build_coloring_stack(const MachineDescription *desc, IRInstructions *instructions, AdjacencyGraph *G) {
  NumberStack stack = {0};

  usz k = desc->register_count;
  usz count = G->matrix.size;
  while (count) {
    /// degree < k rule:
    ///   A graph G is k-colorable if, for every node N in G, the degree
    ///   of N < k.
    bool done;
    do {
      done = true;
      foreach_index(i, G->lists) {
        AdjacencyList *list = G->lists.data[i];
        if (list->color || list->allocated) continue;
        if (list->degree < k) {
          list->allocated = 1;
          done = false;
          count--;

          /// Push onto color allocation stack.
          vector_push(stack, i);
        }
      }
    } while (!done && count);

    if (count) {
      /// Determine node with minimal spill cost.
      usz min_cost = (usz) -1; /// (!)
      usz node_to_spill = 0;

      foreach_ptr (IRInstruction *, instruction, *instructions) {
        AdjacencyList *list = G->lists.data[instruction->index];
        if (list->color || list->allocated) continue;

        list->spill_cost = list->degree ? (list->spill_cost / list->degree) : 0;
        if (list->degree && list->spill_cost <= min_cost) {
          min_cost = list->spill_cost;
          node_to_spill = instruction->index;
          if (!min_cost) break;
        }
      }

      /// Push onto color allocation stack.
      vector_push(stack, node_to_spill);
      G->lists.data[node_to_spill]->allocated = 1;
      count--;
    }
  }

  return stack;
}

static void color(
  const MachineDescription *desc,
  NumberStack *stack,
  IRInstructions *instructions,
  AdjacencyGraph *g
) {
  (void) instructions;

  foreach (usz, i, *stack) {
    AdjacencyList *list = g->lists.data[*i];
    if (list->color || list->instruction->result) continue;

    /// Each bit that is set refers to register in register list that
    /// must not be assigned to this.
    usz register_interferences = list->regmask | desc->instruction_register_interference(list->instruction);
    foreach (usz, adj, list->adjacencies) {
      AdjacencyList *adjacent = g->lists.data[*adj];
      register_interferences |= desc->instruction_register_interference(adjacent->instruction);
      if (adjacent->color) register_interferences |= 1 << (adjacent->color - 1);
    }

    Register r = 0;
    for (usz x = 0; x < desc->register_count; ++x) {
      if (!(register_interferences & 1 << x)) {
        r = (Register) (x + 1);
        break;
      }
    }

    if (!r) TODO("Can not color graph with %zu colors until stack spilling is implemented!", desc->register_count);
    list->color = r;
  }

  foreach_ptr (AdjacencyList*, list, g->lists) {
    IRInstruction *inst = list->instruction;
    Register r = list->color;
    if (inst->kind == IR_PHI) {
      foreach_ptr (IRPhiArgument *, phi, inst->phi_args) {
        if (needs_register(phi->value)) {
          AdjacencyList *phi_list = g->lists.data[phi->value->index];
          if (!phi_list->color) {
            phi_list->color = r;
            phi->value->result = r;
          }
          // TODO: Should we follow argument recursively if it is also PHI?
        }
      }
    }

    /// Do not over-write preallocated registers.
    if (!inst->result) inst->result = r;
  }
}

// Keep track of what registers are used in each function.
void track_registers(IRFunction *f) {
  FOREACH_INSTRUCTION_IN_FUNCTION(f) {
    f->registers_in_use |= 1 << instruction->result;
  }
}

/*
void allocate_registers(IRFunction *f, const MachineDescription *desc) {
  ASSERT(f && desc);
  ir_set_func_ids(f);

#ifdef DEBUG_RA
  fprintf(stdout, "======================= RA =======================\n");
  debug_context = f->context;
  ir_femit_function(stdout, f);
#endif

  phi2copy(f);

  DEBUG("After PHI2COPY\n");
  ir_set_func_ids(f);
  IR_FEMIT(stdout, f);

  function_call_arguments(f, desc);
  function_return_values(f, desc);

  fixup_precoloured(f);

  IRInstructions instructions = collect_instructions(f, 0);

  DEBUG("MTX\n");
  ir_set_func_ids(f);
  IR_FEMIT(stdout, f);

  AdjacencyGraph G = {0};
  G.order = desc->register_count;
  build_adjacency_graph(f, desc, &instructions, &G);

  PRINT_ADJACENCY_MATRIX(G.matrix);

  ir_set_func_ids(f);
  build_adjacency_lists(&instructions, &G);
  PRINT_ADJACENCY_LISTS(&G.lists);

  DEBUG("Before Coalescing\n");
  ir_set_func_ids(f);
  IR_FEMIT(stdout, f);

  coalesce(f, desc, &instructions, &G);

  DEBUG("After Coalescing\n");
  ir_set_func_ids(f);
  IR_FEMIT(stdout, f);

  collect_instructions_into(f, &instructions, 1);
  build_adjacency_graph(f, desc, &instructions, &G);
  build_adjacency_lists(&instructions, &G);

  ir_set_func_ids(f);
  DEBUG("After Rebuild\n");
  ir_set_func_ids(f);
  IR_FEMIT(stdout, f);
  PRINT_ADJACENCY_MATRIX(G.matrix);
  PRINT_ADJACENCY_LISTS(&G.lists);

  NumberStack stack = build_coloring_stack(desc, &instructions, &G);

  DEBUG("After build color stack\n");
  ir_set_func_ids(f);
  IR_FEMIT(stdout, f);
  PRINT_ADJACENCY_MATRIX(G.matrix);
  PRINT_ADJACENCY_LISTS(&G.lists);
  PRINT_NUMBER_STACK(&stack);

  color(desc, &stack, &instructions, &G);

  DEBUG("After coloring\n");
  ir_set_func_ids(f);
  IR_FEMIT(stdout, f);
  PRINT_ADJACENCY_MATRIX(G.matrix);
  PRINT_ADJACENCY_LISTS(&G.lists);
  PRINT_INSTRUCTION_LIST(&instructions);
  PRINT_NUMBER_STACK(&stack);

  track_registers(f);

  if (optimise) codegen_optimise_blocks(f->context);

  /// Free allocated resources.
  foreach_ptr (AdjacencyList*, list, G.lists) {
    vector_delete(list->adjacencies);
    free(list);
  }
  vector_delete(G.lists);
  vector_delete(instructions);
  vector_delete(stack);
  free(G.matrix.data);
  free(G.regmasks);
}
*/

void allocate_registers(MIRFunction *f, const MachineDescription *desc) {
  ASSERT(f && desc);

//#ifdef DEBUG_RA
  fprintf(stdout, "======================= MIR RA =======================\n");
  //debug_context = f->origin->context;
  print_mir_function(f);
//#endif

  DEBUG("MTX\n");
  MIR_PRINT(f);

  // List of all virtual registers that need coloured.
  VRegVector vregs = {0};

  // Populate list of vregs
  foreach_ptr (MIRBlock*, bb, f->blocks) {
    foreach_ptr (MIRInstruction*, inst, bb->instructions) {
      MIROperand *base = NULL;
      if (inst->operand_count <= MIR_OPERAND_SSO_THRESHOLD)
        base = inst->operands.arr;
      else base = inst->operands.vec.data;
      for (size_t j = 0; j < inst->operand_count; ++j) {
        MIROperand *op = base + j;
        if (op->kind == MIR_OP_REGISTER && op->value.reg.value >= MIR_ARCH_START) {
          vector_push(vregs, op->value.reg.value);
        }
      }
    }
  }

  AdjacencyGraph G = {0};
  G.order = desc->register_count;
  build_adjacency_graph(f, desc, &vregs, &G);
  PRINT_ADJACENCY_MATRIX(G.matrix);

  /*
  build_adjacency_lists(&instructions, &G);
  PRINT_ADJACENCY_LISTS(&G.lists);

  DEBUG("Before Coalescing\n");
  MIR_PRINT(f);

  //coalesce(f, desc, &instructions, &G);

  DEBUG("After Coalescing\n");
  MIR_PRINT(f);

  collect_instructions_into(f, &instructions, 1);
  build_adjacency_graph(f, desc, &instructions, &G);
  build_adjacency_lists(&instructions, &G);

  DEBUG("After Rebuild\n");
  MIR_PRINT(f);
  PRINT_ADJACENCY_MATRIX(G.matrix);
  PRINT_ADJACENCY_LISTS(&G.lists);

  NumberStack stack = build_coloring_stack(desc, &instructions, &G);

  DEBUG("After build color stack\n");
  MIR_PRINT(f);
  PRINT_ADJACENCY_MATRIX(G.matrix);
  PRINT_ADJACENCY_LISTS(&G.lists);
  PRINT_NUMBER_STACK(&stack);

  color(desc, &stack, &instructions, &G);

  DEBUG("After coloring\n");
  MIR_PRINT(f);
  PRINT_ADJACENCY_MATRIX(G.matrix);
  PRINT_ADJACENCY_LISTS(&G.lists);
  PRINT_INSTRUCTION_LIST(&instructions);
  PRINT_NUMBER_STACK(&stack);

  track_registers(f);

  if (optimise) codegen_optimise_blocks(f->context);

  /// Free allocated resources.
  foreach_ptr (AdjacencyList*, list, G.lists) {
    vector_delete(list->adjacencies);
    free(list);
  }
  vector_delete(G.lists);
  vector_delete(instructions);
  vector_delete(stack);
  free(G.matrix.data);
  free(G.regmasks);
   */
  vector_delete(vregs);
}
