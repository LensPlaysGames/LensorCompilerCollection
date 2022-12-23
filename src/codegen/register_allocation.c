#include <codegen.h>
#include <codegen/dom.h>
#include <codegen/codegen_forward.h>
#include <codegen/intermediate_representation.h>
#include <codegen/register_allocation.h>
#include <error.h>
#include <opt.h>
#include <stdlib.h>
#include <string.h>
#include <vector.h>

//#define DEBUG_RA

#ifdef DEBUG_RA
#  define IR_FEMIT(file, context) ir_femit(file, context)
#  define DEBUG(...)              fprintf(stdout, __VA_ARGS__)
CodegenContext *debug_context = NULL;
#else
#  define IR_FEMIT(file, context)
#  define DEBUG(...)
#endif

/// Used all over the place.
typedef VECTOR(IRBlock *) BlockVector;

//==== BEG REGISTER ALLOCATION PASSES ====

void phi2copy(IRFunction *f) {
  IRBlock *last_block = NULL;
  FOREACH_INSTRUCTION_IN_FUNCTION_N(f, b, phi) {
    if (phi->type == IR_PHI) {
      ASSERT(phi->parent_block != last_block,
          "Multiple PHI instructions in a single block are not allowed within register allocation!");
      last_block = phi->parent_block;

      /// Single PHI argument means that we can replace it with a simple copy.
      if (phi->phi_args.size == 1) {
        phi->type = IR_COPY;
        IRInstruction *value = phi->phi_args.data[0]->value;
        VECTOR_DELETE(phi->phi_args);
        phi->operand = value;
        continue;
      }

      /// For each of the PHI arguments, we basically insert a copy.
      /// Where we insert it depends on some complicated factors
      /// that have to do with control flow.
      VECTOR_FOREACH_PTR (IRPhiArgument *, arg, phi->phi_args) {
        STATIC_ASSERT(IR_COUNT == 32, "Handle all branch types");
        IRInstruction *branch = arg->block->instructions.last;
        switch (branch->type) {
          /// If the predecessor returns or is unreachable, then the PHI
          /// is never going to be reached anyway, so we can just ignore
          /// this argument.
          case IR_UNREACHABLE:
          case IR_RETURN: continue;

          /// Direct branches are easy, we just insert the copy before the branch.
          case IR_BRANCH: {
            IRInstruction *copy = ir_copy(f->context, arg->value);
            ir_remove_use(arg->value, phi);
            mark_used(copy, phi);
            insert_instruction_before(copy, branch);
            arg->value = copy;
          } break;

          /// Indirect branches are a bit more complicated. We need to insert an
          /// additional block for the copy instruction and replace the branch
          /// to the phi block with a branch to that block.
          case IR_BRANCH_CONDITIONAL: {
            IRInstruction *copy = ir_copy(f->context, arg->value);
            IRBlock *critical_edge_trampoline = ir_block_create();

            ir_remove_use(arg->value, phi);
            mark_used(copy, phi);
            arg->value = copy;

            ir_insert_into_block(critical_edge_trampoline, copy);
            ir_branch_into_block(phi->parent_block, critical_edge_trampoline);
            ir_block_attach_to_function(phi->parent_block->function, critical_edge_trampoline);

            if (branch->cond_br.then == phi->parent_block) {
              branch->cond_br.then = critical_edge_trampoline;
            } else {
              ASSERT(branch->cond_br.else_ == phi->parent_block,
                  "Branch to phi block is neither true nor false branch!");
              branch->cond_br.else_ = critical_edge_trampoline;
            }
          } break;
          default: UNREACHABLE();
        }
      }
    }
  }
}

void function_call_arguments(IRFunction *f, const MachineDescription *desc) {
  FOREACH_INSTRUCTION_IN_FUNCTION(f) {
    if (instruction->type == IR_CALL) {
      VECTOR_FOREACH_INDEX(i, instruction->call.arguments) {
        if (i >= desc->argument_register_count) {
          TODO("Handle stack allocated function parameters, somehow :p");
        }
        IRInstruction *argument = instruction->call.arguments.data[i];
        Register result = desc->argument_registers[i];
        IRInstruction *arg_copy = ir_copy(f->context, argument);
        mark_used(arg_copy, instruction);
        ir_remove_use(argument, instruction);
        arg_copy->result = result;
        insert_instruction_before(arg_copy, instruction);
        instruction->call.arguments.data[i] = arg_copy;
      }
    }
  }
}

void function_return_values(IRFunction *f, const MachineDescription *desc) {
  FOREACH_INSTRUCTION_IN_FUNCTION(f) {
    if (instruction->type == IR_RETURN) {
      IRInstruction *value = instruction->operand;
      IRInstruction *copy = ir_copy(f->context, value);
      mark_used(copy, instruction);
      ir_remove_use(value, instruction);
      copy->result = desc->result_register;
      insert_instruction_before(copy, instruction);
      instruction->operand = copy;
    }
  }
}

/// Insert copies for precoloured REGISTER instructions.
void fixup_precoloured(IRFunction *f) {
  FOREACH_INSTRUCTION_IN_FUNCTION(f) {
    if (instruction->type == IR_REGISTER && instruction->result) {
      IRInstruction *copy = ir_copy_unused(f->context, instruction);
      insert_instruction_after(copy, instruction);
      ir_replace_uses(instruction, copy);
      mark_used(instruction, copy);
    }
  }
}
//==== END REGISTER ALLOCATION PASSES ====

/// Return non-zero iff given instruction needs a register.
bool needs_register(IRInstruction *instruction) {
  STATIC_ASSERT(IR_COUNT == 32, "Exhaustively handle all instruction types");
  ASSERT(instruction);
  switch (instruction->type) {
    case IR_LOAD:
    case IR_PHI:
    case IR_COPY:
    case IR_IMMEDIATE:
    case IR_CALL:
    case IR_REGISTER:
    ALL_BINARY_INSTRUCTION_CASES()
      return true;
    case IR_PARAMETER:
      PANIC("Unlowered parameter instruction in register allocator");
    default:
      return false;
  }
}

//==== BEG INSTRUCTION LIST ====

typedef VECTOR(IRInstruction *) IRInstructions;

/// If needs_register is non-zero, return only instructions that need a register allocated.
void collect_instructions_into(IRFunction *f, IRInstructions *instructions, char needs_register_filter) {
  VECTOR_CLEAR(*instructions);
  FOREACH_INSTRUCTION_IN_FUNCTION(f) {
    // Add instruction to flat list iff instruction needs register
    // allocated.
    if (!needs_register_filter || needs_register(instruction)) {
      instruction->index = (u32) instructions->size;
      VECTOR_PUSH(*instructions, instruction);
    }
  }
}

IRInstructions collect_instructions(IRFunction *f, char needs_register_filter) {
  IRInstructions instructions = {0};
  collect_instructions_into(f, &instructions, needs_register_filter);
  return instructions;
}

void print_instruction_list(IRInstructions *list) {
  VECTOR_FOREACH_PTR (IRInstruction *, instruction, *list) {
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
  char *data;
} AdjacencyMatrix;

char *adjm_entry(AdjacencyMatrix m, usz x, usz y) {
  if (x > m.size) {
    PANIC("Can not access adjacency matrix because X is out of bounds.");
  }
  if (y > m.size) {
    PANIC("Can not access adjacency matrix because Y is out of bounds.");
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

char adjm(AdjacencyMatrix m, usz x, usz y) {
  return *adjm_entry(m, x, y);
}

typedef struct AdjacencyListNode AdjacencyListNode;

typedef struct AdjacencyGraph {
  usz order;
  AdjacencyMatrix matrix;
  usz *regmasks;
  AdjacencyListNode *list;
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
    bool found;
    VECTOR_CONTAINS(*live_vals, *child, found);
    if (!found) VECTOR_PUSH(*live_vals, *child);
  }
}

/// Walk over all possible paths in the dominator tree, starting at a leaf and up to the root.
/// For each path, compute instruction interferences based on the values that are currently live.
static void collect_interferences_for_node(DomTreeNode *leaf, IRInstructions *live_vals, BlockVector *visited, AdjacencyGraph *G) {
  IRBlock *b = leaf->block;

  /// Don't visit the same block twice.
  bool found;
  VECTOR_CONTAINS(*visited, b, found);
  if (found) return;
  VECTOR_PUSH(*visited, b);

  /// Collect interferences for instructions in this block.
  for (IRInstruction *inst = b->instructions.last; inst; inst = inst->prev) {
    /// Make this value interfere with all values that are live at this point.
    VECTOR_FOREACH_PTR (IRInstruction *, live_val, *live_vals)
      adjm_set(G->matrix, inst->index, live_val->index);

    /// Remove its result from the set of live variables;
    VECTOR_REMOVE_ELEMENT_UNORDERED(*live_vals, inst);

    /// Add its operands to the set of live variables.
    ir_for_each_child(inst, collect_interferences, live_vals);
  }

  /// Do the same for all dominators of this block.
  VECTOR_FOREACH_PTR (DomTreeNode *, dominator, leaf->dominators)
    collect_interferences_for_node(dominator, live_vals, visited, G);
}

/// Build the adjacency graph for the given function.
static void build_adjacency_graph(IRFunction *f, const MachineDescription *desc, IRInstructions *instructions, AdjacencyGraph *G) {
  ASSERT(instructions, "Can not build adjacency matrix of NULL instruction list.");
  allocate_adjacency_graph(G, instructions->size);

  /// Build the dominator tree.
  DominatorInfo dom_info = {0};
  build_dominator_tree(f, &dom_info, false);

  /// Collect the leaves of the dominator tree.
  VECTOR(DomTreeNode*) leaves = {0};
  VECTOR_FOREACH (DomTreeNode, node, dom_info.nodes) {
    if (node->children.size == 0) {
      VECTOR_PUSH(leaves, node);
    }
  }

  /// Values that are currently live.
  IRInstructions live_vals = {0};

  /// Blocks that have been visited.
  BlockVector visited = {0};

  /// Collect the interferences for each leaf.
  VECTOR_FOREACH_PTR (DomTreeNode*, node, leaves) {
    VECTOR_CLEAR(live_vals);
    VECTOR_CLEAR(visited);
    collect_interferences_for_node(node, &live_vals, &visited, G);
  }

  /// Free dominator and liveness info.
  free_dominator_info(&dom_info);
  VECTOR_DELETE(leaves);
  VECTOR_DELETE(live_vals);
  VECTOR_DELETE(visited);

  /// While were at it, also check for interferences with physical registers.
  /// For that, we need to iterate over all values that interfere with A and
  /// collect the physical registers that are clobbered by or precoloured to
  /// those values.
  VECTOR_FOREACH_PTR (IRInstruction *, A, *instructions) {
    usz regmask = 0;
    VECTOR_FOREACH_PTR (IRInstruction *, B, *instructions) {
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
    && (A->type == IR_SHL || A->type == IR_SHR || A->type == IR_SAR)) {
      usz regmask_rcx = 1 << (2 - 1);
      G->regmasks[A->lhs->index] |= regmask_rcx;
      G->regmasks[A->rhs->index] |= regmask_rcx;
    }
  }
}

void print_adjacency_matrix(AdjacencyMatrix m) {
  for (usz y = 0; y < m.size; ++y) {
    printf("%4zu |%3hhu", y, adjm(m, 0, y));
    for (usz x = 1; x < y; ++x) {
      char adj = adjm(m, x, y);
      adj ? printf("%3hhu", adj) : printf("   ");
    }
    printf("\n");
  }
  printf("     |  %d", 0);
  for (usz x = 1; x < m.size; ++x) {
    printf("%3zu", x);
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

typedef struct AdjacencyList AdjacencyList;

typedef struct AdjacencyListNode {
  // Degree refers to how many adjacencies this vertex/node has.
  usz degree;

  AdjacencyList *adjacencies;

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
} AdjacencyListNode;

typedef struct AdjacencyList {
  AdjacencyListNode *node;
  struct AdjacencyList *next;
} AdjacencyList;

/// Be sure to assign given head to return value.
AdjacencyList *adjl_create(AdjacencyList *head) {
  AdjacencyList *list = calloc(1, sizeof(AdjacencyList));
  AdjacencyListNode *node = calloc(1, sizeof(AdjacencyListNode));
  list->next = head;
  list->node = node;
  return list;
}

void adjl_add_impl(AdjacencyListNode *node, AdjacencyListNode *adjacent) {
  node->adjacencies = adjl_create(node->adjacencies);
  node->adjacencies->node = adjacent;
  node->degree++;
}

void adjl_add(AdjacencyListNode *A, AdjacencyListNode *B) {
  adjl_add_impl(A, B);
  adjl_add_impl(B, A);
}

void build_adjacency_lists(IRInstructions *instructions, AdjacencyGraph *G) {
  G->list = calloc(G->matrix.size + 1, sizeof(AdjacencyListNode));

  VECTOR_FOREACH_PTR (IRInstruction *, i, *instructions) {
    G->list[i->index].index = i->index;
    G->list[i->index].color = i->result;
    G->list[i->index].instruction = i;
    G->list[i->index].regmask = G->regmasks[i->index];
  }

  VECTOR_FOREACH_PTR (IRInstruction *, A, *instructions) {
    VECTOR_FOREACH_PTR (IRInstruction *, B, *instructions) {
      if (A == B) { continue; }
      if (adjm(G->matrix, A->index, B->index)) {
        adjl_add_impl(&G->list[A->index], &G->list[B->index]);
      }
    }
  }
}

void print_adjacency_list(AdjacencyList *list) {
  if (list) {
    printf("%zu", list->node->index);
    list = list->next;
  }
  for (; list; list = list->next) {
    printf(", %zu", list->node->index);
  }
  printf("\n");
}

#ifdef DEBUG_RA
#  define PRINT_ADJACENCY_LIST(list) print_adjacency_list(list)
#else
#  define PRINT_ADJACENCY_LIST(list)
#endif

void print_adjacency_array(AdjacencyListNode *array, usz size) {
  AdjacencyListNode it = array[0];
  for (usz i = 0; i < size; ++i, it = array[i]) {
    printf("%%%u::%u: ", it.instruction->id, it.instruction->index);
    if (it.color) {
      printf("(r%u) ", it.color);
    }
    print_adjacency_list(it.adjacencies);
  }
  printf("\n");
}

#ifdef DEBUG_RA
#  define PRINT_ADJACENCY_ARRAY(arr, sz) print_adjacency_array(arr, sz)
#else
#  define PRINT_ADJACENCY_ARRAY(arr, sz)
#endif

//==== END ADJACENCY LISTS ====

/// Determine whether an instruction interferes with a register.
bool check_register_interference(usz regmask, IRInstruction *instruction) {
  return (regmask & (1 << (instruction->result - 1))) != 0;
}

/// The aim of register coalescing is to eliminate register-to-register
/// copies by merging the source and destination into a single value.
///
/// Whether copy elimination is possible depends whether the values in
/// question interfere, both with each other and with hardware registers.
void coalesce(IRFunction *f, const MachineDescription *desc, IRInstructions *instructions, AdjacencyGraph *G) {
  VECTOR(IRInstruction *) removed_instructions = {0};
  VECTOR(IRInstruction *) phis = {0};

  for (;;) {
    VECTOR_CLEAR(removed_instructions);
    VECTOR_FOREACH_PTR (IRInstruction *, to, *instructions) {
      if (to->type != IR_COPY) { continue; }
      IRInstruction *from = to->operand;

      /// From and to are precoloured: eliminate the copy if they
      /// are the same, replacing all uses of to w/ from.
      if (from->result && from->result == to->result) { goto eliminate; }

      /// To is precoloured, from is not, and from does not interfere
      /// with the precoloured register: assign the precoloured register to
      /// from, replace all uses of to with from, and eliminate the copy.
      if (to->result && !from->result && !adjm(G->matrix, to->index, from->index) && !check_register_interference(G->regmasks[from->index], to)) {
        from->result = to->result;
        goto eliminate;
      }

      /// From is precoloured, to is not, and to nor any of it's uses
      /// interfere with the precoloured register: assign the
      /// precoloured register to any PHI nodes that use to if those
      /// PHI nodes are uncoloured or precoloured with the same
      /// register; if the are any PHI nodes that are precoloured with
      /// a different register, then the copy cannot be eliminated.
      /// Otherwise, replace all uses of to with from and eliminate the
      /// copy.
      // TODO: Lens_r added this without full confidence of whether or
      // not it is actually correct. :)
      bool to_use_from_interference = false;
      VECTOR_FOREACH_PTR (IRInstruction *, user, to->users) {
        if (user->result == to->result) {
          to_use_from_interference = true;
          break;
        }
      }

      if (!to_use_from_interference && !to->result && from->result && !adjm(G->matrix, to->index, from->index) && !check_register_interference(G->regmasks[to->index], from)) {
        /// Collect all PHI nodes that use to.
        VECTOR_CLEAR(phis);
        VECTOR_FOREACH_PTR (IRInstruction *, phi, *instructions) {
          if (phi->type != IR_PHI) { continue; }
          VECTOR_FOREACH_PTR (IRPhiArgument *, arg, phi->phi_args) {
            if (arg->value == to) {
              /// If a PHI node that uses to is already precoloured with
              /// a different register, then we need to keep the copy.
              if (phi->result && phi->result != from->result) { goto keep_copy; }
              VECTOR_PUSH(phis, phi);
              break;
            }
          }
        }

        /// If we get here, then we know that all PHI node that use to
        /// can be precoloured with the same register as from.
        VECTOR_FOREACH_PTR (IRInstruction *, phi, phis) { phi->result = from->result; }
        goto eliminate;
      }

      /// Neither from nor to are precoloured, and they do not interfere.
      /// Replace all uses of to with from, and eliminate the copy.
      if (!to->result && !from->result && !adjm(G->matrix, to->index, from->index)) {
        goto eliminate;
      }

    /// Otherwise, keep the copy.
    keep_copy:
      continue;

    /// Eliminate the copy.
    eliminate:

      /// First, replace all uses of to with from.
      ir_replace_uses(to, from);

      /// Yeet to.
      VECTOR_PUSH(removed_instructions, to);
    }

    /// Remove all instructions that were marked for removal.
    VECTOR_FOREACH_PTR (IRInstruction *, instruction, removed_instructions) {
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

  /// Cleanup.
  VECTOR_DELETE(removed_instructions);
  VECTOR_DELETE(phis);
}

typedef struct NumberStack {
  usz number;
  struct NumberStack *next;
} NumberStack;

void print_number_stack(NumberStack *stack) {
  if (stack) {
    printf("%zu", stack->number);
    stack = stack->next;
  }
  for (; stack; stack = stack->next) {
    printf(", %zu", stack->number);
  }
  printf("\n");
}

#ifdef DEBUG_RA
#  define PRINT_NUMBER_STACK(stack) print_number_stack(stack)
#else
#  define PRINT_NUMBER_STACK(stack)
#endif

NumberStack *build_coloring_stack(const MachineDescription *desc, IRInstructions *instructions, AdjacencyGraph *G) {
  NumberStack *stack = NULL;

  AdjacencyListNode *array = calloc(G->matrix.size + 1, sizeof(AdjacencyListNode));
  for (usz i = 0; i < G->matrix.size; ++i) {
    AdjacencyListNode *new_node = array + i;
    AdjacencyListNode *node = G->list + i;
    *new_node = *node;
    for (AdjacencyList *adj_it = node->adjacencies; adj_it; adj_it = adj_it->next) {
      adjl_add_impl(new_node, adj_it->node);
    }
  }

  usz k = desc->register_count;
  usz count = G->matrix.size;
  while (count) {
    // degree < k rule:
    //   A graph G is k-colorable if, for every node N in G, the degree
    //   of N < k.
    bool done;
    do {
      done = true;
      for (usz i = 0; i < G->matrix.size; ++i) {
        AdjacencyListNode *node = array + i;
        if (node->color || node->allocated) {
          continue;
        }
        if (node->degree < k) {
          done = false;

          // push onto color allocation stack
          NumberStack *new_number = calloc(1, sizeof(NumberStack));
          new_number->number = i;
          new_number->next = stack;
          stack = new_number;

          node->allocated = 1;

          --count;
        }
      }
    } while (!done && count);

    if (count) {
      // Determine node with minimal spill cost.
      usz min_cost = (usz) -1;
      usz node_to_spill = 0;

      VECTOR_FOREACH_PTR (IRInstruction *, instruction, *instructions) {
        AdjacencyListNode *node = array + instruction->index;
        if (node->color || node->allocated) {
          continue;
        }
        node->spill_cost = node->degree ? (node->spill_cost / node->degree) : 0;
        if (node->degree && node->spill_cost <= min_cost) {
          min_cost = node->spill_cost;
          node_to_spill = instruction->index;
          // node->allocated = 1;
          if (!min_cost) {
            break;
          }
        }
      }

      // push onto color allocation stack
      NumberStack *new_number = calloc(1, sizeof(NumberStack));
      new_number->number = node_to_spill;
      new_number->next = stack;
      stack = new_number;

      (array + node_to_spill)->allocated = 1;

      --count;
    }
  }

  return stack;
}

void color(
  const MachineDescription *desc,
  NumberStack *stack,
  IRInstructions *instructions,
  AdjacencyListNode *array,
  usz length
) {
  (void) instructions;

  for (NumberStack *i = stack; i; i = i->next) {
    AdjacencyListNode *node = array + i->number;
    if (node->color || node->instruction->result) {
      continue;
    }

    Register r = 0;

    usz k = desc->register_count;
    // Each bit that is set refers to register in register list that
    // must not be assigned to this.
    usz register_interferences = node->regmask;
    register_interferences |=
        desc->instruction_register_interference(node->instruction);
    for (AdjacencyList *adj_it = node->adjacencies; adj_it; adj_it = adj_it->next) {
      register_interferences |=
          desc->instruction_register_interference(adj_it->node->instruction);
      if (adj_it->node->color) {
        register_interferences |= 1 << (adj_it->node->color - 1);
      }
    }

    for (usz x = 0; x < k; ++x) {
      if (!(register_interferences & 1 << x)) {
        r = (Register) (x + 1);
        break;
      }
    }

    if (!r) {
      TODO("Can not color graph with %zu colors until stack spilling is implemented!", k);
    }

    node->color = r;
  }

  for (usz i = 0; i < length; ++i) {
    AdjacencyListNode node = array[i];
    IRInstruction *instruction = node.instruction;
    Register r = node.color;
    if (instruction->type == IR_PHI) {
      VECTOR_FOREACH_PTR (IRPhiArgument *, phi, instruction->phi_args) {
        if (needs_register(phi->value)) {
          AdjacencyListNode *phi_node = array + phi->value->index;
          phi_node->color = r;
          phi->value->result = r;
          // TODO: Should we follow argument recursively if it is also PHI?
        }
      }
    }

    // Do not over-write preallocated registers.
    if (instruction->result) {
      continue;
    }
    instruction->result = r;
  }
}

// Keep track of what registers are used in each function.
void track_registers(IRFunction *f) {
  FOREACH_INSTRUCTION_IN_FUNCTION(f) {
    f->registers_in_use |= 1 << instruction->result;
  }
}

void allocate_registers(IRFunction *f, const MachineDescription *desc) {
  ASSERT(f && desc);
  ir_set_func_ids(f);

#ifdef DEBUG_RA
  debug_context = f->context;
  ir_femit_function(stdout, f);
#endif


  phi2copy(f);

  DEBUG("After PHI2COPY\n");
  ir_set_func_ids(f);
  IR_FEMIT(stdout, f->context);

  function_call_arguments(f, desc);
  function_return_values(f, desc);

  fixup_precoloured(f);

  IRInstructions instructions = collect_instructions(f, 0);

  DEBUG("MTX\n");
  ir_set_func_ids(f);
  IR_FEMIT(stdout, f->context);

  AdjacencyGraph G = {0};
  G.order = desc->register_count;
  build_adjacency_graph(f, desc, &instructions, &G);

  PRINT_ADJACENCY_MATRIX(G.matrix);

  ir_set_func_ids(f);
  build_adjacency_lists(&instructions, &G);
  PRINT_ADJACENCY_ARRAY(G.list, G.matrix.size);

  DEBUG("Before Coalescing\n");
  ir_set_func_ids(f);
  IR_FEMIT(stdout, f->context);

  coalesce(f, desc, &instructions, &G);

  DEBUG("After Coalescing\n");
  ir_set_func_ids(f);
  IR_FEMIT(stdout, f->context);

  instructions = collect_instructions(f, 1);
  build_adjacency_graph(f, desc, &instructions, &G);
  build_adjacency_lists(&instructions, &G);

  ir_set_func_ids(f);
  DEBUG("After Rebuild\n");
  ir_set_func_ids(f);
  IR_FEMIT(stdout, f->context);
  PRINT_ADJACENCY_MATRIX(G.matrix);
  PRINT_ADJACENCY_ARRAY(G.list, G.matrix.size);

  NumberStack *stack = build_coloring_stack(desc, &instructions, &G);

  DEBUG("After build color stack\n");
  ir_set_func_ids(f);
  IR_FEMIT(stdout, f->context);
  PRINT_ADJACENCY_MATRIX(G.matrix);
  PRINT_ADJACENCY_ARRAY(G.list, G.matrix.size);
  PRINT_NUMBER_STACK(stack);

  color(desc, stack, &instructions, G.list, G.matrix.size);

  DEBUG("After coloring\n");
  ir_set_func_ids(f);
  IR_FEMIT(stdout, f->context);
  PRINT_ADJACENCY_MATRIX(G.matrix);
  PRINT_ADJACENCY_ARRAY(G.list, G.matrix.size);
  PRINT_INSTRUCTION_LIST(&instructions);
  PRINT_NUMBER_STACK(stack);

  track_registers(f);

  if (optimise) codegen_optimise_blocks(f->context);
}
