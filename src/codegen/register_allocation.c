#include <codegen.h>
#include <codegen/intermediate_representation.h>
#include <codegen/register_allocation.h>
#include <error.h>
#include <opt.h>
#include <stdlib.h>
#include <string.h>
#include <vector.h>

// #define DEBUG_RA

#ifdef DEBUG_RA
#define IR_FEMIT(file, context) ir_femit(file, context)
#define DEBUG(...) fprintf(stdout, __VA_ARGS__)
#else
#define IR_FEMIT(file, context)
#define DEBUG(...)
#endif

RegisterAllocationInfo *ra_allocate_info
(CodegenContext *context,
 Register result_register,
 size_t general_registers_count,
 Register *general_registers,
 size_t argument_registers_count,
 Register *argument_registers,
 size_t instruction_register_interference(IRInstruction *instruction)
 )
{
  if (!context) { return NULL; }
  if (general_registers_count == 0) { return NULL; }
  if (!general_registers) { return NULL; }

  RegisterAllocationInfo *info = calloc(1, sizeof(*info));

  info->context = context;
  info->result_register = result_register;

  info->register_count = general_registers_count;
  info->registers = general_registers;

  info->argument_register_count = argument_registers_count;
  info->argument_registers = argument_registers;

  info->instruction_register_interference = instruction_register_interference;

  return info;
}

//==== BEG REGISTER ALLOCATION PASSES ====

void phi2copy(RegisterAllocationInfo *info) {
  IRBlock *last_block = NULL;
  FOREACH_INSTRUCTION_N (info->context, f, b, phi) {
    if (phi->type == IR_PHI) {
      ASSERT(phi->block != last_block,
             "Multiple PHI instructions in a single block are not allowed within register allocation!");
      last_block = phi->block;

      /// Single PHI argument means that we can replace it with a simple copy.
      /// FIXME: Disabled for now because it would have to mark uses correctly.
      /*if (phi->value.phi_arguments.size == 1) {
        phi->type = IR_COPY;
        IRInstruction *value = phi->value.phi_arguments.data[0].value;
        VECTOR_DELETE(phi->value.phi_arguments);
        phi->value.reference = value;
        continue;
      }*/

      /// For each of the PHI arguments, we basically insert a copy.
      /// Where we insert it depends on some complicated factors
      /// that have to do with control flow.
      VECTOR_FOREACH (IRPhiArgument, arg, phi->value.phi_arguments) {
        STATIC_ASSERT(IR_COUNT == 28, "Handle all branch types");
        IRInstruction *branch = arg->block->instructions.last;
        switch (branch->type) {
          /// If the predecessor returns or is unreachable, then the PHI
          /// is never going to be reached anyway, so we can just ignore
          /// this argument.
          case IR_UNREACHABLE:
          case IR_RETURN: continue;

          /// Direct branches are easy, we just insert the copy before the branch.
          case IR_BRANCH: {
            IRInstruction *copy = ir_copy(info->context, arg->value);
            ir_remove_use(arg->value, phi);
            mark_used(copy, phi);
            insert_instruction_before(copy, branch);
            arg->value = copy;
          } break;

          /// Indirect branches are a bit more complicated. We need to insert an
          /// additional block for the copy instruction and replace the branch
          /// to the phi block with a branch to that block.
          case IR_BRANCH_CONDITIONAL: {
            IRInstruction *copy = ir_copy(info->context, arg->value);
            IRBlock *critical_edge_trampoline = ir_block_create();

            ir_remove_use(arg->value, phi);
            mark_used(copy, phi);
            arg->value = copy;

            ir_insert_into_block(critical_edge_trampoline, copy);
            ir_branch_into_block(phi->block, critical_edge_trampoline);
            ir_block_attach_to_function(phi->block->function, critical_edge_trampoline);

            if (branch->value.conditional_branch.true_branch == phi->block) {
              branch->value.conditional_branch.true_branch = critical_edge_trampoline;
            } else {
              ASSERT(branch->value.conditional_branch.false_branch == phi->block,
                     "Branch to phi block is neither true nor false branch!");
              branch->value.conditional_branch.false_branch = critical_edge_trampoline;
            }
          } break;
          default: UNREACHABLE();
        }
      }
    }
  }
}

void function_call_arguments(RegisterAllocationInfo *info) {
  FOREACH_INSTRUCTION (info->context) {
    if (instruction->type == IR_CALL) {
      IRCallArgument *arguments = instruction->value.call.arguments;
      for (size_t i = 0; arguments; ++i, arguments = arguments->next) {
        if (i >= info->argument_register_count) {
          TODO("Handle stack allocated function parameters, somehow :p");
        }
        IRInstruction *argument = arguments->value;
        Register result = info->argument_registers[i];
        IRInstruction *arg_copy = ir_copy(info->context, argument);
        mark_used(arg_copy, instruction);
        ir_remove_use(argument, instruction);
        arg_copy->result = result;
        insert_instruction_before(arg_copy, instruction);
        arguments->value = arg_copy;
      }
    }
  }
}

void function_return_values(RegisterAllocationInfo *info) {
  FOREACH_INSTRUCTION(info->context) {
    if (instruction->type == IR_RETURN) {
      IRInstruction *value = instruction->value.reference;
      IRInstruction *copy = ir_copy(info->context, value);
      mark_used(copy, instruction);
      ir_remove_use(value, instruction);
      copy->result = info->result_register;
      insert_instruction_before(copy, instruction);
      instruction->value.reference = copy;
    }
  }
}

/// Insert copies for precoloured REGISTER instructions.
void fixup_precoloured(RegisterAllocationInfo *info) {
  FOREACH_INSTRUCTION (info->context) {
    if (instruction->type == IR_REGISTER && instruction->result) {
      IRInstruction *copy = ir_copy_unused(info->context, instruction);
      insert_instruction_after(copy, instruction);
      ir_replace_uses(instruction, copy);
      mark_used(instruction, copy);
    }
  }
}
//==== END REGISTER ALLOCATION PASSES ====

/// Return non-zero iff given instruction needs a register.
bool needs_register(IRInstruction *instruction) {
  ASSERT(instruction);
  switch(instruction->type) {
  case IR_ADD:
  case IR_SUBTRACT:
  case IR_MULTIPLY:
  case IR_DIVIDE:
  case IR_MODULO:
  case IR_SHIFT_LEFT:
  case IR_SHIFT_RIGHT_LOGICAL:
  case IR_SHIFT_RIGHT_ARITHMETIC:
  case IR_LOAD:
  case IR_LOCAL_LOAD:
  case IR_LOCAL_ADDRESS:
  case IR_GLOBAL_LOAD:
  case IR_GLOBAL_ADDRESS:
  case IR_PHI:
  case IR_COPY:
  case IR_IMMEDIATE:
  case IR_COMPARISON:
  case IR_CALL:
  case IR_PARAMETER_REFERENCE:
  case IR_REGISTER:
    return true;
  default:
    return false;
  }
}

//==== BEG INSTRUCTION LIST ====

typedef VECTOR(IRInstruction *) IRInstructions;

/// If needs_register is non-zero, return only instructions that need a register allocated.
void collect_instructions_into(RegisterAllocationInfo *info, IRInstructions *instructions, char needs_register_filter) {
  VECTOR_CLEAR(*instructions);
  FOREACH_INSTRUCTION (info->context) {
    // Add instruction to flat list iff instruction needs register
    // allocated.
    if (!needs_register_filter || needs_register(instruction)) {
      instruction->index = instructions->size;
      VECTOR_PUSH(*instructions, instruction);
    }
  }
}

IRInstructions collect_instructions(RegisterAllocationInfo *info, char needs_register_filter) {
  IRInstructions instructions = {0};
  collect_instructions_into(info, &instructions, needs_register_filter);
  return instructions;
}

void print_instruction_list(IRInstructions *list) {
  VECTOR_FOREACH_PTR (IRInstruction *, instruction, *list) {
    ir_femit_instruction(stdout, instruction);
  }
}

#ifdef DEBUG_RA
#define PRINT_INSTRUCTION_LIST(list) print_instruction_list(list)
#else
#define PRINT_INSTRUCTION_LIST(list)
#endif

//==== END INSTRUCTION LIST ====

typedef struct IRBlockList {
  IRBlock *block;
  struct IRBlockList *next;
} IRBlockList;

//==== BEG ADJACENCY MATRIX ====

typedef struct AdjacencyMatrix {
  size_t size;
  char *data;
} AdjacencyMatrix;

char *adjm_entry(AdjacencyMatrix m, size_t x, size_t y) {
  if (x > m.size) {
    PANIC("Can not access adjacency matrix because X is out of bounds.");
  }
  if (y > m.size) {
    PANIC("Can not access adjacency matrix because Y is out of bounds.");
  }
  // Coordinate translation for lower left triangle matrix
  if (y > x) {
    size_t old_x = x;
    x = y;
    y = old_x;
  }
  return &m.data[y * m.size + x];
}

void adjm_set(AdjacencyMatrix m, size_t x, size_t y) {
  *adjm_entry(m, x, y) = 1;
}

void adjm_clear(AdjacencyMatrix m, size_t x, size_t y) {
  *adjm_entry(m, x, y) = 0;
}

char adjm(AdjacencyMatrix m, size_t x, size_t y) {
  return *adjm_entry(m, x, y);
}

typedef struct AdjacencyListNode AdjacencyListNode;

typedef struct AdjacencyGraph {
  size_t order;
  AdjacencyMatrix matrix;
  size_t* regmasks;
  AdjacencyListNode *list;
} AdjacencyGraph;

void allocate_adjacency_graph(AdjacencyGraph *G, size_t size) {
  if (G->matrix.data) { free(G->matrix.data); }
  if (G->regmasks) { free(G->regmasks); }
  G->matrix.size = size;
  G->matrix.data = calloc(1, size * size + 1);
  G->regmasks = calloc(1, size * sizeof(size_t));
}

/// Vector of blocks for interference checking.
typedef VECTOR(IRBlock*) BlockVector;

/// Forward declarations.
bool interferes_after_def(BlockVector *visited, IRBlock *B, IRInstruction *v1);
bool block_reachable_from_successors(BlockVector *visited, IRBlock *from, IRBlock *block);

/// Check if a block has already been visited.
bool block_visited(BlockVector *visited, IRBlock *block) {
  VECTOR_FOREACH_PTR (IRBlock*, b, *visited) {
    if (b == block) { return true; }
  }

  return false;
}

/// Check if a block is reachable from this block or any of its successors.
/// Do not call this directly. Use block_reachable() instead.
bool block_reachable_from_successor(BlockVector *visited, IRBlock *from, IRBlock *block) {
  if (from == block) return true;

  VECTOR_PUSH(*visited, from);
  return block_reachable_from_successors(visited, from, block);
}

/// Check if a block is reachable from any of the successors of another block.
/// Do not call this directly. Use block_reachable() instead.
bool block_reachable_from_successors(BlockVector *visited, IRBlock *from, IRBlock *block) {
  STATIC_ASSERT(IR_COUNT == 28, "Handle all branch instructions");
  IRInstruction *branch = from->instructions.last;
  switch (branch->type) {
    case IR_UNREACHABLE:
    case IR_RETURN: return false;
    case IR_BRANCH:
      if (block_visited(visited, branch->value.block)) { return false; }
      return block_reachable_from_successor(visited, branch->value.block, block);
    case IR_BRANCH_CONDITIONAL:
      if (!block_visited(visited, branch->value.conditional_branch.true_branch)) {
        if (block_reachable_from_successor(visited, branch->value.conditional_branch.true_branch, block)) {
          return true;
        }
      }
      if (!block_visited(visited, branch->value.conditional_branch.false_branch)) {
        if (block_reachable_from_successor(visited, branch->value.conditional_branch.false_branch, block)) {
          return true;
        }
      }
      return false;
    default: UNREACHABLE();
  }
}

/// Check if a block is reachable from another block.
bool block_reachable(IRBlock *from, IRBlock *block) {
  if (block == from) { return true; }

  BlockVector visited = {0};
  VECTOR_PUSH(visited, from);
  bool reachable = block_reachable_from_successors(&visited, from, block);
  VECTOR_DELETE(visited);
  return reachable;
}

/// Check if there is a use of v1 in B or any of its successors.
/// \see values_interfere()
bool has_use_after_def_in_block(BlockVector *visited, IRBlock *B, IRInstruction *v1) {
  VECTOR_FOREACH_PTR (IRInstruction *, user, v1->users) {
    if (user->block == B) { return true; }
  }

  VECTOR_PUSH(*visited, B);
  return interferes_after_def (visited, B, v1);
}

/// Check if there is a use of v1 in any of the successors of B.
/// \see values_interfere()
bool interferes_after_def(BlockVector *visited, IRBlock *B, IRInstruction *v1) {
  STATIC_ASSERT(IR_COUNT == 28, "Handle all branch instructions");
  IRInstruction *branch = B->instructions.last;
  switch (branch->type) {
    case IR_UNREACHABLE:
    case IR_RETURN: return false;

    case IR_BRANCH:
      if (block_visited(visited, branch->value.block)) { return false; }
      return has_use_after_def_in_block(visited, branch->value.block, v1);

    case IR_BRANCH_CONDITIONAL:
      if (!block_visited(visited, branch->value.conditional_branch.true_branch)) {
        if (has_use_after_def_in_block(visited, branch->value.conditional_branch.true_branch, v1)) {
          return true;
        }
      }
      if (!block_visited(visited, branch->value.conditional_branch.false_branch)) {
        if (has_use_after_def_in_block(visited, branch->value.conditional_branch.false_branch, v1)) {
          return true;
        }
      }
      return false;
    default: UNREACHABLE();
  }
}

/// Check if a value interferes with another value in the block containing
/// the second value’s definition.
/// \see values_interfere()
bool inteferes_at_def(RegisterAllocationInfo *info, IRBlock *b, IRInstruction *v1, IRInstruction *v2) {
  VECTOR_FOREACH_PTR (IRInstruction *, user, v1->users) {
    if (user->block != b) { continue; }
    if (user->index > v2->index) { return true; }
  }

  BlockVector visited = {0};
  VECTOR_PUSH(visited, b);
  bool interferes = interferes_after_def(&visited, b, v1);
  VECTOR_DELETE(visited);
  return interferes;
}

/// Check if two values interfere if the definition of the first precedes
/// that of the second.
/// \see values_interfere()
bool check_values_interfere(RegisterAllocationInfo *info, IRInstruction *v1, IRInstruction *v2) {
  IRBlock *B1 = v1->block, *B2 = v2->block;
  if (B1 == B2) {
    /// This case will be handled by the second invocation of this function
    /// in values_interfere().
    if (v1->index > v2->index) { return false; }
    return inteferes_at_def(info, B1, v1, v2);
  }

  if (!block_reachable(B1, B2)) { return false; }
  return inteferes_at_def(info, B2, v1, v2);
}

/// Determine whether two values interfere.
///
/// Two values interfere if either one is live at the definition point
/// of the other. A value is live starting at its definition point until
/// just before its last use. If a value’s last use is in a different
/// block than its definition, then it is live in all blocks between
/// the definition and the use, in control flow order.
///
/// Whether a value V is live at its own definition point is undefined,
/// simply because it’s not relevant: the only situation in which this
/// would matter is if there were a value V' whose last use was in the
/// definition of V. If V were considered live at its own definition,
/// one might erroneously assume that the two would interfere, and that
/// the RA might think that V' could not be assigned the same register
/// as V. However, this is incorrect, since, as mentioned above, a value
/// is live up to just *before* its last use. Therefore, V' would not
/// interfere with V in this case, and the question of whether or not a
/// value is live at its own definition point is irrelevant.
///
/// The algorithm that is used to determine whether two values interfere
/// is described below and implemented across several mutually recursive
/// functions.
///
/// The algorithm is as follows:
///
///   ## has-use-after-def-in-block (visited, B, v1)
///      1. If there is a use of v1 in B, return true.
///      2. Return interferes-after-def (visited, B, v1).
///
///   ## interferes-after-def (visited, B, v1)
///      1. For each successor S of B:
///         1a. If S is in visited, continue.
///         1b. Add S to visited.
///         1c. If has-use-after-def-in-block (S, v1) returns true, return true.
///      2. Return false.
///
///   ## interferes-at-def (B, v1, v2)
///      1. For each use U of v1 in B, if U->idx > v2->idx, return true.
///      2. Let visited be a set of blocks containing B.
///      3. Return interferes-after-def (visited, B, v1)
///
///   ## check-values-interfere (v1, v2)
///      1. Let B1, B2 be the blocks containing the definitions of v1, v2.
///      2. If B1 == B2, then
///         2a. If v1->idx > v2->idx, return false.
///         2b. Return interferes-at-def (B1, v1, v2)
///      3. If B2 is not reachable from B1, return false.
///      4. Return interferes-at-def (B2, v1, v2).
///
///   ## values-interfere (v1, v2)
///      1. If v1 == v2, return false.
///      2. If v1 is a COPY instruction and its argument is v2, or vice versa, return false.
///      3. If check-values-interfere (v1, v2) returns true, return true.
///      4. Return check-values-interfere (v2, v1).
///
/// \see has_use_after_def_in_block()
/// \see interferes_after_def()
/// \see interferes_at_def()
/// \see check_values_interfere()
bool values_interfere(RegisterAllocationInfo *info, IRInstruction *v1, IRInstruction *v2) {
  if (v1 == v2) { return false; }
  if (v1->type == IR_COPY && v1->value.reference == v2) { return false; }
  if (v2->type == IR_COPY && v2->value.reference == v1) { return false; }
  //if (v1->index == 19 && v2->index == 9) asm volatile ("int $3;");
  if (check_values_interfere(info, v1, v2)) { return true; }
  return check_values_interfere(info, v2, v1);
}

#if 0
enum ControlFlowResult {
  RA_CF_CONTINUE,
  RA_CF_INTERFERE,
  RA_CF_CLEARED,
};

int follow_control_flow(IRBlockList *visited, IRBlock *block, IRInstruction *a_use, IRInstruction *B) {
  ASSERT(block, "Can not follow control flow of NULL block.");
  ASSERT(a_use, "Can not follow control flow of NULL use.");
  ASSERT(B, "Can not follow control flow of NULL definition.");
  while (1) {
    // Return if block has been visited already.
    for (IRBlockList *visited_it = visited; visited_it; visited_it = visited_it->next) {
      if (visited_it->block == block) {
        return RA_CF_CONTINUE;
      }
    }

    // Add block to visited blocks list.
    IRBlockList *new_block = calloc(1, sizeof(IRBlockList));
    new_block->next = visited;
    new_block->block = block;
    visited = new_block;

    // If use of A and definition of B are in this block, and the
    // definition of B precedes the use of A, then it stands to reason
    // that B is in-between the definition of A and its use, meaning
    // there is an interference.
    if (a_use->block == block) {
      if (B->block == block && B->index < a_use->index) {
        // TODO: Check that A and B are not precolored before clearing.
        return B->type == IR_COPY ? RA_CF_CLEARED : RA_CF_INTERFERE;
      }
      return RA_CF_CLEARED;
    }

    if (B->block == block) {
      // If only the definition of B is within the block, then the use of
      // A must come after, otherwise we would have seen it already.
      return B->type == IR_COPY ? RA_CF_CLEARED : RA_CF_INTERFERE;
    }

    // Actually follow control flow, according to branch instruction.
    switch (block->branch->type) {
      case IR_RETURN:
        return RA_CF_CLEARED;
        break;
      case IR_BRANCH:
        block = block->branch->value.block;
        continue;
        break;
      case IR_BRANCH_CONDITIONAL:
        if(0){}
        int result = follow_control_flow
            (visited,
                block->branch->value.conditional_branch.true_branch,
                a_use, B);
        if (result != RA_CF_CONTINUE) {
          return result;
        }
        block = block->branch->value.conditional_branch.false_branch;
        continue;
        break;
      default:
        TODO("Handle branch instruction type: %d", block->branch->type);
        break;
    }
    UNREACHABLE();
  }
  return RA_CF_CONTINUE;
}

char instructions_interfere(IRInstruction *A, IRInstruction *B) {
  ASSERT(A && B, "Can not get interference of NULL instructions.");
  ASSERT(A->block, "Can not get interference when A has NULL containing block.");
  for (Use *a_use = A->uses; a_use; a_use = a_use->next) {
    ASSERT(a_use->user, "Use instruction can not be NULL!");

    // If definition and use of A are in the same block, and if the
    // definition of B is also in the same block, it is a simple index
    // comparison to check for interference.
    if (A->block == a_use->user->block && B->block == A->block && A->index < B->index && a_use->user->index > B->index) {
      return true;
    } else {
      // A and B are not defined in the same block, follow control flow.
      if (follow_control_flow(NULL, A->block, a_use->user, B) == RA_CF_INTERFERE) {
        return 1;
      }
    }
  }
  return 0;
}
#endif

void build_adjacency_graph(RegisterAllocationInfo *info, IRInstructions *instructions, AdjacencyGraph *G) {
  ASSERT(instructions, "Can not build adjacency matrix of NULL instruction list.");
  allocate_adjacency_graph(G, instructions->size);
  VECTOR_FOREACH_PTR (IRInstruction*, A, *instructions) {
    VECTOR_FOREACH_PTR (IRInstruction*, B, *instructions) {
      if (A == B) { break; } // (!)
      if (adjm(G->matrix, A->index, B->index)) { continue; }
      if (values_interfere(info, A, B))
        { adjm_set(G->matrix, A->index, B->index); }
    }
  }

  /// While were at it, also check for interferences with physical registers.
  /// For that, we need to iterate over all values that interfere with A and
  /// collect the physical registers that are clobbered by or precoloured to
  /// those values.
  VECTOR_FOREACH_PTR (IRInstruction *, A, *instructions) {
    size_t regmask = 0;
    VECTOR_FOREACH_PTR (IRInstruction *, B, *instructions) {
      if (A == B) { break; } /// (!)
      if (!adjm(G->matrix, A->index, B->index)) { continue; }
      regmask |= info->instruction_register_interference(B);
      if (B->result) regmask |= 1 << (B->result - 1);
    }
    G->regmasks[A->index] = regmask;
  }
}


void print_adjacency_matrix(AdjacencyMatrix m) {
  for (size_t y = 0; y < m.size; ++y) {
    printf("%4zu |%3hhu", y, adjm(m, 0, y));
    for (size_t x = 1; x < y; ++x) {
      char adj = adjm(m, x, y);
      adj ? printf("%3hhu", adj) : printf("   ");
    }
    printf("\n");
  }
  printf("     |  %d", 0);
  for (size_t x = 1; x < m.size; ++x) {
    printf("%3zu", x);
  }
  printf("\n\n");
}

#ifdef DEBUG_RA
#define PRINT_ADJACENCY_MATRIX(m) print_adjacency_matrix(m)
#else
#define PRINT_ADJACENCY_MATRIX(m)
#endif

//==== END ADJACENCY MATRIX ====

//==== BEG ADJACENCY LISTS ====

typedef struct AdjacencyList AdjacencyList;

typedef struct AdjacencyListNode {
  // Degree refers to how many adjacencies this vertex/node has.
  size_t degree;

  AdjacencyList *adjacencies;

  IRInstruction *instruction;

  // Unique integer index.
  size_t index;

  size_t regmask;

  Register color;

  char allocated;

  // Spill handling.
  char spill_flag;
  size_t spill_offset;
  size_t spill_cost;
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

  VECTOR_FOREACH_PTR (IRInstruction*, i, *instructions) {
    G->list[i->index].index = i->index;
    G->list[i->index].color = i->result;
    G->list[i->index].instruction = i;
    G->list[i->index].regmask = G->regmasks[i->index];
  }

  VECTOR_FOREACH_PTR (IRInstruction*, A, *instructions) {
    VECTOR_FOREACH_PTR (IRInstruction*, B, *instructions) {
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
#define PRINT_ADJACENCY_LIST(list) print_adjacency_list(list)
#else
#define PRINT_ADJACENCY_LIST(list)
#endif

void print_adjacency_array(AdjacencyListNode *array, size_t size) {
  AdjacencyListNode it = array[0];
  for (size_t i = 0; i < size; ++i, it = array[i]) {
    printf("%%%zu::%zu: ", it.instruction->id, it.instruction->index);
    if (it.color) {
      printf("(r%u) ", it.color);
    }
    print_adjacency_list(it.adjacencies);
  }
  printf("\n");
}

#ifdef DEBUG_RA
#define PRINT_ADJACENCY_ARRAY(arr, sz) print_adjacency_array(arr, sz)
#else
#define PRINT_ADJACENCY_ARRAY(arr, sz)
#endif

//==== END ADJACENCY LISTS ====

/// Determine whether an instruction interferes with a register.
bool check_register_interference(size_t regmask, IRInstruction *instruction) {
  return (regmask & (1 << (instruction->result - 1))) != 0;
}

/// The aim of register coalescing is to eliminate register-to-register
/// copies by merging the source and destination into a single value.
///
/// Whether copy elimination is possible depends whether the values in
/// question interfere, both with each other and with hardware registers.
void coalesce(RegisterAllocationInfo *info, IRInstructions *instructions, AdjacencyGraph *G) {
#if 1
  VECTOR(IRInstruction*) removed_instructions = {0};
  VECTOR(IRInstruction*) phis = {0};

  for (;;) {
    VECTOR_CLEAR(removed_instructions);
    VECTOR_FOREACH_PTR (IRInstruction*, to, *instructions) {
      if (to->type != IR_COPY) { continue; }
      IRInstruction *from = to->value.reference;

      /// From and to are precoloured: eliminate the copy if they
      /// are the same, replacing all uses of to w/ from.
      if (from->result && from->result == to->result) { goto eliminate; }

      /// To is precoloured, from is not, and from does not interfere
      /// with the precoloured register: assign the precoloured register to
      /// from, replace all uses of to with from, and eliminate the copy.
      if (to->result &&
         !from->result &&
         !adjm(G->matrix, to->index, from->index) &&
         !check_register_interference(G->regmasks[from->index], to)) {
        from->result = to->result;
        goto eliminate;
      }

      /// From is precoloured, to is not, and to does not interfere
      /// with the precoloured register: assign the precoloured register to
      /// any PHI nodes that use to if those PHI nodes are uncoloured or
      /// precoloured with the same register; if the are any PHI nodes that
      /// are precoloured with a different register, then the copy cannot
      /// be eliminated. Otherwise, replace all uses of to with from and
      /// eliminate the copy.
      if (!to->result &&
           from->result &&
          !adjm(G->matrix, to->index, from->index) &&
          !check_register_interference(G->regmasks[to->index], from)) {
        /// Collect all PHI nodes that use to.
        VECTOR_CLEAR(phis);
        VECTOR_FOREACH_PTR (IRInstruction*, phi, *instructions) {
          if (phi->type != IR_PHI) { continue; }
          VECTOR_FOREACH (IRPhiArgument, arg, phi->value.phi_arguments) {
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
        VECTOR_FOREACH_PTR (IRInstruction*, phi, phis) { phi->result = from->result; }
        goto eliminate;
      }

      /// Neither from nor to are precoloured, and they do not interfere.
      /// Replace all uses of to with from, and eliminate the copy.
      if (!to->result &&
          !from->result &&
          !adjm(G->matrix, to->index, from->index)) {
        goto eliminate;
      }

      /// Otherwise, keep the copy.
      keep_copy: continue;

      /// Eliminate the copy.
      eliminate:

      /// First, replace all uses of to with from.
      ir_replace_uses(to, from);

      /// Yeet to.
      VECTOR_PUSH(removed_instructions, to);
    }

    /// Remove all instructions that were marked for removal.
    VECTOR_FOREACH_PTR (IRInstruction*, instruction, removed_instructions) {
      ir_remove(instruction);
    }

    /// Collect the remaining instructions.
    if (removed_instructions.size) {
      collect_instructions_into(info, instructions, 0);

      /// TODO: Ideally, we don’t want to rebuild the entire matrix on every iteration.
      build_adjacency_graph(info, instructions, G);
    }
    else { break; }
  }

  /// Cleanup.
  VECTOR_DELETE(removed_instructions);
  VECTOR_DELETE(phis);

#else
  IRInstructions instructions_to_remove = {0};
  // Attempt to remove copy instructions.
  for (IRInstructionList *it = *instructions; it; it = it->next) {
    IRInstruction *instruction = it->instruction;
    if (instruction->type == IR_COPY) {
      if (instruction == instruction->value.reference
          || instruction->result
          || !adjm(G->matrix, instruction->index, instruction->value.reference->index)
          ) {
        if (instruction->result == instruction->value.reference->result) {
          // Replace all uses of INSTRUCTION with the thing being copied.
          ir_replace_uses(instruction, instruction->value.reference);
          IRInstructionList *head = calloc(1, sizeof(IRInstructionList));
          head->instruction = instruction;
          head->next = instructions_to_remove;
          instructions_to_remove = head;
          continue;
        }
      } else {
        continue;
      }

      if (instruction->result) {
        // Don't override anything that is already colored.
        if (instruction->value.reference->result) {
          continue;
        }
        // Don't override with interfering register.
        int64_t regmask = info->instruction_register_interference(instruction->value.reference);
        if (regmask & (1 << (instruction->result - 1))) {
          continue;
        }

        instruction->value.reference->result = instruction->result;
      } else {
        // Replace all uses of INSTRUCTION with the thing being copied.
        ir_replace_uses(instruction, instruction->value.reference);
        IRInstructionList *head = calloc(1, sizeof(IRInstructionList));
        head->instruction = instruction;
        head->next = instructions_to_remove;
        instructions_to_remove = head;
      }

      // Also replace all adjacency matrix values regarding COPY instruction with the copied instruction.
      for (IRInstructionList *adj_it = *instructions; adj_it; adj_it = adj_it->next) {
        if (adj_it->instruction == instruction) {
          continue;
        }
        *adjm_entry(G->matrix, instruction->index, adj_it->instruction->index) =
          *adjm_entry(G->matrix, instruction->value.reference->index, adj_it->instruction->index);
      }
    }
  }

  // Remove instructions
  for (; instructions_to_remove; instructions_to_remove = instructions_to_remove->next) {
    // Delete instruction from block.
    IRInstruction *instruction = instructions_to_remove->instruction;
    if (instruction->previous) {
      instruction->previous->next = instruction->next;
    }
    if (instruction->next) {
      instruction->next->previous = instruction->previous;
    }
    if (instruction == instruction->block->instructions) {
      instruction->block->instructions = instruction->next;
    }
    if (instruction == instruction->block->last_instruction) {
      instruction->block->last_instruction = instruction->previous;
    }
    // TODO: Free uses.
    instruction->block = NULL;
    instruction->next = NULL;
    instruction->previous = NULL;
    instruction->uses = NULL;
  }
#endif
}

typedef struct NumberStack {
  size_t number;
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
#define PRINT_NUMBER_STACK(stack) print_number_stack(stack)
#else
#define PRINT_NUMBER_STACK(stack)
#endif

NumberStack *build_coloring_stack(RegisterAllocationInfo *info, IRInstructions *instructions, AdjacencyGraph *G) {
  NumberStack *stack = NULL;

  AdjacencyListNode *array = calloc(G->matrix.size + 1, sizeof(AdjacencyListNode));
  for (size_t i = 0; i < G->matrix.size; ++i) {
    AdjacencyListNode *new_node = array + i;
    AdjacencyListNode *node = G->list + i;
    *new_node = *node;
    for (AdjacencyList *adj_it = node->adjacencies; adj_it; adj_it = adj_it->next) {
      adjl_add_impl(new_node, adj_it->node);
    }
  }

  size_t k = info->register_count;
  size_t count = G->matrix.size;
  while (count) {
    // degree < k rule:
    //   A graph G is k-colorable if, for every node N in G, the degree
    //   of N < k.
    char done = 0;
    do {
      done = 1;
      for (size_t i = 0; i < G->matrix.size; ++i) {
        AdjacencyListNode *node = array + i;
        if (node->color || node->allocated) {
          continue;
        }
        if (node->degree < k) {
          done = 0;

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
      size_t min_cost = (size_t)-1;
      size_t node_to_spill = 0;

      VECTOR_FOREACH_PTR (IRInstruction *, instruction, *instructions) {
        AdjacencyListNode *node = array + instruction->index;
        if (node->color || node->allocated) {
          continue;
        }
        node->spill_cost = node->degree ? (node->spill_cost / node->degree) : 0;
        if (node->degree && node->spill_cost <= min_cost) {
          min_cost = node->spill_cost;
          node_to_spill = instruction->index;
          //node->allocated = 1;
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

void color
(RegisterAllocationInfo *info,
 NumberStack *stack,
 IRInstructions *instructions,
 AdjacencyListNode *array,
 size_t length
 )
{
  for (NumberStack *i = stack; i; i = i->next) {
    AdjacencyListNode *node = array + i->number;
    if (node->color || node->instruction->result) {
      continue;
    }

    Register r = 0;

    size_t k = info->register_count;
    // Each bit that is set refers to register in register list that
    // must not be assigned to this.
    size_t register_interferences = node->regmask;
    register_interferences |=
      info->instruction_register_interference(node->instruction);
    for (AdjacencyList *adj_it = node->adjacencies; adj_it; adj_it = adj_it->next) {
      register_interferences |=
        info->instruction_register_interference(adj_it->node->instruction);
      if (adj_it->node->color) {
        register_interferences |= 1 << (adj_it->node->color - 1);
      }
    }

    for (size_t x = 0; x < k; ++x) {
      if (!(register_interferences & 1 << x)) {
        r = (Register)(x + 1);
        break;
      }
    }

    if (!r) {
      TODO("Can not color graph with %zu colors until stack spilling is implemented!", k);
    }

    node->color = r;
  }

  for (size_t i = 0; i < length; ++i) {
    AdjacencyListNode node = array[i];
    IRInstruction *instruction = node.instruction;
    Register r = node.color;
    if (instruction->type == IR_PHI) {
      VECTOR_FOREACH (IRPhiArgument, phi, instruction->value.phi_arguments) {
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
void track_registers(RegisterAllocationInfo *info) {
  FOREACH_INSTRUCTION (info->context) {
    function->registers_in_use |= 1 << instruction->result;
  }
}

void ra(RegisterAllocationInfo *info) {
  if (!info) { return; }

  ir_set_ids(info->context);
  IR_FEMIT(stdout, info->context);

  phi2copy(info);

  DEBUG("After PHI2COPY\n");
  ir_set_ids(info->context);
  IR_FEMIT(stdout, info->context);

  function_call_arguments(info);
  function_return_values(info);

  fixup_precoloured(info);

  IRInstructions instructions = collect_instructions(info, 0);

  DEBUG("MTX\n");
  ir_set_ids(info->context);
  IR_FEMIT(stdout, info->context);

  AdjacencyGraph G = {0};
  G.order = info->register_count;
  build_adjacency_graph(info, &instructions, &G);

  PRINT_ADJACENCY_MATRIX(G.matrix);

  DEBUG("Before Coalescing\n");
  ir_set_ids(info->context);
  IR_FEMIT(stdout, info->context);

  coalesce(info, &instructions, &G);

  DEBUG("After Coalescing\n");
  ir_set_ids(info->context);
  IR_FEMIT(stdout, info->context);

  instructions = collect_instructions(info, 1);
  build_adjacency_graph(info, &instructions, &G);
  build_adjacency_lists(&instructions, &G);

  ir_set_ids(info->context);
  //PRINT_ADJACENCY_MATRIX(G.matrix);
  //PRINT_ADJACENCY_ARRAY(G.list, G.matrix.size);

  NumberStack *stack = build_coloring_stack(info, &instructions, &G);
  //PRINT_NUMBER_STACK(stack);

  color(info, stack, &instructions, G.list, G.matrix.size);

  //PRINT_INSTRUCTION_LIST(&instructions);

  IR_FEMIT(stdout, info->context);

  track_registers(info);

  if (optimise) codegen_optimise_blocks(info->context);
}
