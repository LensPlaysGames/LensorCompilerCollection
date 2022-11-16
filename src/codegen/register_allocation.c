#include <codegen/register_allocation.h>

#include <codegen.h>
#include <error.h>
#include <stdlib.h>
#include <string.h>
#include <codegen/intermediate_representation.h>

//#define DEBUG_RA

#ifdef DEBUG_RA
#define IR_FEMIT(file, context) ir_femit(file, context)
#else
#define IR_FEMIT(file, context)
#endif

RegisterAllocationInfo *ra_allocate_info
(CodegenContext *context,
 Register result_register,
 size_t general_registers_count,
 Register *general_registers,
 size_t argument_registers_count,
 Register *argument_registers
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

  return info;
}

/// Insert instruction A before instruction B
void insert_instruction_before(IRInstruction *a, IRInstruction *b) {
  if (!a || !b) { return; }

  // 0 - b - 1
  // 0 - a - b - 1

  a->previous = b->previous;

  if (b->previous) {
    b->previous->next = a;
  }
  b->previous = a;

  a->next = b;

  a->block = b->block;

  // Handle beginning of block/function stuffs.
  if (b->block->instructions == b) {
    b->block->instructions = a;
  }
  // TODO: Update entry instruction of function, if needed.
}


/// Insert instruction A after instruction B
void insert_instruction_after(IRInstruction *a, IRInstruction *b) {
  if (!a || !b) { return; }

  if (b == b->block->branch) {
    PANIC("Can not insert instruction *after* the branch instruction of a basic block.");
  }

  // 0 - b - 1
  // 0 - b - a - 1

  if (b->next) {
    b->next->previous = a;
  }
  a->next = b->next;
  b->next = a;

  a->previous = b;

  a->block = b->block;

  // Handle end of block/function stuffs.
  if (b->block->last_instruction == b) {
    b->block->last_instruction = a;
  }
  // TODO: Update return value of function, if needed.
}

//==== BEG REGISTER ALLOCATION PASSES ====

// TODO: Siraide said this is all horribly wrong, and he is right.
void phi2copy(RegisterAllocationInfo *info) {
  for (IRFunction *function = info->context->all_functions;
       function;
       function = function->next
       ) {
    for (IRBlock *block = function->first;
         block;
         block = block->next
         ) {
      IRBlock* last_block = NULL;
      for (IRInstruction *instruction = block->instructions;
           instruction;
           instruction = instruction->next
           ) {
        if (instruction->type == IR_PHI) {
          ASSERT(instruction->block != last_block,
                 "Multiple PHI instructions in a single block are not allowed within register allocation!");
          IRPhiArgument *phi = instruction->value.phi_argument;

          // Single PHI argument means that we can replace it with a
          // simple copy.
          if (phi && !phi->next) {
            instruction->type = IR_COPY;
            instruction->value.reference = phi->value;
            continue;
          }

          // For each of the PHI arguments, we basically insert a copy.
          // Where we insert it depends on some complicated factors
          // that have to do with control flow.
          for (; phi; phi = phi->next) {
            IRInstruction *argument = phi->value;
            IRInstruction *copy = ir_copy(info->context, argument);

            switch (argument->block->branch->type) {
            default:
              insert_instruction_after(copy, argument);
              break;
            case IR_BRANCH_CONDITIONAL:
              if (0) {}
              IRBlock *critical_edge_trampoline = ir_block_create();
              ir_insert_into_block(critical_edge_trampoline, copy);
              IRInstruction *new_branch = ir_branch_into_block(instruction->block, critical_edge_trampoline);

              ASSERT(critical_edge_trampoline->branch, "branch null");
              ASSERT(critical_edge_trampoline, "HERE");

              ir_block_attach_to_function(instruction->block->function, critical_edge_trampoline);
              if (argument->block->branch->value.conditional_branch.true_branch == instruction->block) {
                argument->block->branch->value.conditional_branch.true_branch = critical_edge_trampoline;
              } else {
                argument->block->branch->value.conditional_branch.false_branch = critical_edge_trampoline;
              }
              break;
            }

            mark_used(argument, copy);
            mark_used(copy, instruction);

            // Remove use of PHI argument in PHI; it's now used in the
            // copy instead.
            Use *previous_use = NULL;
            for (Use *use = argument->uses;
                 use;
                 previous_use = use,
                   use = use->next
                 ) {
              if (use->user == instruction) {
                if (previous_use) {
                  previous_use->next = use->next;
                } else {
                  argument->uses = use;
                }
              }
            }

            phi->value = copy;
          }
        }
      }
    }
  }
}

void function_call_arguments(RegisterAllocationInfo *info) {
  for (IRFunction *function = info->context->all_functions;
       function;
       function = function->next
       ) {
    for (IRBlock *block = function->first;
         block;
         block = block->next
         ) {
      for (IRInstruction *instruction = block->instructions;
           instruction;
           instruction = instruction->next
           ) {
        if (instruction->type == IR_CALL) {
          IRCallArgument *arguments = instruction->value.call.arguments;
          for (size_t i = 0; arguments; ++i, arguments = arguments->next) {
            if (i >= info->argument_register_count) {
              TODO("Handle stack allocated function parameters, somehow :p");
            }
            IRInstruction *argument = arguments->value;
            Register result = info->argument_registers[i];
            IRInstruction *arg_copy = ir_copy(info->context, argument);
            arg_copy->result = result;
            insert_instruction_before(arg_copy, instruction);
            arguments->value = arg_copy;
          }
        }
      }
    }
  }
}

void function_return_values(RegisterAllocationInfo *info) {
  for (IRFunction *function = info->context->all_functions;
       function;
       function = function->next
       ) {
    function->return_value->result = info->result_register;
  }
}

//==== END REGISTER ALLOCATION PASSES ====

/// Return non-zero iff given instruction needs a register.
char needs_register(IRInstruction *instruction) {
  if (!instruction) {
    return 0;
  }

  switch(instruction->type) {
  case IR_ADD:
  case IR_SUBTRACT:
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
    return 1;
    break;
  default:
    return 0;
    break;
  }
  return 0;
}

//==== BEG INSTRUCTION LIST ====

typedef struct IRInstructionList {
  IRInstruction *instruction;
  struct IRInstructionList *next;
} IRInstructionList;

IRInstructionList *collect_instructions(RegisterAllocationInfo *info) {
  size_t index = 0;
  IRInstructionList *list = NULL;
  IRInstructionList *list_it = NULL;
  for (IRFunction *function = info->context->all_functions;
       function;
       function = function->next
       ) {
    for (IRBlock *block = function->first;
         block;
         block = block->next
         ) {
      for (IRInstruction *instruction = block->instructions;
           instruction;
           instruction = instruction->next
           ) {
        // Add instruction to flat list iff instruction needs register
        // allocated.
        if (needs_register(instruction)) {
          if (list_it) {
            list_it->next = calloc(1, sizeof(IRInstructionList));
            list_it = list_it->next;
          } else {
            list_it = calloc(1, sizeof(IRInstructionList));
            list = list_it;
          }
          instruction->index = index++;
          list_it->instruction = instruction;
        }
      }
    }
  }

  return list;
}

void print_instruction_list(IRInstructionList *list) {
  for (; list; list = list->next) {
    ir_femit_instruction(stdout, list->instruction);
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

AdjacencyMatrix adjm_create(size_t size) {
  AdjacencyMatrix m;
  m.size = size;
  m.data = calloc(1, size * size + 1);
  return m;
}

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
  AdjacencyListNode *list;
} AdjacencyGraph;

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

/// If the definition of B lies between the definition of A and any one
/// of its uses, then A and B interfere.
char instructions_interfere(IRInstruction *A, IRInstruction *B) {
  ASSERT(A && B, "Can not get interference of NULL instructions.");
  ASSERT(A->block, "Can not get interference when A has NULL containing block.");
  for (Use *a_use = A->uses; a_use; a_use = a_use->next) {
    ASSERT(a_use->user, "Use instruction can not be NULL!");

    // If definition and use of A are in the same block, and if the
    // definition of B is also in the same block, it is a simple index
    // comparison to check for interference.
    if (A->block == a_use->user->block) {
      return B->block == A->block
        && A->index < B->index
        && a_use->user->index > B->index;
    } else {
      // A and B are not defined in the same block, follow control flow.
      if (follow_control_flow(NULL, A->block, a_use->user, B) == RA_CF_INTERFERE) {
        return 1;
      }
    }
  }
  return 0;
}

void build_adjacency_matrix(RegisterAllocationInfo *info, IRInstructionList *instructions, AdjacencyGraph *G) {
  ASSERT(instructions, "Can not build adjacency matrix of NULL instruction list.");
   IRInstructionList *last = instructions;
   while (last->next) {
     last = last->next;
   }
  size_t size = last->instruction->index + 1;
  G->matrix = adjm_create(size);
  for(IRInstructionList *A = instructions; A; A = A->next) {
    for(IRInstructionList *B = instructions; B; B = B->next) {
      if (A == B) { continue; }
      if (adjm(G->matrix, A->instruction->index, B->instruction->index)) {
        continue;
      }
      if (instructions_interfere(A->instruction, B->instruction)) {
        adjm_set(G->matrix, A->instruction->index, B->instruction->index);
      }
    }
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
  size_t color;

  // Degree refers to how many adjacencies this vertex/node has.
  size_t degree;

  AdjacencyList *adjacencies;

  IRInstruction *instruction;

  // Unique integer index.
  size_t index;

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

void build_adjacency_lists(RegisterAllocationInfo *info, IRInstructionList *instructions, AdjacencyGraph *G) {
  G->list = calloc(G->matrix.size + 1, sizeof(AdjacencyListNode));

  for (IRInstructionList *it = instructions; it; it = it->next) {
    G->list[it->instruction->index].index = it->instruction->index;
    G->list[it->instruction->index].color = it->instruction->result;
    G->list[it->instruction->index].instruction = it->instruction;
  }

  for (IRInstructionList *A = instructions; A; A = A->next) {
    for (IRInstructionList *B = instructions; B; B = B->next) {
      if (A->instruction == B->instruction) {
        continue;
      }
      if (adjm(G->matrix, A->instruction->index, B->instruction->index)) {
       adjl_add_impl(&G->list[A->instruction->index], &G->list[B->instruction->index]);
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
      printf("(r%zu) ", it.color);
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

void replace_use(IRInstruction *usee, Use *use, IRInstruction *replacement) {
  if (use->user == replacement) {
    return;
  }

  ASSERT(IR_COUNT == 18);
  switch (use->user->type) {
  case IR_PHI:
    for (IRPhiArgument *phi = use->user->value.phi_argument; phi; phi = phi->next) {
      if (phi->value == usee) {
        phi->value = replacement;
      }
    }
    break;
  case IR_LOAD:
  case IR_LOCAL_ADDRESS:
  case IR_LOCAL_LOAD:
  case IR_COPY:
    if (use->user->value.reference == usee) {
      use->user->value.reference = replacement;
    }
    break;
  case IR_GLOBAL_STORE:
    if (use->user->value.global_assignment.new_value == usee) {
      use->user->value.global_assignment.new_value = replacement;
    }
    break;
    //case IR_STORE:
  case IR_LOCAL_STORE:
  case IR_COMPARISON:
  case IR_ADD:
  case IR_SUBTRACT:
    if (use->user->value.pair.car == usee) {
      use->user->value.pair.car = replacement;
    }
    if (use->user->value.pair.cdr == usee) {
      use->user->value.pair.cdr = replacement;
    }
    break;
  case IR_CALL:
    if (use->user->value.call.type == IR_CALLTYPE_INDIRECT) {
      if (use->user->value.call.value.callee == usee) {
        use->user->value.call.value.callee = replacement;
      }
    }
    break;
  case IR_BRANCH_CONDITIONAL:
    if (use->user->value.conditional_branch.condition == usee) {
      use->user->value.conditional_branch.condition = replacement;
    }
    break;
  case IR_PARAMETER_REFERENCE:
  case IR_GLOBAL_ADDRESS:
  case IR_GLOBAL_LOAD:
  case IR_IMMEDIATE:
  case IR_BRANCH:
  case IR_RETURN:
    break;
  default:
    TODO("Handle IR instruction type to be able to replace uses.");
    break;
  }
}

void replace_uses(IRInstruction *instruction, IRInstruction *replacement) {
  for (Use *use = instruction->uses; use; use = use->next) {
    replace_use(instruction, use, replacement);
  }
}

void coalesce(RegisterAllocationInfo *info, IRInstructionList **instructions, AdjacencyGraph *G) {
  IRInstructionList *instructions_to_remove = NULL;

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
          replace_uses(instruction, instruction->value.reference);
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

        instruction->value.reference->result = instruction->result;
      } else {
        // Replace all uses of INSTRUCTION with the thing being copied.
        replace_uses(instruction, instruction->value.reference);
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

NumberStack *build_coloring_stack(RegisterAllocationInfo *info, IRInstructionList *instructions, AdjacencyGraph *G) {
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

      for (IRInstructionList *it = instructions; it; it = it->next) {
        AdjacencyListNode *node = array + it->instruction->index;
        if (node->color || node->allocated) {
          continue;
        }
        node->spill_cost = node->degree ? (node->spill_cost / node->degree) : 0;
        if (node->degree && node->spill_cost <= min_cost) {
          min_cost = node->spill_cost;
          node_to_spill = it->instruction->index;
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
 IRInstructionList *instructions,
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
    // Each byte refers to register in register list that must not be
    // assigned to this.
    char *register_interferences = calloc(1, k);

    for (AdjacencyList *adj_it = node->adjacencies; adj_it; adj_it = adj_it->next) {
      if (adj_it->node->color) {
        register_interferences[adj_it->node->color - 1] = 1;
      }
    }

    for (size_t x = 0; x < k; ++x) {
      if (!register_interferences[x]) {
        r = x + 1;
        break;
      }
    }

    free(register_interferences);

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
      for (IRPhiArgument *phi = instruction->value.phi_argument; phi; phi = phi->next) {
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

void ra(RegisterAllocationInfo *info) {
  if (!info) { return; }

  phi2copy(info);

  function_call_arguments(info);
  function_return_values(info);

  ir_set_ids(info->context);
  IR_FEMIT(stdout, info->context);

  IRInstructionList *instructions = collect_instructions(info);

  AdjacencyGraph G;
  G.order = info->register_count;
  build_adjacency_matrix(info, instructions, &G);

  PRINT_ADJACENCY_MATRIX(G.matrix);

  coalesce(info, &instructions, &G);

  ir_set_ids(info->context);
  IR_FEMIT(stdout, info->context);

  instructions = collect_instructions(info);
  build_adjacency_matrix(info, instructions, &G);
  build_adjacency_lists(info, instructions, &G);

  ir_set_ids(info->context);
  PRINT_ADJACENCY_MATRIX(G.matrix);
  PRINT_ADJACENCY_ARRAY(G.list, G.matrix.size);

  NumberStack *stack = build_coloring_stack(info, instructions, &G);
  PRINT_NUMBER_STACK(stack);

  color(info, stack, instructions, G.list, G.matrix.size);

  PRINT_INSTRUCTION_LIST(instructions);

  IR_FEMIT(stdout, info->context);
}
