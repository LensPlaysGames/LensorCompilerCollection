#ifndef INTERMEDIATE_REPRESENTATION_H
#define INTERMEDIATE_REPRESENTATION_H

#include <codegen.h>
#include <codegen/codegen_forward.h>
#include <inttypes.h>
#include <stdbool.h>
#include <vector.h>

#define INSTRUCTION(name, given_type)                       \
  IRInstruction *(name) = calloc(1, sizeof(IRInstruction)); \
  ASSERT((name), "Could not allocate new IRInstruction.");  \
  (name)->type = (given_type);

#define FOREACH_INSTRUCTION_N(context, function, block, instruction)  \
  VECTOR_FOREACH_PTR (IRFunction *, function, *context->functions)    \
    DLIST_FOREACH(IRBlock *, block, function->blocks)                 \
      DLIST_FOREACH(IRInstruction *, instruction, block->instructions)

#define FOREACH_INSTRUCTION_IN_FUNCTION_N(function, block) \
  DLIST_FOREACH(IRBlock *, block, function->blocks)                 \
    DLIST_FOREACH(IRInstruction *, instruction, block->instructions)

#define FOREACH_INSTRUCTION(context) FOREACH_INSTRUCTION_N(context, function, block, instruction)
#define FOREACH_INSTRUCTION_IN_FUNCTION(function) FOREACH_INSTRUCTION_IN_FUNCTION_N(function, block)

typedef enum IRType {
  IR_IMMEDIATE,
  IR_CALL,
  IR_LOAD,

  IR_RETURN,
  IR_BRANCH,
  IR_BRANCH_CONDITIONAL,
  IR_UNREACHABLE,

  IR_PHI,
  IR_COPY,

  IR_ADD,
  IR_SUBTRACT,
  IR_MULTIPLY,
  IR_DIVIDE,
  IR_MODULO,

  IR_SHIFT_LEFT,
  IR_SHIFT_RIGHT_ARITHMETIC,
  IR_SHIFT_RIGHT_LOGICAL,

  IR_LOCAL_LOAD,
  IR_LOCAL_STORE,
  IR_LOCAL_ADDRESS,

  IR_GLOBAL_LOAD,
  IR_GLOBAL_STORE,
  IR_GLOBAL_ADDRESS,

  /// Store data at an address.
  IR_STORE,

  IR_COMPARISON,

  IR_PARAMETER_REFERENCE,

  // A lot of backends have these instructions, but the IR isn't
  // generated with them in it.
  IR_REGISTER,
  IR_STACK_ALLOCATE,

  IR_COUNT
} IRType;

typedef struct IRPhiArgument {
  /// The value of the argument itself.
  IRInstruction *value;
  /// Stores the predecessor to the Phi node in the direction of the
  /// argument assignment.
  ///    [a]
  ///  [t] [o]
  ///    \ [b]
  ///    [j]
  /// For example, if arg->value->block == o, then arg->block == b.
  IRBlock *block;
} IRPhiArgument;

typedef struct IRPair {
  IRInstruction *car;
  IRInstruction *cdr;
} IRPair;

typedef struct IRCallArgument {
  IRInstruction *value;
  struct IRCallArgument *next;
} IRCallArgument;

typedef enum IRCallType {
  IR_CALLTYPE_DIRECT,
  IR_CALLTYPE_INDIRECT,
  IR_CALLTYPE_COUNT
} IRCallType;

typedef union IRCallValue {
  const char *name;
  IRInstruction *callee;
} IRCallValue;

typedef struct IRCall {
  IRCallType type;
  IRCallValue value;
  IRCallArgument *arguments;
  bool tail_call;
} IRCall;

typedef struct IRBranchConditional {
  IRInstruction *condition;
  IRBlock *true_branch;
  IRBlock *false_branch;
} IRBranchConditional;

typedef struct IRComparison {
  enum ComparisonType type;
  IRPair pair;
} IRComparison;

typedef struct IRGlobalAssignment {
  IRInstruction *new_value;
  char *name;
} IRGlobalAssignment;

typedef struct IRStackAllocation {
  size_t size;
  size_t offset;
} IRStackAllocation;

typedef union IRValue {
  IRBlock *block;
  IRInstruction *reference;
  int64_t immediate;
  IRCall call;
  VECTOR(IRPhiArgument*) phi_arguments; /// For unfortunate reasons, these *have* to be on the heap.
  IRBranchConditional conditional_branch;
  IRPair pair;
  IRComparison comparison;
  char *name;
  IRGlobalAssignment global_assignment;
  IRStackAllocation stack_allocation;
} IRValue;


void mark_used(IRInstruction *usee, IRInstruction *user);

void set_pair_and_mark
(IRInstruction *parent,
 IRPair *pair,
 IRInstruction *lhs,
 IRInstruction *rhs);


typedef struct IRInstruction {
  int type;
  IRValue value;

  /// A unique identifier (mainly for debug purposes).
  size_t id;

  IRBlock *block;

  DLIST_NODE(struct IRInstruction);

  // Register allocation.
  size_t index;

  Register result;

  /// Sometimes we don’t want to allocate a register for an instruction
  /// or emit it, but we still want to use it as an operand and keep it
  /// in the block so that we don’t leak it.
  ///
  /// An example of this are comparisons that are immediately used in
  /// a conditional branch, and unused after.
  ///
  /// This is a bit of a hack, but it works. A dedicated instruction
  /// selection pass would be better.
  bool dont_emit;

  /// List of instructions using this instruction.
  VECTOR(IRInstruction*) users;
} IRInstruction;

/// A block is a list of instructions that have control flow enter at
/// the beginning and leave at the end.
typedef struct IRBlock {
  const char *name;

  DLIST(IRInstruction) instructions;

  /// A pointer to the function the block is attached to, or NULL if
  /// detached.
  IRFunction *function;

  DLIST_NODE(struct IRBlock);

  // Unique ID (among blocks)
  size_t id;
  // For the backend.
  bool done;
} IRBlock;

typedef struct IRFunction {
  char *name;

  DLIST(IRBlock) blocks;

  // Unique ID (among functions)
  size_t id;

  // Used by certain backends.
  size_t locals_total_size;

  size_t registers_in_use;

  uint32_t attr_consteval : 1;
  uint32_t attr_forceinline : 1;
  uint32_t attr_global : 1;
  uint32_t attr_leaf : 1;
  uint32_t attr_noreturn : 1;
  uint32_t attr_pure : 1;
  uint32_t : 26;
} IRFunction;

struct IR {
  VECTOR (IRFunction*) functions;
};

void ir_set_ids(CodegenContext *context);

bool ir_is_branch(IRInstruction*);

/// Check whether a block is closed.
bool ir_is_closed(IRBlock *block);

void ir_femit_instruction
(FILE *file,
 IRInstruction *instruction);
void ir_femit_block
(FILE *file,
 IRBlock *block);
void ir_femit_function
(FILE *file,
 IRFunction *function);
void ir_femit
(FILE *file,
 CodegenContext *context);

void ir_add_function_call_argument
(CodegenContext *context,
 IRInstruction *call,
 IRInstruction *argument);

IRBlock *ir_block_create();

void ir_block_attach_to_function
(IRFunction *function,
 IRBlock *new_block);

void ir_block_attach
(CodegenContext *context,
 IRBlock *new_block);

IRFunction *ir_function_create();
IRFunction *ir_function(CodegenContext *context, const char *name);

void ir_force_insert_into_block
(IRBlock *block,
 IRInstruction *new_instruction);

void ir_insert_into_block
(IRBlock *block,
 IRInstruction *new_instruction);

void ir_insert
(CodegenContext *context,
 IRInstruction *new_instruction);

void insert_instruction_before(IRInstruction *i, IRInstruction *before);
void insert_instruction_after(IRInstruction *i, IRInstruction *after);

IRInstruction *ir_parameter_reference
(CodegenContext *context,
 int64_t index);

void ir_phi_add_argument
(IRInstruction *phi,
 IRPhiArgument *argument);

void ir_phi_argument
(IRInstruction *phi,
 IRBlock *phi_predecessor,
 IRInstruction *argument);

void ir_phi_remove_argument(IRInstruction *phi, IRBlock *block);

IRInstruction *ir_phi
(CodegenContext *context);

/// NOTE: Does not insert call instruction.
IRInstruction *ir_direct_call
(CodegenContext *context,
 char* function_name);

/// NOTE: Does not insert call instruction.
IRInstruction *ir_indirect_call
(CodegenContext *context,
 IRInstruction *function);

IRInstruction *ir_load_global_address
(CodegenContext *context,
 char *name);

IRInstruction *ir_immediate
(CodegenContext *context,
 int64_t immediate);

IRInstruction *ir_load
(CodegenContext *context,
 IRInstruction *address);

IRInstruction *ir_load_local_address
(CodegenContext *context,
 IRInstruction *local);

IRInstruction *ir_load_global
(CodegenContext *context,
 char *name);

IRInstruction *ir_load_local
(CodegenContext *context,
 IRInstruction *local);

IRInstruction *ir_store_global
(CodegenContext *context,
 IRInstruction *source,
 char *name);

IRInstruction *ir_store_local
(CodegenContext *context,
 IRInstruction *source,
 IRInstruction *local);

IRInstruction *ir_store
(CodegenContext *context,
 IRInstruction *data,
 IRInstruction *address);

IRInstruction *ir_branch_conditional
(CodegenContext *context,
 IRInstruction *condition,
 IRBlock *then_block,
 IRBlock *otherwise_block);

IRInstruction *ir_branch
(CodegenContext *context,
 IRBlock *destination);

IRInstruction *ir_branch_into_block
(IRBlock *destination,
 IRBlock *block);

IRInstruction *ir_return
(CodegenContext *context,
 IRInstruction *return_value);

IRInstruction *ir_copy_unused
(CodegenContext *context,
 IRInstruction *source);

IRInstruction *ir_copy
(CodegenContext *context,
 IRInstruction *source);

IRInstruction *ir_comparison
(CodegenContext *context,
 enum ComparisonType type,
 IRInstruction *lhs,
 IRInstruction *rhs);

IRInstruction *ir_add
(CodegenContext *context,
 IRInstruction *lhs,
 IRInstruction *rhs);

IRInstruction *ir_subtract
(CodegenContext *context,
 IRInstruction *lhs,
 IRInstruction *rhs);

IRInstruction *ir_multiply
(CodegenContext *context,
 IRInstruction *lhs,
 IRInstruction *rhs);

IRInstruction *ir_divide
(CodegenContext *context,
 IRInstruction *lhs,
 IRInstruction *rhs);

IRInstruction *ir_modulo
(CodegenContext *context,
 IRInstruction *lhs,
 IRInstruction *rhs);

IRInstruction *ir_shift_left
(CodegenContext *context,
 IRInstruction *lhs,
 IRInstruction *rhs);

IRInstruction *ir_shift_right_arithmetic
(CodegenContext *context,
 IRInstruction *lhs,
 IRInstruction *rhs);

IRInstruction *ir_stack_allocate
(CodegenContext *context,
 int64_t size);

IRFunction *ir_get_function
(CodegenContext *context,
 const char *name);

/// Replace all uses of instruction with replacement.
void ir_replace_uses(IRInstruction *instruction, IRInstruction *replacement);

/// Remove this instruction from the users lists of its children.
void ir_unmark_usees(IRInstruction *instruction);

/// Remove an instruction from the AST and free it.
/// Used by the optimiser.
void ir_remove(IRInstruction* instruction);
void ir_remove_use(IRInstruction *usee, IRInstruction *user);
void ir_remove_and_free_block(IRBlock *block);

/// Mark a block as ending w/ `unreachable` and remove it
/// from PHIs.
void ir_mark_unreachable(IRBlock *block);

#endif /* INTERMEDIATE_REPRESENTATION_H */
