#ifndef INTERMEDIATE_REPRESENTATION_H
#define INTERMEDIATE_REPRESENTATION_H

#include <codegen.h>
#include <codegen/codegen_forward.h>
#include <inttypes.h>

#define INSTRUCTION(name, given_type)                       \
  IRInstruction *(name) = calloc(1, sizeof(IRInstruction)); \
  ASSERT((name), "Could not allocate new IRInstruction.");  \
  (name)->type = (given_type);

typedef enum IRType {
  IR_IMMEDIATE,
  IR_CALL,
  IR_LOAD,

  IR_RETURN,
  IR_BRANCH,
  IR_BRANCH_CONDITIONAL,

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
  // A linked list of arguments.
  struct IRPhiArgument *next;
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
  char *name;
  IRInstruction *callee;
} IRCallValue;

typedef struct IRCall {
  IRCallType type;
  IRCallValue value;
  IRCallArgument *arguments;
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
  IRPhiArgument *phi_argument;
  IRBranchConditional conditional_branch;
  IRPair pair;
  IRComparison comparison;
  char *name;
  IRGlobalAssignment global_assignment;
  IRStackAllocation stack_allocation;
} IRValue;


typedef struct Use {
  IRInstruction *user;
  struct Use *next;
} Use;

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

  // Register allocation.
  size_t index;

  Register result;

  // List of uses---instructions that use this instruction should go in
  // this list.
  Use *uses;

  // Doubly linked list.
  struct IRInstruction *previous;
  struct IRInstruction *next;
} IRInstruction;

typedef struct IRBlockPredecessor {
  IRBlock *block;
  struct IRBlockPredecessor *next;
} IRBlockPredecessor;

/// A block is a list of instructions that have control flow enter at
/// the beginning and leave at the end.
typedef struct IRBlock {
  const char *name;

  IRInstruction *instructions;
  IRInstruction *last_instruction;

  IRInstruction *branch;

  IRBlockPredecessor *predecessor;

  /// A pointer to the function the block is attached to, or NULL if
  /// detached.
  IRFunction *function;

  // Doubly linked list.
  struct IRBlock *previous;
  struct IRBlock *next;

  // Unique ID (among blocks)
  size_t id;
} IRBlock;

typedef struct IRFunction {
  const char *name;

  IRBlock *first;
  IRBlock *last;

  IRInstruction *return_value;

  // Linked list.
  struct IRFunction *next;

  // Unique ID (among functions)
  size_t id;

  // Used by certain backends.
  size_t locals_total_size;

  int64_t registers_in_use;
} IRFunction;

void ir_set_ids(CodegenContext *context);

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

void ir_insert_into_block
(IRBlock *block,
 IRInstruction *new_instruction);

void ir_insert
(CodegenContext *context,
 IRInstruction *new_instruction);

/// Insert instruction A before instruction B
void insert_instruction_before(IRInstruction *a, IRInstruction *b);

/// Insert instruction A after instruction B
void insert_instruction_after(IRInstruction *a, IRInstruction *b);

void ir_phi_argument
(IRInstruction *phi,
 IRBlock *phi_predecessor,
 IRInstruction *argument);

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
(CodegenContext *context);

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

/// Remove an instruction from the AST and free it.
/// Used by the optimiser.
void ir_remove(IRInstruction* instruction);
void ir_remove_use(IRInstruction *usee, IRInstruction *user);

#endif /* INTERMEDIATE_REPRESENTATION_H */
