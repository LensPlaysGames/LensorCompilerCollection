#ifndef IR_H
#define IR_H

#include <codegen/codegen_forward.h>

enum IRInstructionType {
  IR_INSTRUCTION_ADD,
  IR_INSTRUCTION_SUB,
  IR_INSTRUCTION_MUL,
  IR_INSTRUCTION_DIV,
  IR_INSTRUCTION_MOD,
  IR_INSTRUCTION_SHL,
  IR_INSTRUCTION_SAR,

  IR_INSTRUCTION_ALLOCA,
  IR_INSTRUCTION_CALL,
  IR_INSTRUCTION_COMMENT,
  IR_INSTRUCTION_COMPARISON,
  IR_INSTRUCTION_BRANCH,
  IR_INSTRUCTION_BRANCH_IF,
  IR_INSTRUCTION_IMMEDIATE,
  IR_INSTRUCTION_PHI,
  IR_INSTRUCTION_RETURN,
  IR_INSTRUCTION_FUNCTION_REF,

  IR_INSTRUCTION_GLOBAL_REF,
  IR_INSTRUCTION_GLOBAL_VAL,
  IR_INSTRUCTION_STORE_GLOBAL,
  IR_INSTRUCTION_LOCAL_REF,
  IR_INSTRUCTION_LOCAL_VAL,
  IR_INSTRUCTION_STORE_LOCAL,
  IR_INSTRUCTION_STORE,
  IR_INSTRUCTION_PARAM_REF,

  IR_INSTRUCTION_COUNT
};

struct Function {
  char *name;
  BasicBlock *entry;
  BasicBlock *last;
  Function *next;
  Value* return_value;
};

struct BasicBlock {
  Function *parent;
  BasicBlock *next;
  BasicBlock *prev;
  /// Linked list of values.
  Value *values;
  /// The last value in the linked list.
  Value *end;
  char closed;
};

typedef struct FunctionCall {
  enum FunctionCallType {
    FUNCTION_CALL_TYPE_INTERNAL,
    FUNCTION_CALL_TYPE_EXTERNAL
  } type;
  Value *callee;
  Value *args;
  /// Architecture-specific data.
  void *arch_call_data;
} FunctionCall;

typedef struct PHINodeEntry {
  BasicBlock *block;
  Value *value;
  struct PHINodeEntry *next;
} PHINodeEntry;

typedef struct Variable {
  long long int local_offset;
  size_t size;
} Variable;

typedef struct CondBranch {
  BasicBlock *true_branch;
  BasicBlock *false_branch;
} CondBranch;

typedef struct Comparison {
  enum ComparisonType type;
  Value *lhs;
  Value *rhs;
} Comparison;

typedef struct ParamRef {
  Function *func;
  size_t index;
} ParamRef;

typedef struct GlobalStore {
  const char *name;
  Value *value;
} GlobalStore;

struct Value {
  enum IRInstructionType type;
  BasicBlock *parent;

  // FIXME(Sirraide): There is such a thing as too many linked lists...
  Value *next;
  Value *prev;
  Value *next_in_block;
  Value *prev_in_block;

  union {
    char *comment_value;
    const char *global_name;
    PHINodeEntry* phi_entries;
    BasicBlock *branch_target;
    Function *function_ref;
    Value *local_ref;
    long long int immediate;

    struct {
      Value *lhs;
      Value *rhs;
    };

    FunctionCall call_value;
    Variable variable_value;
    CondBranch cond_branch_value;
    Comparison comparison;
    ParamRef param_ref;
    GlobalStore global_store;
  };
};

#endif // IR_H
