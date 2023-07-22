#ifndef INTERCEPT_IR_IMPL_H
#define INTERCEPT_IR_IMPL_H

#include <ir/ir.h>

///
/// Internal header. Do not include this in non-IR-implementation files.
///

typedef struct IRCall {
  IRInstructionVector arguments;
  // TODO: Make this a named union!
  union {
    IRInstruction *callee_instruction;
    IRFunction *callee_function;
  };
  enum IntrinsicKind intrinsic; /// Only used by intrinsic calls.
  bool is_indirect : 1;
  bool tail_call : 1;
  bool force_inline : 1; /// Annotated with __builtin_inline().
} IRCall;

typedef struct IRBranchConditional {
  IRInstruction *condition;
  IRBlock *then;
  IRBlock *else_;
} IRBranchConditional;

typedef struct IRStackAllocation {
  usz size; /// FIXME: REMOVE. Should be unnecessary since we know the type of the allocation.
  usz offset;
} IRStackAllocation;

void mark_used(IRInstruction *usee, IRInstruction *user);

typedef struct IRInstruction {
  enum IRType kind;

  Register result;
  MIRInstruction *machine_inst;

  Type *type;

  u32 id;

  /// List of instructions using this instruction.
  InstructionVector users;

  IRBlock *parent_block;

  /// Source location of the instruction.
  loc source_location;

  union {
    IRBlock *destination_block;
    IRInstruction *operand;
    u64 imm;
    IRCall call;
    Vector(IRPhiArgument) phi_args;
    IRBranchConditional cond_br;
    struct {
      IRInstruction *addr;
      IRInstruction *value;
    } store;
    struct {
      IRInstruction *lhs;
      IRInstruction *rhs;
    };
    IRStaticVariable* static_ref;
    IRFunction *function_ref;
    IRStackAllocation alloca;
    CodegenContext *ctx; /// Used by IR_POISON.
    struct {
      string str;
      usz string_index;
    };
  };
} IRInstruction;

/// A block is a list of instructions that have control flow enter at
/// the beginning and leave at the end.
typedef struct IRBlock {
  string name;

  IRInstructionVector instructions;

  /// A pointer to the function the block is attached to, or NULL if
  /// detached.
  IRFunction *function;

  // Unique ID (among blocks)
  u32 id;

  // MIRBlock that was created to represent this IRBlock.
  MIRBlock *machine_block;

  // For the backend.
  bool done;
} IRBlock;

typedef struct IRFunction {
  string name;

  IRBlockVector blocks;

  /// Cached parameter instructions.
  IRInstructionVector parameters;

  loc source_location;

  /// Pointer to the context that owns this function.
  CodegenContext *context;

  /// The type of the function.
  Type *type;

  // Unique ID (among functions)
  usz id;

  // MIRFunction that was created to represent this IRFunction.
  MIRFunction *machine_func;

  usz registers_in_use;

  SymbolLinkage linkage;

#define def_function_attr(_, name) bool attr_##name : 1;
    SHARED_FUNCTION_ATTRIBUTES(def_function_attr)
    IR_FUNCTION_ATTRIBUTES(def_function_attr)
#undef def_function_attr
} IRFunction;

struct IR {
  Vector(IRFunction*) functions;
};

/// Get the index of an instruction in a block. The instruction
/// must have a parent block.
usz index_in_block(IRInstruction* inst);

/// Check if an instruction returns a value.
bool ir_is_value(IRInstruction *instruction);

/// Print the defun signature of a function.
void ir_print_defun(FILE *file, IRFunction *function);

/// Free memory used by an instruction. This is unsafe as
/// it doesn’t check whether the instruction is used by other
/// instructions, so use this only when freeing the entire IR.
void ir_free_instruction_data(IRInstruction *instruction);

/// Iterate over each child of an instruction.
///
/// \param instruction The instruction to iterate over.
/// \param callback A callback that is called with the instruction and the child.
/// \param data User data that is passed to the callback.
void ir_for_each_child(
    IRInstruction *inst,
    void callback(IRInstruction *user, IRInstruction **child, void *data),
    void *data
);

/// Do not use these unless you know what you’re doing.
void mark_used(IRInstruction *usee, IRInstruction *user);
void remove_use(IRInstruction *usee, IRInstruction *user);

#endif // INTERCEPT_IR_IMPL_H
