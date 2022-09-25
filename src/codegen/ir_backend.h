#ifndef IR_BACKEND_H
#define IR_BACKEND_H

#include <codegen/ir.h>

/// Create a COPY instruction that copies `v`.
Value *create_copy(CodegenContext *context, Value *v);

/// A backend should call this before emitting a function. Do not call this elsewhere.
void codegen_function_finalise(CodegenContext *context, Function *f);

/// Attach a basic block to a function.
void codegen_basic_block_attach_to(CodegenContext *context, BasicBlock* block, Function *f);

/// Remove an instruction from its parent.
void delete_from_block(Value *v);

/// Remove an instruction from its parent and free it.
void delete_value(Value *v);

/// Insert a value into the current basic block.
void insert(CodegenContext *context, Value *value);

/// Insert `value_to_insert` before `value`.
void insert_before(Value* value, Value *value_to_insert);

/// Insert `value_to_insert` after `value`.
void insert_after(Value *value, Value *value_to_insert);

/// Mark a value as used by another value.
void mark_used_by(Value *value, Value *parent);

/// IR sanity checks go here.
void typecheck_ir(CodegenContext *context, Function *f);

#endif // IR_BACKEND_H
