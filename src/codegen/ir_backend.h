#ifndef IR_BACKEND_H
#define IR_BACKEND_H

#include <codegen/ir.h>

/// Insert a value into the current basic block.
void insert(CodegenContext *context, Value *value);

/// Create a COPY instruction that copies `v`.
Value *create_copy(CodegenContext *context, Value *v);

/// A backend should call this before emitting a function. Do not call this elsewhere.
void codegen_function_finalise(CodegenContext *context, Function *f);

/// Insert `value_to_insert` before `value`.
void insert_before(Value* value, Value *value_to_insert);

/// Insert `value_to_insert` after `value`.
void insert_after(Value *value, Value *value_to_insert);

#endif // IR_BACKEND_H
