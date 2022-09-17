#ifndef CODEGEN_PLATFORMS_H
#define CODEGEN_PLATFORMS_H

#include "codegen_forward.h"
#include <stdio.h>

/// Create a top-level codegen context.
CodegenContext *codegen_context_create_top_level
(enum CodegenOutputFormat format,
 enum CodegenCallingConvention call_convention,
 enum CodegenAssemblyDialect dialect,
 FILE* code);

/// Create a codegen context from a parent context.
CodegenContext *codegen_context_create(CodegenContext *parent);

/// Free a codegen context.
void codegen_context_free(CodegenContext *parent);

/// Save state before a function call.
void codegen_prepare_call(CodegenContext *cg_context);

/// Add an argument to the current external function call.
void codegen_add_external_function_arg(CodegenContext *cg_context, RegisterDescriptor arg);

/// Add an argument to the current function call.
void codegen_add_internal_function_arg(CodegenContext *cg_context, RegisterDescriptor arg);

/// Call an external function. Return the register containing the return value.
RegisterDescriptor codegen_perform_external_call
(CodegenContext *cg_context,
 const char* function_name);

/// Call an internal function. Return the register containing the return value.
RegisterDescriptor codegen_perform_internal_call
(CodegenContext *cg_context,
 RegisterDescriptor function);

/// Clean up after a function call.
void codegen_cleanup_call(CodegenContext *cg_context);

/// Load the address of global variable into a newly allocated register and return it.
RegisterDescriptor codegen_load_global_address
(CodegenContext *cg_context,
 const char *name);

/// Load the address of local variable into a newly allocated register and return it.
RegisterDescriptor codegen_load_local_address
(CodegenContext *cg_context,
 long long int offset);

/// Load the address of global variable into a register.
void codegen_load_global_address_into
(CodegenContext *cg_context,
 const char *name,
 RegisterDescriptor target);

/// Load the address of local variable into a register.
void codegen_load_local_address_into
(CodegenContext *cg_context,
 long long int offset,
 RegisterDescriptor target);

/// Load the value of global variable into a newly allocated register and return it.
RegisterDescriptor codegen_load_global
(CodegenContext *cg_context,
 const char *name);

/// Load the value of local variable into a newly allocated register and return it.
RegisterDescriptor codegen_load_local
(CodegenContext *cg_context,
 long long int offset);

/// Load the value of global variable into a register.
void codegen_load_global_into
(CodegenContext *cg_context,
 const char *name,
 RegisterDescriptor target);

/// Load the value of local variable into a register.
void codegen_load_local_into
(CodegenContext *cg_context,
 long long int offset,
 RegisterDescriptor target);

/// Store a global variable.
void codegen_store_global
(CodegenContext *cg_context,
 RegisterDescriptor source,
 const char *name);

/// Store a local variable.
void codegen_store_local
(CodegenContext *cg_context,
 RegisterDescriptor source,
 long long int offset);

/// Store data in the memory pointed to by the given address.
void codegen_store
(CodegenContext *cg_context,
 RegisterDescriptor data,
 RegisterDescriptor address);

/// Add an immediate value to a register.
void codegen_add_immediate
(CodegenContext *cg_context,
 RegisterDescriptor reg,
 long long int immediate);

/// Branch to a label if a register is zero.
void codegen_branch_if_zero
(CodegenContext *cg_context,
 RegisterDescriptor reg,
 const char *label);

/// Branch to a label.
void codegen_branch
(CodegenContext *cg_context,
 const char *label);

/// Load an immediate value into a new register.
RegisterDescriptor codegen_load_immediate
(CodegenContext *cg_context,
 long long int immediate);

/// Zero out a register.
void codegen_zero_register
(CodegenContext *cg_context,
 RegisterDescriptor reg);

/// Copy a register to another register.
void codegen_copy_register
(CodegenContext *cg_context,
 RegisterDescriptor src,
 RegisterDescriptor dest);

/// Generate a comparison between two registers.
RegisterDescriptor codegen_comparison
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 enum ComparisonType type,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs);

/// Add two registers together.
RegisterDescriptor codegen_add
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs);

/// Subtract rhs from lhs.
RegisterDescriptor codegen_subtract
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs);

/// Multiply two registers together.
RegisterDescriptor codegen_multiply
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs);

/// Divide lhs by rhs.
RegisterDescriptor codegen_divide
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs);

/// Modulo lhs by rhs.
RegisterDescriptor codegen_modulo
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs);

/// Shift lhs to the left by rhs.
RegisterDescriptor codegen_shift_left
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs);

/// Shift lhs to the right by rhs (arithmetic).
RegisterDescriptor codegen_shift_right_arithmetic
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs);

/// Allocate space on the stack.
void codegen_alloca(CodegenContext *cg_context, long long int size);

/// Emit the function prologue.
void codegen_function_prologue(CodegenContext *cg_context);

/// Emit the function epilogue.
void codegen_function_epilogue(CodegenContext *cg_context);

/// Set the return value of a function.
void codegen_set_return_value(CodegenContext *cg_context, RegisterDescriptor value);

/// Emit the entry point of the program.
void codegen_entry_point(CodegenContext *cg_context);

#endif // CODEGEN_PLATFORMS_H
