#ifndef ARCH_X86_64_H
#define ARCH_X86_64_H

#include "../codegen_forward.h"

/// This is used for defining lookup tables etc. and
/// ensures that the registers are always in the correct
/// order
#define FOR_ALL_X86_64_REGISTERS(F)     \
  F(RAX, "rax", "eax", "ax", "al")      \
  F(RBX, "rbx", "ebx", "bx", "bl")      \
  F(RCX, "rcx", "ecx", "cx", "cl")      \
  F(RDX, "rdx", "edx", "dx", "dl")      \
  F(R8,  "r8", "r8d", "r8w", "r8b")     \
  F(R9,  "r9", "r9d", "r9w", "r9b")     \
  F(R10, "r10", "r10d", "r10w", "r10b") \
  F(R11, "r11", "r11d", "r11w", "r11b") \
  F(R12, "r12", "r12d", "r12w", "r12b") \
  F(R13, "r13", "r13d", "r13w", "r13b") \
  F(R14, "r14", "r14d", "r14w", "r14b") \
  F(R15, "r15", "r15d", "r15w", "r15b") \
  F(RSI, "rsi", "esi", "si", "sil")     \
  F(RDI, "rdi", "edi", "di", "dil")     \
  F(RBP, "rbp", "ebp", "bp", "bpl")     \
  F(RSP, "rsp", "esp", "sp", "spl")     \
  F(RIP, "rip", "eip", "ip", "ipl")

/// Context allocation/deallocation
CodegenContext *codegen_context_x86_64_mswin_create(CodegenContext *parent);
void codegen_context_x86_64_mswin_free(CodegenContext *ctx);

/// Save state before a function call.
void codegen_prepare_call_x86_64(CodegenContext *cg_context);

/// Add an argument to the current function call.
void codegen_add_external_function_arg_x86_64(CodegenContext *cg_context, RegisterDescriptor arg);

/// Add an argument to the current function call.
void codegen_add_internal_function_arg_x86_64(CodegenContext *cg_context, RegisterDescriptor arg);

/// Call an external function. Returns the register containing the return value.
RegisterDescriptor codegen_perform_external_call_x86_64
(CodegenContext *cg_context,
 const char* function_name);

/// Call an internal function. Return the register containing the return value.
RegisterDescriptor codegen_perform_internal_call_x86_64
(CodegenContext *cg_context,
 RegisterDescriptor function);

/// Clean up after a function call.
void codegen_cleanup_call_x86_64(CodegenContext *cg_context);

/// Load the address of a global variable into a register.
void codegen_load_global_address_into_x86_64
(CodegenContext *cg_context,
 const char *name,
 RegisterDescriptor target);

/// Load the address of a local variable into a register.
void codegen_load_local_address_into_x86_64
(CodegenContext *cg_context,
 long long int offset,
 RegisterDescriptor target);


/// Load the value of a global variable into a register.
void codegen_load_global_into_x86_64
(CodegenContext *cg_context,
 const char *name,
 RegisterDescriptor target);

/// Load the value of a local variable into a register.
void codegen_load_local_into_x86_64
(CodegenContext *cg_context,
 long long int offset,
 RegisterDescriptor target);

/// Store a global variable.
void codegen_store_global_x86_64
(CodegenContext *cg_context,
 RegisterDescriptor source,
 const char *name);

/// Store a local variable.
void codegen_store_local_x86_64
(CodegenContext *cg_context,
 RegisterDescriptor source,
 long long int offset);

/// Store data in the memory pointed to by the given address.
void codegen_store_x86_64
(CodegenContext *cg_context,
 RegisterDescriptor source,
 RegisterDescriptor address);

/// Load an immediate value into a new register.
RegisterDescriptor codegen_load_immediate_x86_64
(CodegenContext *cg_context,
 long long int immediate);

/// Add an immediate value to a register.
void codegen_add_immediate_x86_64
(CodegenContext *cg_context,
 RegisterDescriptor reg,
 long long int immediate);

/// Branch to a label if a register is zero.
void codegen_branch_if_zero_x86_64
(CodegenContext *cg_context,
 RegisterDescriptor reg,
 const char *label);

/// Branch to a label.
void codegen_branch_x86_64
(CodegenContext *cg_context,
 const char *label);

/// Copy a register to another register.
void codegen_copy_register_x86_64
(CodegenContext *cg_context,
 RegisterDescriptor src,
 RegisterDescriptor dest);

/// Zero out a register.
void codegen_zero_register_x86_64
(CodegenContext *cg_context,
 RegisterDescriptor reg);

/// Generate a comparison between two registers.
RegisterDescriptor codegen_comparison_x86_64
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 enum ComparisonType type,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs);

/// Add two registers together.
RegisterDescriptor codegen_add_x86_64
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs);

/// Subtract rhs from lhs.
RegisterDescriptor codegen_subtract_x86_64
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs);

/// Multiply two registers together.
RegisterDescriptor codegen_multiply_x86_64
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs);

/// Divide lhs by rhs.
RegisterDescriptor codegen_divide_x86_64
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs);

/// Modulo lhs by rhs.
RegisterDescriptor codegen_modulo_x86_64
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs);

/// Shift lhs to the left by rhs.
RegisterDescriptor codegen_shift_left_x86_64
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs);

/// Shift lhs to the right by rhs (arithmetic).
RegisterDescriptor codegen_shift_right_arithmetic_x86_64
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs);

/// Allocate space on the stack.
void codegen_alloca_x86_64(CodegenContext *cg_context, long long int size);


/// Emit the function prologue.
void codegen_prologue_x86_64(CodegenContext *cg_context);

/// Emit the function epilogue.
void codegen_epilogue_x86_64(CodegenContext *cg_context);

/// Set the return value of a function.
void codegen_set_return_value_x86_64(CodegenContext *cg_context, RegisterDescriptor reg);

/// Emit the entry point of the program.
void codegen_entry_point_x86_64(CodegenContext *cg_context);

#endif // ARCH_X86_64_H
