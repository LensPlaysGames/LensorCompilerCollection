#include <codegen/codegen_platforms.h>

#include <codegen/x86_64/arch_x86_64.h>

#include <codegen.h>
#include <error.h>

CodegenContext *codegen_context_create_top_level
(enum CodegenOutputFormat format,
 enum CodegenCallingConvention call_convention,
 enum CodegenAssemblyDialect dialect,
 FILE* code) {
  CodegenContext *cg_context;

  if (format == CG_FMT_x86_64_GAS) {
    // TODO: Handle call_convention for creating codegen context!
    if (call_convention == CG_CALL_CONV_MSWIN) {
      cg_context = codegen_context_x86_64_mswin_create(NULL);
      ASSERT(cg_context);
    } else if (call_convention == CG_CALL_CONV_LINUX) {
      // TODO: Create codegen context for GAS linux assembly.
      panic("Not implemented: Create codegen context for GAS linux x86_64 assembly.");
    } else {
      panic("Unrecognized calling convention!");
    }
  } else {
    panic("Unrecognized codegen format");
  }

  cg_context->code = code;
  cg_context->dialect = dialect;
  return cg_context;
}

/// Create a codegen context from a parent context.
CodegenContext *codegen_context_create(CodegenContext *parent) {
  ASSERT(parent, "create_codegen_context() can only create contexts when a parent is given.");
  ASSERT(CG_FMT_COUNT == 1, "create_codegen_context() must exhaustively handle all codegen output formats.");
  ASSERT(CG_CALL_CONV_COUNT == 2, "create_codegen_context() must exhaustively handle all calling conventions.");
  if (parent->format == CG_FMT_x86_64_GAS) {
    if (parent->call_convention == CG_CALL_CONV_MSWIN) {
      return codegen_context_x86_64_mswin_create(parent);
    } else if (parent->call_convention == CG_CALL_CONV_LINUX) {
      // return codegen_context_x86_64_gas_linux_create(parent);
    }
  }
  panic("create_codegen_context() could not create a new context from the given parent.");
  return NULL; // Unreachable
}

/// Free a codegen context.
void codegen_context_free(CodegenContext *context) {
  if (context->format == CG_FMT_x86_64_GAS) {
    if (context->call_convention == CG_CALL_CONV_MSWIN) {
      return codegen_context_x86_64_mswin_free(context);
    } else if (context->call_convention == CG_CALL_CONV_MSWIN) {
      // return codegen_context_x86_64_gas_linux_free(parent);
    }
  }
  panic("free_codegen_context() could not free the given context.");
}

/// Save state before a function call.
void codegen_prepare_call(CodegenContext *cg_context) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: codegen_prepare_call_x86_64(cg_context); break;
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Add an argument to the current function call.
void codegen_add_external_function_arg(CodegenContext *cg_context, RegisterDescriptor arg) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: codegen_add_external_function_arg_x86_64(cg_context, arg); break;
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Add an argument to the current function call.
void codegen_add_internal_function_arg(CodegenContext *cg_context, RegisterDescriptor arg) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: codegen_add_internal_function_arg_x86_64(cg_context, arg); break;
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Call an external function. Allocate and return a register containing the return value.
RegisterDescriptor codegen_perform_external_call
(CodegenContext *cg_context,
 const char* function_name) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: return codegen_perform_external_call_x86_64(cg_context, function_name);
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Call an internal function. Allocate and return a register containing the return value.
RegisterDescriptor codegen_perform_internal_call
(CodegenContext *cg_context,
 RegisterDescriptor function) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: return codegen_perform_internal_call_x86_64(cg_context, function);
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Clean up after a function call.
void codegen_cleanup_call(CodegenContext *cg_context) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: codegen_cleanup_call_x86_64(cg_context); break;
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Load the address of a global variable into a newly allocated register and return it.
RegisterDescriptor codegen_load_global_address
(CodegenContext *cg_context,
 const char *name) {
  RegisterDescriptor reg = register_allocate(cg_context);
  codegen_load_global_address_into(cg_context, name, reg);
  return reg;
}

/// Load the address of a local variable into a newly allocated register and return it.
RegisterDescriptor codegen_load_local_address
(CodegenContext *cg_context,
 long long int offset) {
  RegisterDescriptor reg = register_allocate(cg_context);
  codegen_load_local_address_into(cg_context, offset, reg);
  return reg;
}

/// Load the address of a global variable into a register.
void codegen_load_global_address_into
(CodegenContext *cg_context,
 const char *name,
 RegisterDescriptor target) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: codegen_load_global_address_into_x86_64(cg_context, name, target); break;
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Load the address of a local variable into a register.
void codegen_load_local_address_into
(CodegenContext *cg_context,
 long long int offset,
 RegisterDescriptor target) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: codegen_load_local_address_into_x86_64(cg_context, offset, target); break;
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Load the value of global variable into a newly allocated register and return it.
RegisterDescriptor codegen_load_global
(CodegenContext *cg_context,
 const char *name) {
  RegisterDescriptor reg = register_allocate(cg_context);
  codegen_load_global_into(cg_context, name, reg);
  return reg;
}

/// Load the value of local variable into a newly allocated register and return it.
RegisterDescriptor codegen_load_local
(CodegenContext *cg_context,
 long long int offset) {
  RegisterDescriptor reg = register_allocate(cg_context);
  codegen_load_local_into(cg_context, offset, reg);
  return reg;
}

/// Load the value of global variable into a register.
void codegen_load_global_into
(CodegenContext *cg_context,
 const char *name,
 RegisterDescriptor target) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: codegen_load_global_into_x86_64(cg_context, name, target); break;
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Load the value of local variable into a register.
void codegen_load_local_into
(CodegenContext *cg_context,
 long long int offset,
 RegisterDescriptor target) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: codegen_load_local_into_x86_64(cg_context, offset, target); break;
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Store a global variable.
void codegen_store_global
(CodegenContext *cg_context,
 RegisterDescriptor source,
 const char *name) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: codegen_store_global_x86_64(cg_context, source, name); break;
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Store a local variable.
void codegen_store_local
(CodegenContext *cg_context,
 RegisterDescriptor source,
 long long int offset) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: codegen_store_local_x86_64(cg_context, source, offset); break;
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Store data in the memory pointed to by the given address.
void codegen_store
(CodegenContext *cg_context,
 RegisterDescriptor source,
 RegisterDescriptor address) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: codegen_store_x86_64(cg_context, source, address); break;
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Add an immediate value to a register.
void codegen_add_immediate
(CodegenContext *cg_context,
 RegisterDescriptor reg,
 long long int immediate) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: codegen_add_immediate_x86_64(cg_context, reg, immediate); break;
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Branch to a label if a register is zero.
void codegen_branch_if_zero
(CodegenContext *cg_context,
 RegisterDescriptor reg,
 const char *label) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: codegen_branch_if_zero_x86_64(cg_context, reg, label); break;
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Branch to a label.
void codegen_branch
(CodegenContext *cg_context,
 const char *label) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: codegen_branch_x86_64(cg_context, label); break;
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Load an immediate value into a new register.
RegisterDescriptor codegen_load_immediate
(CodegenContext *cg_context,
 long long int immediate) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: return codegen_load_immediate_x86_64(cg_context, immediate);
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Zero out a register.
void codegen_zero_register
(CodegenContext *cg_context,
 RegisterDescriptor reg) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: codegen_zero_register_x86_64(cg_context, reg); break;
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Copy a register to another register.
void codegen_copy_register
(CodegenContext *cg_context,
 RegisterDescriptor src,
 RegisterDescriptor dest) {
  if (src == dest) return;

  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: codegen_copy_register_x86_64(cg_context, src, dest); break;
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Generate a comparison between two registers.
RegisterDescriptor codegen_comparison
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 enum ComparisonType type,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: return codegen_comparison_x86_64(cg_context, mode, type, lhs, rhs);
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Add two registers together.
RegisterDescriptor codegen_add
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: return codegen_add_x86_64(cg_context, mode, lhs, rhs);
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Subtract rhs from lhs.
RegisterDescriptor codegen_subtract
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: return codegen_subtract_x86_64(cg_context, mode, lhs, rhs);
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Multiply two registers together.
RegisterDescriptor codegen_multiply
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: return codegen_multiply_x86_64(cg_context, mode, lhs, rhs);
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Divide lhs by rhs.
RegisterDescriptor codegen_divide
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: return codegen_divide_x86_64(cg_context, mode, lhs, rhs);
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Modulo lhs by rhs.
RegisterDescriptor codegen_modulo
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: return codegen_modulo_x86_64(cg_context, mode, lhs, rhs);
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Shift lhs to the left by rhs.
RegisterDescriptor codegen_shift_left
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: return codegen_shift_left_x86_64(cg_context, mode, lhs, rhs);
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Shift lhs to the right by rhs (arithmetic).
RegisterDescriptor codegen_shift_right_arithmetic
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: return codegen_shift_right_arithmetic_x86_64(cg_context, mode, lhs, rhs);
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Allocate space on the stack.
void codegen_alloca(CodegenContext *cg_context, long long int size) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: codegen_alloca_x86_64(cg_context, size); break;
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Emit the function prologue.
void codegen_function_prologue(CodegenContext *cg_context) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: codegen_prologue_x86_64(cg_context); break;
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Emit the function epilogue.
void codegen_function_epilogue(CodegenContext *cg_context) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: codegen_epilogue_x86_64(cg_context); break;
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Set the return value of a function.
void codegen_set_return_value(CodegenContext *cg_context, RegisterDescriptor value) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: codegen_set_return_value_x86_64(cg_context, value); break;
    default: panic("ERROR: Unrecognized codegen format");
  }
}

/// Emit the entry point of the program.
void codegen_entry_point(CodegenContext *cg_context) {
  switch (cg_context->format) {
    case CG_FMT_x86_64_GAS: codegen_entry_point_x86_64(cg_context); break;
    default: panic("ERROR: Unrecognized codegen format");
  }
}