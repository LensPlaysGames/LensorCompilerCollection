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
    } else if (context->call_convention == CG_CALL_CONV_LINUX) {
      // return codegen_context_x86_64_gas_linux_free(parent);
    }
  }
  panic("free_codegen_context() could not free the given context.");
}

#define SECOND(x, y) y
#define CODEGEN_API_FUNCTION(return_type, name, ...) \
  return_type name(FOR_EACH_PAIR_JOIN(CODEGEN_API_PARAMS_DECLARE, VA_SEPARATOR_COMMA, CodegenContext*, ctx, __VA_ARGS__)) { \
    switch (ctx->format) { \
      case CG_FMT_x86_64_GAS: CONCATENATE(name, _x86_64)(FOR_EACH_PAIR_JOIN(SECOND, VA_SEPARATOR_COMMA, CodegenContext*, ctx, __VA_ARGS__)); break; \
      default: panic("ERROR: Unrecognized codegen format"); \
    } \
  }

#include <codegen/codegen_interface.def>
