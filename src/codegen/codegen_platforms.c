#include <codegen.h>
#include <codegen/ir.h>
#include <codegen/codegen_platforms.h>
#include <codegen/x86_64/arch_x86_64.h>

#include <error.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

CodegenContext *codegen_context_create_top_level
(enum CodegenOutputFormat format,
 enum CodegenCallingConvention call_convention,
 enum CodegenAssemblyDialect dialect,
 FILE* code) {
  CodegenContext *cg_context;

  if (format == CG_FMT_x86_64) {
    cg_context = codegen_context_x86_64_create(NULL);
  } else {
    panic("Unrecognized codegen format");
  }

  ASSERT(cg_context);
  cg_context->format = format;
  cg_context->code = code;
  cg_context->dialect = dialect;
  cg_context->call_convention = call_convention;
  return cg_context;
}

/// Create a codegen context from a parent context.
CodegenContext *codegen_context_create(CodegenContext *parent) {
  ASSERT(parent, "Can only create contexts when a parent is given.");
  STATIC_ASSERT(CG_FMT_COUNT == 1, "Must exhaustively handle all codegen output formats.");
  STATIC_ASSERT(CG_CALL_CONV_COUNT == 2, "Must exhaustively handle all calling conventions.");
  if (parent->format == CG_FMT_x86_64) {
    return codegen_context_x86_64_create(parent);
  }
  panic("create_codegen_context() could not create a new context from the given parent.");
  return NULL; // Unreachable
}

/// Free a codegen context.
void codegen_context_free(CodegenContext *context) {
  STATIC_ASSERT(CG_FMT_COUNT == 1, "codegen_context_free() must exhaustively handle all codegen output formats.");

  if (!context->parent) codegen_free_ir(context);

  switch (context->format) {
    default: PANIC("Unrecognized codegen format");
    case CG_FMT_x86_64: codegen_context_x86_64_free(context); break;
  }
}

void codegen_emit(CodegenContext *context) {
  STATIC_ASSERT(CG_FMT_COUNT == 1, "codegen_emit() must exhaustively handle all codegen output formats.");
  context->insert_point = NULL;
  switch (context->format) {
    case CG_FMT_x86_64:
      codegen_emit_x86_64(context);
      break;
    default:
      PANIC("Unknown codegen format");
  }
}