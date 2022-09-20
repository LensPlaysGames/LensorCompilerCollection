#ifndef CODEGEN_PLATFORMS_H
#define CODEGEN_PLATFORMS_H

#include <codegen/codegen_forward.h>

///  Create a top-level codegen context.
CodegenContext * codegen_context_create_top_level
(enum CodegenOutputFormat format,
 enum CodegenCallingConvention call_convention,
 enum CodegenAssemblyDialect dialect,
 FILE* code);

/// Create a codegen context from a parent context.
CodegenContext * codegen_context_create(CodegenContext *parent);

/// Free a codegen context.
void codegen_context_free(CodegenContext *parent);

/// Emit the program.
void codegen_emit(CodegenContext *context);
#endif // CODEGEN_PLATFORMS_H
