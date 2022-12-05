#ifndef FUNCOMPILER_IR_H
#define FUNCOMPILER_IR_H

#include <codegen/codegen_forward.h>

CodegenContext *codegen_context_ir_create(CodegenContext *parent);
void codegen_context_ir_free(CodegenContext *ctx);

/// The `_backend` is so we don’t confuse these w/ frontent functions.
void codegen_lower_ir_backend(CodegenContext *context);
void codegen_emit_ir_backend(CodegenContext *context);

#endif // FUNCOMPILER_IR_H
