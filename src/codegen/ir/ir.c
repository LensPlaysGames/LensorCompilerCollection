#include <codegen/ir/ir.h>
#include <codegen/intermediate_representation.h>

CodegenContext *codegen_context_ir_create(CodegenContext *parent) {
    CodegenContext *cg_ctx = calloc(1,sizeof(CodegenContext));

    if (parent) {
        cg_ctx->code = parent->code;
        cg_ctx->arch_data = parent->arch_data;
        cg_ctx->format = parent->format;
        cg_ctx->call_convention = parent->call_convention;
        cg_ctx->dialect = parent->dialect;
    } else {
        cg_ctx->format = CG_FMT_IR;
        cg_ctx->call_convention = CG_CALL_CONV_MSWIN;
        cg_ctx->dialect = CG_ASM_DIALECT_ATT;
    }

    cg_ctx->parent = parent;
    cg_ctx->locals = environment_create(NULL);
    return cg_ctx;
}

void codegen_context_ir_free(CodegenContext *ctx) {
  /// TODO (Sirraide): Free environment.
  free(ctx);
}

void codegen_lower_ir_backend(CodegenContext *context) {
  (void) context;
  /// No-op.
}

void codegen_emit_ir_backend(CodegenContext *context) {
  ir_set_ids(context);
  ir_femit(context->code, context);
}