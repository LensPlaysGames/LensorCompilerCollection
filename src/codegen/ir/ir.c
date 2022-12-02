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
  /// Eliminate .params and loads from them.
  typedef VECTOR(IRInstruction*) IRVector;
  typedef struct {
    IRInstruction *param;
    IRVector loads;
  } param;
  VECTOR(param) param_loads = {0};

  VECTOR_FOREACH_PTR (IRFunction*, f, *context->functions) {
    VECTOR_FOREACH (param, p, param_loads) VECTOR_DELETE(p->loads);
    VECTOR_CLEAR(param_loads);
    FOREACH_INSTRUCTION_IN_FUNCTION(f) {
      switch (instruction->type) {
        /// Add the parameter to the param loads.
        case IR_PARAMETER_REFERENCE: {
          param *p;
          VECTOR_FIND_IF(param_loads, p, i, param_loads.data[i].param == instruction);
          if (!p) VECTOR_PUSH(param_loads, ((param){instruction, {0}}));
        } break;

        /// Add loads from parameter to the list.
        case IR_LOCAL_LOAD:
        case IR_LOAD: {
          param *p;
          VECTOR_FIND_IF(param_loads, p, i, param_loads.data[i].param == instruction->value.reference);
          if (p) VECTOR_PUSH(p->loads, instruction);
        } break;
      }
    }

    /// Keep the param refs for now so that ids are assigned correctly
    /// later on, but replace all uses of all loads from params w/ a
    /// reference to the param and delete the load.
    ///
    /// NOTE: This transformation results in internally invalid IR.
    /// However this is fine since this is just for printing.
    VECTOR_FOREACH (param, p, param_loads) {
      VECTOR_FOREACH_PTR (IRInstruction*, load, p->loads) {
        ir_replace_uses(load, p->param);
        ir_remove(load);
      }
    }
  }
}

void codegen_emit_ir_backend(CodegenContext *context) {
  ir_set_ids(context);
  ir_femit(context->code, context);
}