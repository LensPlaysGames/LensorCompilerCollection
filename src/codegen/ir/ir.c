#include <codegen/ir/ir.h>
#include <codegen/intermediate_representation.h>

CodegenContext *codegen_context_ir_create() {
  CodegenContext *cg_ctx = calloc(1, sizeof(CodegenContext));
  cg_ctx->format = CG_FMT_IR;
  cg_ctx->call_convention = CG_CALL_CONV_MSWIN;
  cg_ctx->dialect = CG_ASM_DIALECT_ATT;
  return cg_ctx;
}

void codegen_context_ir_free(CodegenContext *context) {
  (void) context;
  /// No-op.
}

void codegen_lower_ir_backend(CodegenContext *context) {
  (void) context;
  /// No-op.
}

void codegen_emit_ir_backend(CodegenContext *context) {
  bool use_colour = _thread_use_diagnostics_colours_;
  disable_colours();

  ir_set_ids(context);
  VECTOR_FOREACH_PTR (IRFunction*, f, context->functions) {
    ir_print_defun(context->code, f);

    /// Function body.
    fprintf(context->code, " {\n");
    DLIST_FOREACH (IRBlock *, b, f->blocks) {
      fprintf(context->code, "bb%zu:\n", b->id);
      DLIST_FOREACH (IRInstruction *, instruction, b->instructions) {
        if (instruction->type == IR_PARAMETER) continue;

        fprintf(context->code, "    ");
        STATIC_ASSERT(IR_COUNT == 32, "Handle all IR instructions");

        if (instruction->id) fprintf(context->code, "%%%u = ", instruction->id);
        switch (instruction->type) {
          case IR_IMMEDIATE:
            fprintf(context->code, "imm %"PRId64,instruction->imm);
            break;

          case IR_CALL: {
            if (instruction->call.tail_call) { fprintf(context->code, "tail "); }
            if (instruction->call.is_indirect) fprintf(context->code, "call %%%u", instruction->call.callee_instruction->id);
            else fprintf(context->code, "call %s", instruction->call.callee_function->name.data);

            fputc('(', context->code);
            bool first = true;
            VECTOR_FOREACH_PTR (IRInstruction*, arg, instruction->call.arguments) {
              if (!first) fprintf(context->code, ", ");
              else first = false;
              fprintf(context->code, "%%%u", arg->id);
            }
            fputc(')', context->code);
          } break;

          case IR_RETURN:
            if (instruction->operand) fprintf(context->code, "ret %%%u", instruction->operand->id);
            else fprintf(context->code, "ret");
            break;
#define PRINT_BINARY_INSTRUCTION(enumerator, name) \
  case IR_##enumerator: fprintf(context->code, #name " %%%u, %%%u", instruction->lhs->id, instruction->rhs->id); break;
          ALL_BINARY_INSTRUCTION_TYPES(PRINT_BINARY_INSTRUCTION)
#undef PRINT_BINARY_INSTRUCTION

          case IR_COPY:
            fprintf(context->code, "copy %%%u", instruction->operand->id);
            break;
          case IR_NOT:
            fprintf(context->code, "not %%%u", instruction->operand->id);
            break;
          case IR_PARAMETER: UNREACHABLE();
          case IR_BRANCH:
            fprintf(context->code, "br bb%zu", instruction->destination_block->id);
            break;
          case IR_BRANCH_CONDITIONAL:
            fprintf(context->code, "br.cond %%%u, bb%zu, bb%zu",
                instruction->cond_br.condition->id,
                instruction->cond_br.then->id,
                instruction->cond_br.else_->id);
            break;
          case IR_PHI: {
            fprintf(context->code, "phi ");
            bool first = true;
            VECTOR_FOREACH_PTR (IRPhiArgument*, arg, instruction->phi_args) {
              if (first) { first = false; }
              else { fprintf(context->code, ", "); }
              fprintf(context->code, "[bb%zu : %%%u]",
                  arg->block->id,
                  arg->value->id);
            }
          } break;
          case IR_LOAD:
            fprintf(context->code, "load %%%u", instruction->operand->id);
            break;
          case IR_STORE:
            fprintf(context->code, "store %%%u, %%%u",
                instruction->lhs->id,
                instruction->rhs->id);
            break;
          case IR_REGISTER:
            fprintf(context->code, "register r%d", instruction->result);
            break;
          case IR_ALLOCA:
            fprintf(context->code, "alloca %"PRId64, instruction->imm);
            break;
          case IR_UNREACHABLE:
            fprintf(context->code, "unreachable");
            break;
          default:
            TODO("Handle IRType %d\n", instruction->type);
        }
        fprintf(context->code, "\n");
      }
    }

    /// End of function.
    fprintf(context->code, "}\n\n");
  }

  if (use_colour) enable_colours();
}
