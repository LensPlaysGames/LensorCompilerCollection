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
  bool use_colour = thread_use_colours;
  disable_colours();

  ir_set_ids(context);
  foreach_ptr (IRFunction*, f, context->functions) {
    ir_print_defun(context->code, f);

    /// Function body.
    fprint(context->code, " {\n");
    list_foreach (IRBlock *, b, f->blocks) {
      fprint(context->code, "bb%Z:\n", b->id);
      list_foreach (IRInstruction *, instruction, b->instructions) {
        if (instruction->kind == IR_PARAMETER) continue;

        fprint(context->code, "    ");
        STATIC_ASSERT(IR_COUNT == 32, "Handle all IR instructions");

        if (instruction->id) fprint(context->code, "%%%u = ", instruction->id);
        switch (instruction->kind) {
          case IR_IMMEDIATE:
            fprint(context->code, "imm %U", instruction->imm);
            break;

          case IR_CALL: {
            if (instruction->call.tail_call) { fprint(context->code, "tail "); }
            if (instruction->call.is_indirect) fprint(context->code, "call %%%u", instruction->call.callee_instruction->id);
            else fprint(context->code, "call %s", instruction->call.callee_function->name.data);

            fputc('(', context->code);
            bool first = true;
            foreach_ptr (IRInstruction*, arg, instruction->call.arguments) {
              if (!first) fprint(context->code, ", ");
              else first = false;
              fprint(context->code, "%%%u", arg->id);
            }
            fputc(')', context->code);
          } break;

          case IR_RETURN:
            if (instruction->operand) fprint(context->code, "ret %%%u", instruction->operand->id);
            else fprint(context->code, "ret");
            break;
#define PRINT_BINARY_INSTRUCTION(enumerator, name) \
  case IR_##enumerator: fprint(context->code, #name " %%%u, %%%u", instruction->lhs->id, instruction->rhs->id); break;
          ALL_BINARY_INSTRUCTION_TYPES(PRINT_BINARY_INSTRUCTION)
#undef PRINT_BINARY_INSTRUCTION

          case IR_COPY:
            fprint(context->code, "copy %%%u", instruction->operand->id);
            break;
          case IR_NOT:
            fprint(context->code, "not %%%u", instruction->operand->id);
            break;
          case IR_PARAMETER: UNREACHABLE();
          case IR_BRANCH:
            fprint(context->code, "br bb%zu", instruction->destination_block->id);
            break;
          case IR_BRANCH_CONDITIONAL:
            fprint(context->code, "br.cond %%%u, bb%zu, bb%zu",
                instruction->cond_br.condition->id,
                instruction->cond_br.then->id,
                instruction->cond_br.else_->id);
            break;
          case IR_PHI: {
            fprint(context->code, "phi ");
            bool first = true;
            foreach_ptr (IRPhiArgument*, arg, instruction->phi_args) {
              if (first) { first = false; }
              else { fprint(context->code, ", "); }
              fprint(context->code, "[bb%zu : %%%u]",
                  arg->block->id,
                  arg->value->id);
            }
          } break;
          case IR_LOAD:
            fprint(context->code, "load %%%u", instruction->operand->id);
            break;
          case IR_STORE:
            fprint(context->code, "store %%%u, %%%u",
                instruction->lhs->id,
                instruction->rhs->id);
            break;
          case IR_REGISTER:
            fprint(context->code, "register r%u", instruction->result);
            break;
          case IR_ALLOCA:
            fprint(context->code, "alloca %U", instruction->imm);
            break;
          case IR_UNREACHABLE:
            fprint(context->code, "unreachable");
            break;
          default:
            TODO("Handle IRType %d\n", instruction->kind);
        }
        fprint(context->code, "\n");
      }
    }

    /// End of function.
    fprint(context->code, "}\n\n");
  }

  if (use_colour) enable_colours();
}
