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
  VECTOR_FOREACH_PTR (IRFunction*, f, *context->functions) {
    ir_print_defun(context->code, f);

    /// Function body.
    fprintf(context->code, " {\n");
    DLIST_FOREACH (IRBlock *, b, f->blocks) {
      fprintf(context->code, "bb%zu:\n", b->id);
      DLIST_FOREACH (IRInstruction *, instruction, b->instructions) {
        if (instruction->type == IR_PARAMETER) continue;

        fprintf(context->code, "    ");
        STATIC_ASSERT(IR_COUNT == 31, "Handle all IR instructions");

        if (instruction->id) fprintf(context->code, "%%%zu = ", instruction->id);
        switch (instruction->type) {
          case IR_IMMEDIATE:
            fprintf(context->code, "imm %"PRId64,instruction->value.immediate);
            break;
          case IR_CALL:
            if (instruction->value.call.tail_call) { fprintf(context->code, "tail "); }
            switch (instruction->value.call.type) {
              case IR_CALLTYPE_DIRECT:
                fprintf(context->code, "call %s", instruction->value.call.value.name);
                break;
              case IR_CALLTYPE_INDIRECT:
                fprintf(context->code, "call %%%zu", instruction->value.call.value.callee->id);
                break;
              default:
                TODO("Handle %d IRCallType.", instruction->value.call.type);
                break;
            }
            fputc('(', context->code);
            IRCallArgument *argument = instruction->value.call.arguments;
            if (argument) {
              fprintf(context->code, "%%%zu", argument->value->id);
              argument = argument->next;
            }
            for (; argument; argument = argument->next) {
              fprintf(context->code, ", %%%zu", argument->value->id);
            }
            fputc(')', context->code);
            break;
          case IR_RETURN:
            fprintf(context->code, "ret %%%zu", instruction->value.reference->id);
            break;
          case IR_ADD:
            fprintf(context->code, "add %%%zu, %%%zu",
                instruction->value.pair.car->id,
                instruction->value.pair.cdr->id);
            break;
          case IR_MULTIPLY:
            fprintf(context->code, "mul %%%zu, %%%zu",
                instruction->value.pair.car->id,
                instruction->value.pair.cdr->id);
            break;
          case IR_DIVIDE:
            fprintf(context->code, "div %%%zu, %%%zu",
                instruction->value.pair.car->id,
                instruction->value.pair.cdr->id);
            break;
          case IR_MODULO:
            fprintf(context->code, "mod %%%zu, %%%zu",
                instruction->value.pair.car->id,
                instruction->value.pair.cdr->id);
            break;
          case IR_SHIFT_LEFT:
            fprintf(context->code, "shl %%%zu, %%%zu",
                instruction->value.pair.car->id,
                instruction->value.pair.cdr->id);
            break;
          case IR_SHIFT_RIGHT_ARITHMETIC:
            fprintf(context->code, "sar %%%zu, %%%zu",
                instruction->value.pair.car->id,
                instruction->value.pair.cdr->id);
            break;
          case IR_AND:
            fprintf(context->code, "and %%%zu, %%%zu",
                instruction->value.pair.car->id,
                instruction->value.pair.cdr->id);
            break;
          case IR_OR:
            fprintf(context->code, "or %%%zu, %%%zu",
                instruction->value.pair.car->id,
                instruction->value.pair.cdr->id);
            break;

          case IR_SUBTRACT:
            fprintf(context->code, "sub %%%zu, %%%zu",
                instruction->value.pair.car->id,
                instruction->value.pair.cdr->id);
            break;
          case IR_GLOBAL_LOAD:
            fprintf(context->code, "load %s", instruction->value.name);
            break;
          case IR_GLOBAL_STORE:
            fprintf(context->code, "store %%%zu, %s",
                instruction->value.global_assignment.new_value->id,
                instruction->value.global_assignment.name);
            break;
          case IR_GLOBAL_ADDRESS:
            fprintf(context->code, ".addr %s", instruction->value.name);
            break;
          case IR_COPY:
            fprintf(context->code, "copy %%%zu", instruction->value.reference->id);
            break;
          case IR_NOT:
            fprintf(context->code, "not %%%zu", instruction->value.reference->id);
            break;
          case IR_LOCAL_LOAD:
            fprintf(context->code, "load %%%zu", instruction->value.reference->id);
            break;
          case IR_LOCAL_STORE:
            fprintf(context->code, "store %%%zu, %%%zu",
                instruction->value.pair.car->id,
                instruction->value.pair.cdr->id);
            break;
          case IR_LOCAL_ADDRESS:
            fprintf(context->code, ".addr %%%zu", instruction->value.reference->id);
            break;
          case IR_PARAMETER: UNREACHABLE();
          case IR_COMPARISON:
            switch (instruction->value.comparison.type) {
              case COMPARE_EQ:
                fprintf(context->code, "eq");
                break;
              case COMPARE_GE:
                fprintf(context->code, "ge");
                break;
              case COMPARE_LE:
                fprintf(context->code, "le");
                break;
              case COMPARE_GT:
                fprintf(context->code, "gt");
                break;
              case COMPARE_LT:
                fprintf(context->code, "lt");
                break;
              case COMPARE_NE:
                fprintf(context->code, "ne");
                break;
              default:
                PANIC("Unhandled comparison type: %d", instruction->value.comparison.type);
                break;
            }
            fprintf(context->code, " %%%zu, %%%zu",
                instruction->value.comparison.pair.car->id,
                instruction->value.comparison.pair.cdr->id);
            break;
          case IR_BRANCH:
            fprintf(context->code, "br bb%zu", instruction->value.block->id);
            break;
          case IR_BRANCH_CONDITIONAL:
            fprintf(context->code, "br.cond %%%zu, bb%zu, bb%zu",
                instruction->value.conditional_branch.condition->id,
                instruction->value.conditional_branch.true_branch->id,
                instruction->value.conditional_branch.false_branch->id);
            break;
          case IR_PHI: {
            fprintf(context->code, "phi ");
            bool first = true;
            VECTOR_FOREACH_PTR (IRPhiArgument*, arg, instruction->value.phi_arguments) {
              if (first) { first = false; }
              else { fprintf(context->code, ", "); }
              fprintf(context->code, "[bb%zu : %%%zu]",
                  arg->block->id,
                  arg->value->id);
            }
          } break;
          case IR_LOAD:
            fprintf(context->code, "load %%%zu", instruction->value.reference->id);
            break;
          case IR_STORE:
            fprintf(context->code, "store %%%zu, %%%zu",
                instruction->value.pair.cdr->id,
                instruction->value.pair.car->id);
            break;
          case IR_REGISTER:
            fprintf(context->code, "register r%d", instruction->result);
            break;
          case IR_STACK_ALLOCATE:
            fprintf(context->code, "alloca %"PRId64, instruction->value.immediate);
            break;
          /// No-op
          case IR_UNREACHABLE:
            fprintf(context->code, "unreachable");
            break;
          default:
            TODO("Handle IRType %d\n", instruction->type);
            break;
        }
        fprintf(context->code, "\n");
      }
    }

    /// End of function.
    fprintf(context->code, "}\n\n");
  }
}
