#include "codegen/codegen_forward.h"
#include "codegen/machine_ir_forward.h"
#include "utils.h"
#include <codegen/machine_ir.h>

#include <codegen.h>
#include <codegen/intermediate_representation.h>

MIROperand mir_op_function(IRFunction *f) {
  MIROperand out = {0};
  out.kind = MIR_OP_FUNCTION;
  out.value.function = f;
  return out;
}

MIROperand mir_op_block(IRBlock *block) {
  MIROperand out = {0};
  out.kind = MIR_OP_BLOCK;
  out.value.block = block;
  return out;
}

MIROperand mir_op_reference(MIRInstruction *m_inst, IRInstruction *inst) {
  ASSERT(inst, "Invalid argument");
  if (!inst->machine_inst) {
    ir_femit_instruction(stdout, inst);
    ICE("Must translate IRInstruction into MIR before taking reference to it.");
  }
  MIROperand out = {0};
  out.kind = MIR_OP_REFERENCE;
  out.value.ref = inst->machine_inst;
  return out;
}

MIROperand mir_op_immediate(int64_t imm) {
  MIROperand out = {0};
  out.kind = MIR_OP_IMMEDIATE;
  out.value.imm = imm;
  return out;
}

MIROperand mir_op_register(RegisterDescriptor reg, uint16_t size) {
  MIROperand out = {0};
  out.kind = MIR_OP_REGISTER;
  out.value.reg.value = reg;
  out.value.reg.size = size;
  return out;
}

static size_t mir_alloc_id = 0;
MIRInstruction *mir_alloc(uint32_t opcode) {
  MIRInstruction *mir = calloc(1, sizeof(*mir));
  ASSERT(mir, "Memory allocation failure");
  mir->id = ++mir_alloc_id;
  mir->opcode = opcode;
  return mir;
}

MIRInstruction *mir_function(IRFunction *f) {
  MIRInstruction* mir = mir_alloc(MIR_FUNCTION);
  mir->operands[0] = mir_op_function(f);
  return mir;
}

MIRInstruction *mir_block(IRBlock *bb) {
  MIRInstruction* mir = mir_alloc(MIR_BLOCK);
  mir->operands[0] = mir_op_block(bb);
  return mir;
}

MIRInstruction *mir_imm(int64_t imm) {
  MIRInstruction* mir = mir_alloc(MIR_IMMEDIATE);
  mir->operands[0] = mir_op_immediate(imm);
  return mir;
}

MIRVector mir_from_ir(CodegenContext *context) {
  MIRVector out = {0};
  foreach_ptr (IRFunction*, f, context->functions) {
    vector_push(out, mir_function(f));
    if (f->is_extern) continue;
    list_foreach (IRBlock*, bb, f->blocks) {
      print("Block %S\n", bb->name);
      vector_push(out, mir_block(bb));
      list_foreach (IRInstruction*, inst, bb->instructions) {
        ir_femit_instruction(stdout, inst);
        switch (inst->kind) {
        case IR_IMMEDIATE: {
          MIRInstruction *mir = mir_imm((int64_t)inst->imm);
          inst->machine_inst = mir;
          vector_push(out, mir);
        } break;
        case IR_CALL: {
          MIRInstruction *mir = mir_alloc(MIR_CALL);
          if (inst->call.is_indirect)
            mir->operands[0] = mir_op_reference(mir, inst->call.callee_instruction);
          else mir->operands[0] = mir_op_function(inst->call.callee_function);
          inst->machine_inst = mir;
          vector_push(out, mir);
        } break;
        case IR_NOT:
        case IR_ZERO_EXTEND:
        case IR_SIGN_EXTEND:
        case IR_TRUNCATE:
        case IR_BITCAST:
        case IR_COPY:
        case IR_LOAD: {
          MIRInstruction *mir = mir_alloc((uint32_t)inst->kind);
          mir->operands[0] = mir_op_reference(mir, inst->operand);
          inst->machine_inst = mir;
          vector_push(out, mir);
        } break;
        case IR_RETURN: {
          MIRInstruction *mir = mir_alloc(MIR_RETURN);
          if (inst->operand) mir->operands[0] = mir_op_reference(mir, inst->operand);
          inst->machine_inst = mir;
          vector_push(out, mir);
        } break;
        case IR_BRANCH: {
          MIRInstruction *mir = mir_alloc(MIR_BRANCH);
          mir->operands[0] = mir_op_block(inst->destination_block);
          inst->machine_inst = mir;
          vector_push(out, mir);
        } break;
        case IR_BRANCH_CONDITIONAL: {
          MIRInstruction *mir = mir_alloc(MIR_BRANCH_CONDITIONAL);
          mir->operands[0] = mir_op_reference(mir, inst->cond_br.condition);
          mir->operands[1] = mir_op_block(inst->cond_br.then);
          mir->operands[2] = mir_op_block(inst->cond_br.else_);
          inst->machine_inst = mir;
          vector_push(out, mir);
        } break;
        case IR_ADD:
        case IR_SUB:
        case IR_MUL:
        case IR_DIV:
        case IR_MOD:
        case IR_SHL:
        case IR_SAR:
        case IR_SHR:
        case IR_AND:
        case IR_OR:
        case IR_LT:
        case IR_LE:
        case IR_GT:
        case IR_GE:
        case IR_EQ:
        case IR_NE: {
          MIRInstruction *mir = mir_alloc((uint32_t)inst->kind);
          mir->operands[0] = mir_op_reference(mir, inst->lhs);
          mir->operands[1] = mir_op_reference(mir, inst->rhs);
          inst->machine_inst = mir;
          vector_push(out, mir);
        } break;
        case IR_STATIC_REF:
        case IR_FUNC_REF: {
          MIRInstruction *mir = mir_alloc((uint32_t)inst->kind);
          inst->machine_inst = mir;
          mir->operands[0] = mir_op_reference(mir, inst);
          vector_push(out, mir);
        } break;
        case IR_STORE: {
          MIRInstruction *mir = mir_alloc(MIR_STORE);
          mir->operands[0] = mir_op_reference(mir, inst->store.value);
          mir->operands[1] = mir_op_reference(mir, inst->store.addr);
          inst->machine_inst = mir;
          vector_push(out, mir);
        } break;
        case IR_ALLOCA: {
          MIRInstruction *mir = mir_alloc(MIR_ALLOCA);
          mir->operands[0] = mir_op_immediate((int64_t)inst->alloca.size);
          inst->machine_inst = mir;
          vector_push(out, mir);
        } break;

        case IR_PHI: {
          TODO("How does phi work?");
        } break;

        case IR_REGISTER: {
          MIRInstruction *mir = mir_alloc(MIR_REGISTER);
          mir->operands[0] = mir_op_register(inst->result, 0);
          inst->machine_inst = mir;
          vector_push(out, mir);
        } break;

        case IR_UNREACHABLE: {
          MIRInstruction *mir = mir_alloc(MIR_UNREACHABLE);
          inst->machine_inst = mir;
          vector_push(out, mir);
        } break;

        case IR_PARAMETER:
        case IR_LIT_INTEGER:
        case IR_LIT_STRING:
        case IR_COUNT: UNREACHABLE();

        } // switch (inst->kind)
      }
    }
  }
  return out;
}

const char *mir_operand_kind_string(MIROperandKind opkind) {
  switch (opkind) {
  case MIR_OP_NONE: return "none";
  case MIR_OP_REFERENCE: return "reference";
  case MIR_OP_REGISTER: return "register";
  case MIR_OP_IMMEDIATE: return "immediate";
  case MIR_OP_BLOCK: return "block";
  case MIR_OP_FUNCTION: return "function";
  default: break;
  }
  return "";
}

const char *mir_common_opcode_mnemonic(uint32_t opcode) {
  switch (opcode) {
  case MIR_IMMEDIATE: return "immediate";
  case MIR_CALL: return "call";
  case MIR_NOT: return "not";
  case MIR_ZERO_EXTEND: return "zero_extend";
  case MIR_SIGN_EXTEND: return "sign_extend";
  case MIR_TRUNCATE: return "truncate";
  case MIR_BITCAST: return "bitcast";
  case MIR_COPY: return "copy";
  case MIR_LOAD: return "load";
  case MIR_RETURN: return "return";
  case MIR_BRANCH: return "branch";
  case MIR_BRANCH_CONDITIONAL: return "branch_conditional";
  case MIR_ADD: return "add";
  case MIR_SUB: return "sub";
  case MIR_MUL: return "mul";
  case MIR_DIV: return "div";
  case MIR_MOD: return "mod";
  case MIR_SHL: return "shl";
  case MIR_SAR: return "sar";
  case MIR_SHR: return "shr";
  case MIR_AND: return "and";
  case MIR_OR: return "or";
  case MIR_LT: return "<";
  case MIR_LE: return "<=";
  case MIR_GT: return ">";
  case MIR_GE: return ">=";
  case MIR_EQ: return "=";
  case MIR_NE: return "!=";
  case MIR_STATIC_REF: return "static_reference";
  case MIR_FUNC_REF: return "function_reference";
  case MIR_STORE: return "store";
  case MIR_ALLOCA: return "alloca";
  case MIR_PHI: return "phi";
  case MIR_REGISTER: return "register";
  case MIR_UNREACHABLE: return "unreachable";
  case MIR_PARAMETER: return "parameter";
  case MIR_LIT_INTEGER: return "literal_integer";
  case MIR_LIT_STRING: return "literal_string";
  case MIR_FUNCTION: return "function";
  case MIR_BLOCK: return "block";
  case MIR_COUNT: return "count";
  default: break;
  }
  return "";
}

void print_mir_instruction(MIRInstruction *mir) {
  print("%31m%Z | %34%s %36(%31%d%36) ", mir->id, mir_common_opcode_mnemonic(mir->opcode), mir->opcode);
  for (size_t j = 0; j < sizeof(mir->operands) / sizeof(mir->operands[0]); ++j) {
    MIROperand *op = mir->operands + j;
    if (op->kind == MIR_OP_NONE) break;
    //print("  %34%s ", mir_operand_kind_string(op->kind));
    switch (op->kind) {
    case MIR_OP_REGISTER: {
      print(" %34r%u %36(%34%u%36)",
            (unsigned)op->value.reg.value,
            (unsigned)op->value.reg.size);
    } break;
    case MIR_OP_REFERENCE: {
      print(" %31m%Z", op->value.ref->id);
    } break;
    case MIR_OP_IMMEDIATE: {
      print(" %31%Z", op->value.imm);
    } break;
    case MIR_OP_BLOCK: {
      print(" %33%S", op->value.block->name);
    } break;
    case MIR_OP_FUNCTION: {
      print(" %33%S", op->value.function->name);
    } break;
    case MIR_OP_NONE: UNREACHABLE();
    }
  }
  print("\n%m");
}
