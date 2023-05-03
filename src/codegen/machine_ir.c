#include <codegen/machine_ir.h>

#include <codegen.h>
#include <codegen/intermediate_representation.h>
#include <codegen/codegen_forward.h>
#include <utils.h>
#include <vector.h>

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

MIROperand mir_op_reference(MIRInstruction *inst) {
  ASSERT(inst, "Invalid argument");
  MIROperand out = {0};
  out.kind = MIR_OP_REGISTER;
  while (inst->lowered) inst = inst->lowered;
  out.value.reg.value = inst->reg;
  return out;
}

MIROperand mir_op_reference_ir(IRInstruction *inst) {
  ASSERT(inst, "Invalid argument");
  {
    IRInstruction *it = inst;
    for (; it->kind == IR_COPY; it = it->operand);
    if (it->kind == IR_IMMEDIATE) return mir_op_immediate((i64)it->imm);
    if (it->kind == IR_REGISTER) return mir_op_register(it->result, 0);
  }
  if (!inst->machine_inst) {
    ir_femit_instruction(stdout, inst);
    ICE("Must translate IRInstruction into MIR before taking reference to it.");
  }
  return mir_op_reference(inst->machine_inst);
}

MIROperand mir_op_immediate(int64_t imm) {
  MIROperand out = {0};
  out.kind = MIR_OP_IMMEDIATE;
  out.value.imm = imm;
  return out;
}

MIROperand mir_op_name(const char *name) {
  MIROperand out = {0};
  out.kind = MIR_OP_NAME;
  out.value.name = name;
  return out;
}

MIROperand mir_op_register(RegisterDescriptor reg, uint16_t size) {
  MIROperand out = {0};
  out.kind = MIR_OP_REGISTER;
  out.value.reg.value = reg;
  out.value.reg.size = size;
  return out;
}

static bool mir_make_arch = false;
static size_t mir_alloc_id = 0;
static size_t mir_arch_alloc_id = MIR_ARCH_START;
MIRInstruction *mir_makenew(uint32_t opcode) {
  MIRInstruction *mir = calloc(1, sizeof(*mir));
  ASSERT(mir, "Memory allocation failure");
  if (mir_make_arch)
    mir->id = ++mir_arch_alloc_id;
  else mir->id = ++mir_alloc_id;
  mir->opcode = opcode;
  return mir;
}
MIRInstruction *mir_makecopy(MIRInstruction *original) {
  MIRInstruction *mir = mir_makenew(MIR_UNREACHABLE);
  mir->opcode = original->opcode;
  mir->operand_count = original->operand_count;
  if (mir->operand_count <= MIR_OPERAND_SSO_THRESHOLD)
    memcpy(mir->operands.arr, original->operands.arr, sizeof(mir->operands.arr));
  else vector_append_all(mir->operands.vec, original->operands.vec);
  mir->origin = original->origin;
  return mir;
}

void mir_push_with_reg(MIRFunction *f, MIRInstruction *mi, MIRRegister reg) {
  mi->reg = reg;
  vector_push(vector_back(f->blocks)->instructions, mi);
  f->inst_count++;
}

static void mir_push(MIRFunction *f, MIRInstruction *mi) {
  mir_push_with_reg(f, mi, (MIRRegister)(f->inst_count + (size_t)MIR_ARCH_START));
}

MIRFunction *mir_function(IRFunction *ir_f) {
  MIRFunction* f = calloc(1, sizeof(*f));
  f->origin = ir_f;
  f->name = string_dup(ir_f->name);
  return f;
}

MIRBlock *mir_block(MIRFunction *function, IRBlock *ir_bb) {
  MIRBlock* bb = calloc(1, sizeof(*bb));
  bb->function = function;
  bb->origin = ir_bb;
  bb->name = string_dup(ir_bb->name);
  vector_push(function->blocks, bb);
  return bb;
}

MIRInstruction *mir_imm(int64_t imm) {
  MIRInstruction* mir = mir_makenew(MIR_IMMEDIATE);
  mir_add_op(mir, mir_op_immediate(imm));
  return mir;
}

MIRFunctionVector mir_from_ir(CodegenContext *context) {
  MIRFunctionVector out = {0};
  foreach_ptr (IRFunction*, f, context->functions) {
    MIRFunction *function = mir_function(f);
    vector_push(out, function);
    if (f->is_extern) continue;
    list_foreach (IRBlock*, bb, f->blocks) {
      //print("Block %S\n", bb->name);
      (void)mir_block(function, bb);
      list_foreach (IRInstruction*, inst, bb->instructions) {
        //ir_femit_instruction(stdout, inst);
        switch (inst->kind) {

        case IR_IMMEDIATE:
        case IR_REGISTER:
          break;

        case IR_CALL: {
          MIRInstruction *mir = mir_makenew(MIR_CALL);
          mir->origin = inst;
          if (inst->call.is_indirect)
            mir_add_op(mir, mir_op_reference_ir(inst->call.callee_instruction));
          else mir_add_op(mir, mir_op_function(inst->call.callee_function));
          // TODO: Arguments...?
          inst->machine_inst = mir;
          mir_push(function, mir);
        } break;
        case IR_NOT:
        case IR_ZERO_EXTEND:
        case IR_SIGN_EXTEND:
        case IR_TRUNCATE:
        case IR_BITCAST:
        case IR_LOAD: {
          MIRInstruction *mir = mir_makenew((uint32_t)inst->kind);
          mir->origin = inst;
          mir_add_op(mir, mir_op_reference_ir(inst->operand));
          inst->machine_inst = mir;
          mir_push(function, mir);
        } break;

        case IR_COPY: {
          if (inst->operand->kind == IR_IMMEDIATE || inst->operand->kind == IR_REGISTER) break;
          MIRInstruction *mir = mir_makenew(MIR_COPY);
          mir->origin = inst;
          mir_add_op(mir, mir_op_reference_ir(inst->operand));
          inst->machine_inst = mir;
          mir_push(function, mir);
        } break;

        case IR_RETURN: {
          MIRInstruction *mir = mir_makenew(MIR_RETURN);
          mir->origin = inst;
          if (inst->operand) mir_add_op(mir, mir_op_reference_ir(inst->operand));
          inst->machine_inst = mir;
          mir_push(function, mir);
        } break;
        case IR_BRANCH: {
          MIRInstruction *mir = mir_makenew(MIR_BRANCH);
          mir->origin = inst;
          mir_add_op(mir, mir_op_block(inst->destination_block));
          inst->machine_inst = mir;
          mir_push(function, mir);
        } break;
        case IR_BRANCH_CONDITIONAL: {
          MIRInstruction *mir = mir_makenew(MIR_BRANCH_CONDITIONAL);
          mir->origin = inst;
          mir_add_op(mir, mir_op_reference_ir(inst->cond_br.condition));
          mir_add_op(mir, mir_op_block(inst->cond_br.then));
          mir_add_op(mir, mir_op_block(inst->cond_br.else_));
          inst->machine_inst = mir;
          mir_push(function, mir);
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
          MIRInstruction *mir = mir_makenew((uint32_t)inst->kind);
          mir->origin = inst;
          mir_add_op(mir, mir_op_reference_ir(inst->lhs));
          mir_add_op(mir, mir_op_reference_ir(inst->rhs));
          inst->machine_inst = mir;
          mir_push(function, mir);
        } break;
        case IR_STATIC_REF:
        case IR_FUNC_REF: {
          MIRInstruction *mir = mir_makenew((uint32_t)inst->kind);
          inst->machine_inst = mir;
          mir->origin = inst;
          mir_add_op(mir, mir_op_reference_ir(inst));
          mir_push(function, mir);
        } break;
        case IR_STORE: {
          MIRInstruction *mir = mir_makenew(MIR_STORE);
          mir->origin = inst;
          mir_add_op(mir, mir_op_reference_ir(inst->store.value));
          mir_add_op(mir, mir_op_reference_ir(inst->store.addr));
          inst->machine_inst = mir;
          mir_push(function, mir);
        } break;
        case IR_ALLOCA: {
          MIRInstruction *mir = mir_makenew(MIR_ALLOCA);
          mir->origin = inst;
          mir_add_op(mir, mir_op_immediate((int64_t)inst->alloca.size));
          inst->machine_inst = mir;
          mir_push(function, mir);
        } break;

        case IR_PHI: {
          TODO("How does phi work?");
        } break;
        case IR_UNREACHABLE: {
          MIRInstruction *mir = mir_makenew(MIR_UNREACHABLE);
          mir->origin = inst;
          inst->machine_inst = mir;
          mir_push(function, mir);
        } break;

        case IR_PARAMETER:
        case IR_LIT_INTEGER:
        case IR_LIT_STRING:
        case IR_COUNT: UNREACHABLE();

        } // switch (inst->kind)
      }
    }
  }
  mir_make_arch = true;
  return out;
}

const char *mir_operand_kind_string(MIROperandKind opkind) {
  STATIC_ASSERT(MIR_OP_COUNT == 8, "Exhaustive handling of MIR operand kinds");
  switch (opkind) {
  case MIR_OP_NONE: return "none";
  case MIR_OP_REGISTER: return "register";
  case MIR_OP_IMMEDIATE: return "immediate";
  case MIR_OP_BLOCK: return "block";
  case MIR_OP_FUNCTION: return "function";
  case MIR_OP_STATIC_REF: return "static";
  case MIR_OP_LOCAL_REF: return "local";
  default: break;
  }
  return "";
}

const char *mir_common_opcode_mnemonic(uint32_t opcode) {
  STATIC_ASSERT(MIR_COUNT == 39, "Exhaustive handling of MIRCommonOpcodes (string conversion)");
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
  case MIR_BLOCK: return "block";
  case MIR_COUNT: return "count";
  default: break;
  }
  return "";
}

void print_mir_operand(MIROperand *op) {
  STATIC_ASSERT(MIR_OP_COUNT == 8, "Exhaustive handling of MIR operand kinds");
  switch (op->kind) {
  case MIR_OP_REGISTER: {
    // TODO: Maybe print size?
    if (op->value.reg.value >= MIR_ARCH_START)
      print("%34v%u%m", (unsigned)op->value.reg.value);
    else print("%32r%u%m", (unsigned)op->value.reg.value);
  } break;
  case MIR_OP_IMMEDIATE: {
    print("%35%I%m", op->value.imm);
  } break;
  case MIR_OP_BLOCK: {
    print("%33%S%m", op->value.block->name);
  } break;
  case MIR_OP_FUNCTION: {
    print("%33%S%m", op->value.function->name);
  } break;
  case MIR_OP_NAME: {
    print("%37\"%33%s%37\"%m", op->value.name);
  } break;
  case MIR_OP_STATIC_REF: {
    print("%37\"%33%s%37\" %36%T%m", op->value.static_ref->name, op->value.static_ref->name);
  } break;
  case MIR_OP_LOCAL_REF: {
    print("offs:%Z sz:%Z", op->value.local_ref->offset, op->value.local_ref->size);
  } break;
  case MIR_OP_COUNT:
  case MIR_OP_NONE: UNREACHABLE();
  }
}

void print_mir_instruction(MIRInstruction *mir) {
  //if (mir->reg < MIR_ARCH_START)
  //  print("%31m%Z %37| %32r%Z %37| ", mir->id, (usz)mir->reg);
  //else print("%31m%Z %37| %34v%Z %37| ", mir->id, (usz)mir->reg);
  if (mir->reg < MIR_ARCH_START)
    print("%32r%Z %37| ", (usz)mir->reg);
  else print("%34v%Z %37| ", (usz)mir->reg);
  if (mir->opcode < MIR_COUNT) print("%31%s%m ", mir_common_opcode_mnemonic(mir->opcode));
  else print("%31op%d%36 ", (int)mir->opcode);
  MIROperand *base = NULL;
  if (mir->operand_count <= MIR_OPERAND_SSO_THRESHOLD)
    base = mir->operands.arr;
  else base = mir->operands.vec.data;
  for (size_t j = 0; j < mir->operand_count; ++j) {
    MIROperand *op = base + j;
    if (op->kind == MIR_OP_NONE) break;
    print_mir_operand(op);
    if (j < mir->operand_count - 1 && (op + 1)->kind != MIR_OP_NONE)
      print("%37, ");
  }
  print("\n%m");
}

/// Clear the given instructions operands.
void mir_op_clear(MIRInstruction *inst) {
  ASSERT(inst, "Invalid argument");
  inst->operand_count = 0;
  inst->operands.arr[0].kind = MIR_OP_NONE;
}

void mir_add_op(MIRInstruction *inst, MIROperand op) {
  ASSERT(inst, "Invalid argument");
  ASSERT(op.kind != MIR_OP_NONE, "Refuse to add NONE operand.");
  if (inst->operand_count < MIR_OPERAND_SSO_THRESHOLD) {
    inst->operands.arr[inst->operand_count] = op;
  } else if (inst->operand_count == MIR_OPERAND_SSO_THRESHOLD) {
    MIROperand tmp[MIR_OPERAND_SSO_THRESHOLD];
    memcpy(tmp, inst->operands.arr, sizeof(inst->operands.arr[0]) * MIR_OPERAND_SSO_THRESHOLD);
    memset(&inst->operands.vec, 0, sizeof(inst->operands.vec));
    for (size_t i = 0; i < MIR_OPERAND_SSO_THRESHOLD; ++i)
      vector_push(inst->operands.vec, tmp[i]);
  } else {
    // inst->operand_count > MIR_OPERAND_SSO_THRESHOLD
    vector_push(inst->operands.vec, op);
  }
  inst->operand_count++;
}

MIROperand *mir_get_op(MIRInstruction *inst, size_t index) {
  ASSERT(inst, "Invalid argument");
  ASSERT(index < inst->operand_count, "Index out of bounds");
  //if (index >= inst->operand_count) return NULL;
  if (inst->operand_count <= MIR_OPERAND_SSO_THRESHOLD)
    return inst->operands.arr + index;
  return inst->operands.vec.data + index;
}

MIRInstruction *mir_find_by_vreg(MIRFunction *f, size_t reg) {
  ASSERT(f, "Invalid argument");
  ASSERT(reg >= (size_t)MIR_ARCH_START, "Invalid MIR virtual register");

  // Bad linear lookup == sad times
  foreach_ptr (MIRBlock*, block, f->blocks)
    foreach_ptr (MIRInstruction*, inst, block->instructions)
      if (inst->reg == reg) return inst;

  ICE("Could not find machine instruction in function \"%S\" with register %Z\n", f->name, reg);
}
