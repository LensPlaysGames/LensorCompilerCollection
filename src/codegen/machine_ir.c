#include <codegen/machine_ir.h>

#include <codegen.h>
#include <codegen/intermediate_representation.h>
#include <codegen/codegen_forward.h>
#include <utils.h>
#include <vector.h>

MIROperand mir_op_function(MIRFunction *f) {
  ASSERT(f, "Cannot create MIR function operand from NULL MIR function.");
  MIROperand out = {0};
  out.kind = MIR_OP_FUNCTION;
  out.value.function = f;
  return out;
}

MIROperand mir_op_block(MIRBlock *block) {
  ASSERT(block, "Cannot create MIR block operand from NULL MIR block.");
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

/// Function is only needed to update instruction count. May pass NULL.
static void mir_push_with_reg_into_block(MIRFunction *f, MIRBlock *block, MIRInstruction *mi, MIRRegister reg) {
  vector_push(block->instructions, mi);
  mi->block = block;
  mi->reg = reg;
  if (f) f->inst_count++;
}

static void mir_push_into_block(MIRFunction *f, MIRBlock *block, MIRInstruction *mi) {
  mir_push_with_reg_into_block(f, block, mi, (MIRRegister)(f->inst_count + (size_t)MIR_ARCH_START));
}

void mir_push_with_reg(MIRFunction *f, MIRInstruction *mi, MIRRegister reg) {
  mir_push_with_reg_into_block(f, vector_back(f->blocks), mi, reg);
}

static void mir_push(MIRFunction *f, MIRInstruction *mi) {
  mir_push_with_reg(f, mi, (MIRRegister)(f->inst_count + (size_t)MIR_ARCH_START));
}

MIRFunction *mir_function(IRFunction *ir_f) {
  MIRFunction* f = calloc(1, sizeof(*f));
  f->origin = ir_f;
  f->name = string_dup(ir_f->name);
  ir_f->machine_func = f;
  return f;
}

MIRBlock *mir_block(MIRFunction *function, IRBlock *ir_bb) {
  MIRBlock* bb = calloc(1, sizeof(*bb));
  bb->function = function;
  bb->origin = ir_bb;
  bb->name = string_dup(ir_bb->name);
  ir_bb->machine_block = bb;
  vector_push(function->blocks, bb);
  return bb;
}

MIRBlock *mir_block_makenew(MIRFunction *function, const char *name) {
  MIRBlock* bb = calloc(1, sizeof(*bb));
  bb->function = function;
  bb->name = string_create(name);
  vector_push(function->blocks, bb);
  return bb;
}

MIRInstruction *mir_imm(int64_t imm) {
  MIRInstruction* mir = mir_makenew(MIR_IMMEDIATE);
  mir_add_op(mir, mir_op_immediate(imm));
  return mir;
}

/// MAY RETURN NULL iff copy operand is of inlined operand type (static ref, local ref, register, or immediate).
MIRInstruction *mir_from_ir_copy(IRInstruction *copy) {
  if (copy->operand->kind == IR_IMMEDIATE || copy->operand->kind == IR_REGISTER) return NULL;
  MIRInstruction *mir = mir_makenew(MIR_COPY);
  mir->origin = copy;
  mir_add_op(mir, mir_op_reference_ir(copy->operand));
  copy->machine_inst = mir;
  return mir;
}

/// Return non-zero iff given instruction needs a register.
static bool needs_register(IRInstruction *instruction) {
  STATIC_ASSERT(IR_COUNT == 38, "Exhaustively handle all instruction types");
  ASSERT(instruction);
  switch (instruction->kind) {
    case IR_LOAD:
    case IR_PHI:
    case IR_COPY:
    case IR_IMMEDIATE:
    case IR_CALL:
    case IR_REGISTER:
    case IR_NOT:
    case IR_ZERO_EXTEND:
    case IR_SIGN_EXTEND:
    case IR_TRUNCATE:
    case IR_BITCAST:
    ALL_BINARY_INSTRUCTION_CASES()
      return true;

    case IR_PARAMETER:
      ICE("Unlowered parameter instruction in register allocator");

    /// Allocas and static refs need a register iff they are actually used.
    case IR_ALLOCA:
    case IR_STATIC_REF:
    case IR_FUNC_REF:
      return instruction->users.size;

    default:
      return false;
  }
}

/// For each argument of each phi instruction, add in a copy to the phi's virtual register.
static void phi2copy(MIRFunction *function) {
  IRBlock *last_block = NULL;
  foreach_ptr (MIRBlock*, block, function->blocks) {
    MIRInstructionVector instructions_to_remove = {0};
    foreach_ptr (MIRInstruction*, instruction, block->instructions) {
      if (instruction->opcode != MIR_PHI) continue;
      IRInstruction *phi = instruction->origin;
      ASSERT(phi->parent_block != last_block,
             "Multiple PHI instructions in a single block are not allowed!");
      last_block = phi->parent_block;

      /// Single PHI argument means that we can replace it with a simple copy.
      if (phi->phi_args.size == 1) {
        instruction->opcode = MIR_COPY;
        mir_op_clear(instruction);
        mir_add_op(instruction, mir_op_reference_ir(phi->phi_args.data[0]->value));
        continue;
      }

      /// For each of the PHI arguments, we basically insert a copy.
      /// Where we insert it depends on some complicated factors
      /// that have to do with control flow.
      foreach_ptr (IRPhiArgument *, arg, phi->phi_args) {
        STATIC_ASSERT(IR_COUNT == 38, "Handle all branch types");
        IRInstruction *branch = arg->block->instructions.last;
        switch (branch->kind) {
        /// If the predecessor returns or is unreachable, then the PHI
        /// is never going to be reached, so we can just ignore
        /// this argument.
        case IR_UNREACHABLE:
        case IR_RETURN: continue;

        /// For direct branches, we just insert the copy before the branch.
        case IR_BRANCH: {
          if (needs_register(arg->value)) {
            MIRInstruction *copy = mir_makenew(MIR_COPY);
            copy->reg = instruction->reg;
            mir_add_op(copy, mir_op_reference_ir(arg->value));
            // Insert copy before branch machine instruction.
            // mir_push_before(branch->machine_inst, copy);
            MIRInstructionVector *instructions = &arg->value->machine_inst->block->instructions;
            vector_insert(*instructions, instructions->data + (instructions->size - 1), copy);
          } else {
            print("\n\n%31Offending block%m:\n");
            ir_femit_block(stdout, arg->value->parent_block);
            ICE("Block ends with instruction that does not return value.");
          }
        } break;

        /// Indirect branches are a bit more complicated. We need to insert an
        /// additional block for the copy instruction and replace the branch
        /// to the phi block with a branch to that block.
        case IR_BRANCH_CONDITIONAL: {

          // Create a COPY of the argument into the MIR PHI's vreg.
          // When we eventually remove the MIR PHI, what will be left is a bunch
          // of copies into the same virtual register. RA can then fill this virtual
          // register in with a single register and boom our PHI is codegenned
          // properly.
          MIRInstruction *copy = mir_makenew(MIR_COPY);
          mir_add_op(copy, mir_op_reference_ir(arg->value));
          copy->reg = instruction->reg;

          // Possible FIXME: This relies on backend filling empty block
          // names with something.
          MIRBlock *critical_edge_trampoline = mir_block_makenew(function, "");
          mir_push_into_block(function, critical_edge_trampoline, copy);
          // Branch to phi block from critical edge
          MIRInstruction *critical_edge_branch = mir_makenew(MIR_BRANCH);
          critical_edge_branch->block = instruction->block;
          mir_push_into_block(function, critical_edge_trampoline, critical_edge_branch);

          // The critical edge trampoline block is now complete. This
          // means we can replace the branch of the argument block to that
          // of this critical edge trampoline.

          // Condition is first operand, then the "then" branch, then "else".
          MIROperand *branch_then = mir_get_op(branch->machine_inst, 1);
          MIROperand *branch_else = mir_get_op(branch->machine_inst, 2);
          if (branch_then->value.block->origin == phi->parent_block)
            *branch_then = mir_op_block(critical_edge_trampoline);
          else {
            ASSERT(branch_else->value.block->origin == phi->parent_block,
                   "Branch to phi block is neither true nor false branch of conditional branch!");
            *branch_else = mir_op_block(critical_edge_trampoline);
          }
        } break;
        default: UNREACHABLE();
        }

        // Add index of phi instruction to list of instructions to remove.
        vector_push(instructions_to_remove, instruction);
      }

      foreach_ptr(MIRInstruction*, to_remove, instructions_to_remove) {
        vector_remove_element(block->instructions, to_remove);
      }
    }
  }
}

MIRFunctionVector mir_from_ir(CodegenContext *context) {
  MIRFunctionVector out = {0};
  // Forward function references require this.
  foreach_ptr (IRFunction*, f, context->functions) {
    MIRFunction *function = mir_function(f);
    vector_push(out, function);
  }
  foreach_ptr (MIRFunction*, function, out) {
    // NOTE for devs: function->origin == IRFunction*
    if (function->origin->is_extern) continue;
    // Forward block references require this.
    list_foreach (IRBlock*, bb, function->origin->blocks) {
      (void)mir_block(function, bb);
    }
    foreach_ptr (MIRBlock*, mir_bb, function->blocks) {
      IRBlock *bb = mir_bb->origin;
      list_foreach (IRInstruction*, inst, bb->instructions) {
        switch (inst->kind) {

        case IR_IMMEDIATE:
        case IR_REGISTER:
          break;

        case IR_PHI: {
          MIRInstruction *mir = mir_makenew(MIR_PHI);
          mir->origin = inst;
          inst->machine_inst = mir;
          mir_push_into_block(function, mir_bb, mir);
        } break;

        case IR_CALL: {
          MIRInstruction *mir = mir_makenew(MIR_CALL);
          mir->origin = inst;
          inst->machine_inst = mir;

          // Call target (destination)
          if (inst->call.is_indirect)
            mir_add_op(mir, mir_op_reference_ir(inst->call.callee_instruction));
          else mir_add_op(mir, mir_op_function(inst->call.callee_function->machine_func));

          // Call arguments
          foreach_ptr (IRInstruction*, arg, inst->call.arguments) {
            mir_add_op(mir, mir_op_reference_ir(arg));
          }

          mir_push_into_block(function, mir_bb, mir);
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
          mir_push_into_block(function, mir_bb, mir);
        } break;

        case IR_COPY: {
          if (inst->operand->kind == IR_IMMEDIATE || inst->operand->kind == IR_REGISTER) break;
          MIRInstruction *mir = mir_from_ir_copy(inst);
          mir_push_into_block(function, mir_bb, mir);
        } break;

        case IR_RETURN: {
          MIRInstruction *mir = mir_makenew(MIR_RETURN);
          mir->origin = inst;
          if (inst->operand) mir_add_op(mir, mir_op_reference_ir(inst->operand));
          inst->machine_inst = mir;
          mir_push_into_block(function, mir_bb, mir);
        } break;
        case IR_BRANCH: {
          MIRInstruction *mir = mir_makenew(MIR_BRANCH);
          mir->origin = inst;
          mir_add_op(mir, mir_op_block(inst->destination_block->machine_block));
          inst->machine_inst = mir;
          mir_push_into_block(function, mir_bb, mir);
        } break;
        case IR_BRANCH_CONDITIONAL: {
          MIRInstruction *mir = mir_makenew(MIR_BRANCH_CONDITIONAL);
          mir->origin = inst;
          mir_add_op(mir, mir_op_reference_ir(inst->cond_br.condition));
          mir_add_op(mir, mir_op_block(inst->cond_br.then->machine_block));
          mir_add_op(mir, mir_op_block(inst->cond_br.else_->machine_block));
          inst->machine_inst = mir;
          mir_push_into_block(function, mir_bb, mir);
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
          mir_push_into_block(function, mir_bb, mir);
        } break;
        case IR_STATIC_REF:
        case IR_FUNC_REF: {
          MIRInstruction *mir = mir_makenew((uint32_t)inst->kind);
          inst->machine_inst = mir;
          mir->origin = inst;
          mir_add_op(mir, mir_op_reference_ir(inst));
          mir_push_into_block(function, mir_bb, mir);
        } break;
        case IR_STORE: {
          MIRInstruction *mir = mir_makenew(MIR_STORE);
          mir->origin = inst;
          mir_add_op(mir, mir_op_reference_ir(inst->store.value));
          mir_add_op(mir, mir_op_reference_ir(inst->store.addr));
          inst->machine_inst = mir;
          mir_push_into_block(function, mir_bb, mir);
        } break;
        case IR_ALLOCA: {
          MIRInstruction *mir = mir_makenew(MIR_ALLOCA);
          mir->origin = inst;
          mir_add_op(mir, mir_op_immediate((int64_t)inst->alloca.size));
          inst->machine_inst = mir;
          mir_push_into_block(function, mir_bb, mir);
        } break;

        case IR_UNREACHABLE: {
          MIRInstruction *mir = mir_makenew(MIR_UNREACHABLE);
          mir->origin = inst;
          inst->machine_inst = mir;
          mir_push_into_block(function, mir_bb, mir);
        } break;

        case IR_PARAMETER:
        case IR_LIT_INTEGER:
        case IR_LIT_STRING:
        case IR_COUNT: UNREACHABLE();

        } // switch (inst->kind)
      }
    }

    phi2copy(function);
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
    ASSERT(op->value.block, "Invalid block operand");
    print("%37Block:%33%S%m", op->value.block->name);
  } break;
  case MIR_OP_FUNCTION: {
    ASSERT(op->value.function, "Invalid function operand");
    print("%37Function:%33%S%m", op->value.function->name);
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

void print_mir_instruction_with_mnemonic(MIRInstruction *mir, OpcodeMnemonicFunction opcode_mnemonic) {
  ASSERT(mir, "Invalid argument");
  ASSERT(opcode_mnemonic, "Invalid argument");
  if (mir->reg < MIR_ARCH_START)
    print("%32r%Z %37| ", (usz)mir->reg);
  else print("%34v%Z %37| ", (usz)mir->reg);
  const char *mnemonic = opcode_mnemonic(mir->opcode);
  if (mnemonic && *mnemonic != '\0') print("%31%s%m ", opcode_mnemonic(mir->opcode));
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

void print_mir_instruction(MIRInstruction *mir) {
  print_mir_instruction_with_mnemonic(mir, &mir_common_opcode_mnemonic);
}

void print_mir_block_with_mnemonic(MIRBlock *block, OpcodeMnemonicFunction opcode_mnemonic) {
  print("%S:\n", block->name);
  foreach_ptr (MIRInstruction*, inst, block->instructions)
    print_mir_instruction_with_mnemonic(inst, opcode_mnemonic);
}
void print_mir_block(MIRBlock *block) {
  print_mir_block_with_mnemonic(block, &mir_common_opcode_mnemonic);
}
void print_mir_function_with_mnemonic(MIRFunction *function, OpcodeMnemonicFunction opcode_mnemonic) {
  print("%S {\n", function->name);
  foreach_ptr (MIRBlock*, block, function->blocks) {
    print_mir_block_with_mnemonic(block, opcode_mnemonic);
  }
  print("}\n");
}
void print_mir_function(MIRFunction *function) {
  print_mir_function_with_mnemonic(function, &mir_common_opcode_mnemonic);
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
