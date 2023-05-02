#include <codegen/x86_64/arch_x86_64_isel.h>

#include <codegen.h>
#include <codegen/intermediate_representation.h>
#include <codegen/machine_ir.h>
#include <codegen/x86_64/arch_x86_64_common.h>
#include <codegen/x86_64/arch_x86_64.h>
#include <opt.h>

static MIRValue_x86_64 mir_none(Instruction inst) {
  MIRValue_x86_64 out = {0};
  out.instruction_form = I_FORM_NONE;
  out.instruction = (uint16_t)inst;
  return out;
}

static MIRValue_x86_64 mir_imm(Instruction inst, int64_t immediate) {
  MIRValue_x86_64 out = {0};
  out.instruction_form = I_FORM_IMM;
  out.instruction = (uint16_t)inst;
  out.immediate = immediate;
  return out;
}
static MIRValue_x86_64 mir_imm_to_mem(Instruction inst, int64_t immediate, RegisterDescriptor address, int64_t offset) {
  MIRValue_x86_64 out = {0};
  out.instruction_form = I_FORM_IMM_TO_MEM;
  out.instruction = (uint16_t)inst;
  out.immediate = immediate;
  out.reg_addr = (uint8_t)address;
  out.offset = offset;
  return out;

}
static MIRValue_x86_64 mir_imm_to_reg(Instruction inst, int64_t immediate, RegisterDescriptor destination, RegSize size) {
  MIRValue_x86_64 out = {0};
  out.instruction_form = I_FORM_IMM_TO_REG;
  out.instruction = (uint16_t)inst;
  out.immediate = immediate;
  out.reg_dst = (uint8_t)destination;
  out.reg_dst_sz = (uint8_t)size;
  return out;

}
static MIRValue_x86_64 mir_indirect_branch(Instruction inst, RegisterDescriptor address) {
  MIRValue_x86_64 out = {0};
  out.instruction_form = I_FORM_INDIRECT_BRANCH;
  out.instruction = (uint16_t)inst;
  out.reg_addr = (uint8_t)address;
  return out;

}
static MIRValue_x86_64 mir_mem(Instruction inst, int64_t offset, RegisterDescriptor address) {
  MIRValue_x86_64 out = {0};
  out.instruction_form = I_FORM_MEM;
  out.instruction = (uint16_t)inst;
  out.offset = offset;
  out.reg_addr = (uint8_t)address;
  return out;

}
static MIRValue_x86_64 mir_mem_to_reg(Instruction inst, RegisterDescriptor address_register, int64_t offset, RegisterDescriptor destination_register, RegSize size) {
  MIRValue_x86_64 out = {0};
  out.instruction_form = I_FORM_MEM_TO_REG;
  out.instruction = (uint16_t)inst;
  out.reg_addr = (uint8_t)address_register;
  out.reg_dst = (uint8_t)destination_register;
  out.reg_dst_sz = (uint8_t)size;
  out.offset = offset;
  return out;

}
static MIRValue_x86_64 mir_name(Instruction inst, const char *name) {
  MIRValue_x86_64 out = {0};
  out.instruction_form = I_FORM_NAME;
  out.instruction = (uint16_t)inst;
  out.name = name;
  return out;

}
static MIRValue_x86_64 mir_name_to_reg(Instruction inst, RegisterDescriptor address_register, const char *name, RegisterDescriptor destination_register, RegSize size) {
  MIRValue_x86_64 out = {0};
  out.instruction_form = I_FORM_NAME_TO_REG;
  out.instruction = (uint16_t)inst;
  out.reg_addr = (uint8_t)address_register;
  out.name = name;
  out.reg_dst = (uint8_t)destination_register;
  out.reg_dst_sz = (uint8_t)size;
  return out;
}
static MIRValue_x86_64 mir_reg(Instruction inst, RegisterDescriptor reg, RegSize size) {
  MIRValue_x86_64 out = {0};
  out.instruction_form = I_FORM_REG;
  out.instruction = (uint16_t)inst;
  out.reg_dst = (uint8_t)reg;
  out.reg_dst_sz = (uint8_t)size;
  return out;
}
static MIRValue_x86_64 mir_reg_shift(Instruction inst, RegisterDescriptor reg) {
  MIRValue_x86_64 out = {0};
  out.instruction_form = I_FORM_REG_SHIFT;
  out.instruction = (uint16_t)inst;
  out.reg_dst = (uint8_t)reg;
  return out;
}
static MIRValue_x86_64 mir_reg_to_mem(Instruction inst, RegisterDescriptor source_register, RegSize size, RegisterDescriptor address_register, int64_t offset) {
  MIRValue_x86_64 out = {0};
  out.instruction_form = I_FORM_REG_TO_MEM;
  out.instruction = (uint16_t)inst;
  out.reg_src = (uint8_t)source_register;
  out.reg_src_sz = (uint8_t)size;
  out.reg_addr = (uint8_t)address_register;
  out.offset = offset;
  return out;
}
static MIRValue_x86_64 mir_reg_to_name(Instruction inst, RegisterDescriptor source_register, RegSize size, RegisterDescriptor address_register, const char *name) {
  MIRValue_x86_64 out = {0};
  out.instruction_form = I_FORM_REG_TO_NAME;
  out.instruction = (uint16_t)inst;
  out.reg_src = (uint8_t)source_register;
  out.reg_src_sz = (uint8_t)size;
  out.reg_addr = (uint8_t)address_register;
  out.name = name;
  return out;
}
static MIRValue_x86_64 mir_reg_to_offset_name(Instruction inst, RegisterDescriptor source_register, RegSize size, RegisterDescriptor address_register, const char *name, int64_t offset) {
  MIRValue_x86_64 out = {0};
  out.instruction_form = I_FORM_REG_TO_OFFSET_NAME;
  out.instruction = (uint16_t)inst;
  out.reg_src = (uint8_t)source_register;
  out.reg_src_sz = (uint8_t)size;
  out.reg_addr = (uint8_t)address_register;
  out.offset = offset;
  out.name = name;
  return out;
}
static MIRValue_x86_64 mir_reg_to_reg
(Instruction inst,
 RegisterDescriptor source_register, enum RegSize source_size,
 RegisterDescriptor destination_register, enum RegSize destination_size
) {
  MIRValue_x86_64 out = {0};
  out.instruction_form = I_FORM_REG_TO_REG;
  out.instruction = (uint16_t)inst;
  out.reg_src = (uint8_t)source_register;
  out.reg_dst = (uint8_t)destination_register;
  out.reg_src_sz = (uint8_t)source_size;
  out.reg_dst_sz = (uint8_t)destination_size;
  return out;
}

static MIRValue_x86_64 mir_setcc(enum ComparisonType compare_type, RegisterDescriptor destination_register) {
  MIRValue_x86_64 out = {0};
  out.instruction_form = I_FORM_SETCC;
  out.instruction = (uint16_t)I_SETCC;
  out.immediate = (int64_t)compare_type;
  out.reg_dst = (uint8_t)destination_register;
  return out;
}

static MIRValue_x86_64 mir_jcc(IndirectJumpType jump_type, const char *name) {
  MIRValue_x86_64 out = {0};
  out.instruction_form = I_FORM_JCC;
  out.instruction = (uint16_t)I_JCC;
  out.immediate = (int64_t)jump_type;
  out.name = name;
  return out;
}

static void append_mir(MIRVector *mir, MIRValue_x86_64 value, IRInstruction *origin) {
  ASSERT(mir, "Invalid argument");
  MIRInstruction *m_inst = calloc(1, sizeof(*m_inst));
  ASSERT(m_inst, "Memory allocation failure");
  m_inst->x64 = value;
  m_inst->origin = origin;
  vector_push(*mir, m_inst);
}

NODISCARD static bool is_caller_saved(Register r) {
  for (size_t i = 0; i < caller_saved_register_count; ++i) {
    if (caller_saved_registers[i] == r) {
      return 1;
    }
  }
  return 0;
}

NODISCARD static bool is_callee_saved(Register r) { return !is_caller_saved(r); }

/// Generate a comparison between two registers.
static RegisterDescriptor codegen_comparison
(CodegenContext *cg_context,
 IRInstruction *inst,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs,
 RegisterDescriptor result,
 enum RegSize size)
{
  enum ComparisonType type = COMPARE_COUNT;
  switch (inst->kind) {
    case IR_EQ: type = COMPARE_EQ; break;
    case IR_NE: type = COMPARE_NE; break;
    case IR_LT: type = COMPARE_LT; break;
    case IR_GT: type = COMPARE_GT; break;
    case IR_LE: type = COMPARE_LE; break;
    case IR_GE: type = COMPARE_GE; break;
    default: ICE("Unsupported IR instruction in codegen_comparison: %d", inst->kind);
  }

  // Perform the comparison.
  append_mir(cg_context->mir, mir_reg_to_reg(I_CMP, rhs, size, lhs, size), inst);
  // IF YOU REPLACE THIS WITH A XOR IT WILL BREAK HORRIBLY
  // We use MOV because it doesn't set flags.
  append_mir(cg_context->mir, mir_imm_to_reg(I_MOV, 0, result, r32), inst);
  append_mir(cg_context->mir, mir_setcc(type, result), inst);

  return result;
}

enum StackFrameKind {
  FRAME_FULL,
  FRAME_MINIMAL,
  FRAME_NONE,
};

static enum StackFrameKind stack_frame_kind(IRFunction *f) {
  /// Always emit a frame if we’re not optimising.
  if (!optimise) return FRAME_FULL;

  /// Emit a frame if we have local variables.
  if (f->locals_total_size) return FRAME_FULL;

  /// We need *some* sort of prologue if we don’t use the stack but
  /// still call other functions.
  if (!f->attr_leaf) return FRAME_MINIMAL;

  /// Otherwise, no frame is required.
  return FRAME_NONE;
}

/// Emit the function prologue.
static void codegen_prologue(CodegenContext *cg_context, IRFunction *f) {
  enum StackFrameKind frame_kind = stack_frame_kind(f);
  switch (frame_kind) {
    case FRAME_NONE: break;

    case FRAME_FULL: {
      append_mir(cg_context->mir, mir_reg(I_PUSH, REG_RBP, r64), NULL);
      append_mir(cg_context->mir, mir_reg_to_reg(I_MOV, REG_RSP, r64, REG_RBP, r64), NULL);
      if (!optimise || f->locals_total_size)
        append_mir(cg_context->mir, mir_imm_to_reg(I_SUB, (int64_t)f->locals_total_size, REG_RSP, r64), NULL);
    } break;

    case FRAME_MINIMAL: {
      append_mir(cg_context->mir, mir_reg(I_PUSH, REG_RBP, r64), NULL);
    } break;
  }
}

/// Emit the function epilogue.
static void codegen_epilogue(CodegenContext *cg_context, IRFunction *f) {
  enum StackFrameKind frame_kind = stack_frame_kind(f);
  switch (frame_kind) {
    case FRAME_NONE: break;

    case FRAME_FULL: {
      append_mir(cg_context->mir, mir_reg_to_reg(I_MOV, REG_RBP, r64, REG_RSP, r64), NULL);
      append_mir(cg_context->mir, mir_reg(I_POP, REG_RBP, r64), NULL);
    } break;

    case FRAME_MINIMAL: {
      append_mir(cg_context->mir, mir_reg(I_POP, REG_RBP, r64), NULL);
    } break;
  }
}

static void divmod(CodegenContext *context, IRInstruction *inst) {
  ASSERT(inst->kind == IR_DIV || inst->kind == IR_MOD, "divmod must be passed a div or mod instruction!");
  // Dividend of div/mod goes in rdx:rax; divisor must not be in those registers.
  ASSERT(inst->rhs->result != REG_RAX,
         "Register allocation must not allocate RAX to divisor.");
  ASSERT(inst->rhs->result != REG_RDX,
         "Register allocation must not allocate RDX to divisor.");
  enum RegSize lhs_size = regsize_from_bytes(type_sizeof(inst->lhs->type));
  enum RegSize rhs_size = regsize_from_bytes(type_sizeof(inst->rhs->type));

  ASSERT(lhs_size == rhs_size, "x86_64 backend requires divisor and dividend to be of same sized type!");

  // Move dividend into RDX:RAX (for now, just RAX)
  if (lhs_size == r8 || lhs_size == r16) append_mir(context->mir, mir_reg_to_reg(I_XOR, REG_RAX, r64, REG_RAX, r64), inst);
  append_mir(context->mir, mir_reg_to_reg(I_MOV, inst->lhs->result, lhs_size, REG_RAX, lhs_size), inst);

  if (type_is_signed(inst->type)) {
    if (lhs_size == r64) {
      // For 8-byte signed types, we need to extend RAX into the 16-byte
      // RDX:RAX. For this, we use CQO (convert quad-word to octal-word).
      append_mir(context->mir, mir_none(I_CQO), inst);
    } else if (lhs_size == r32) {
      // For 4-byte signed types, we need to extend EAX into the 8-byte
      // EDX:EAX. For this, we use CDQ (convert double-word to quad-word).
      append_mir(context->mir, mir_none(I_CDQ), inst);
    } else if (lhs_size == r16) {
      // For 2-byte signed types, we need to extend AX into the 4-byte
      // DX:AX. For this, we use CWD (convert word to double-word).
      append_mir(context->mir, mir_none(I_CWD), inst);
    } else ICE("Unhandled register size for signed division");

    append_mir(context->mir, mir_reg(I_IDIV, inst->rhs->result, lhs_size), inst);
  }
  else {
    // For unsigned types, we need to make sure RDX is zero; if it
    // was set, it would cause a signed division, and we don't want
    // that.
    append_mir(context->mir, mir_reg_to_reg(I_XOR, REG_RDX, r64, REG_RDX, r64), inst);
    append_mir(context->mir, mir_reg(I_DIV, inst->rhs->result, lhs_size), inst);
  }
}

static void emit_instruction(CodegenContext *context, IRInstruction *inst) {
  STATIC_ASSERT(IR_COUNT == 38, "Handle all IR instructions");

  if (annotate_code) {
    // TODO: Base comment syntax on dialect or smth.
    fprint(context->code, ";;#;");
    thread_use_colours = false;
    ir_femit_instruction(context->code, inst);
    thread_use_colours = true;
  }

  switch (inst->kind) {
  case IR_PHI:
  case IR_REGISTER:
  case IR_UNREACHABLE:
  case IR_LIT_INTEGER:
  case IR_LIT_STRING:
    break;
  case IR_IMMEDIATE:
    if (inst->type == t_integer_literal) {
      // TODO: integer_literal probably shouldn't be handled here.
      // Do this in a pass before-hand or something.
      if (inst->imm <= UINT32_MAX)
        append_mir(context->mir, mir_imm_to_reg(I_MOV, (int64_t)inst->imm, inst->result, r32), inst);
      else if (inst->imm <= UINT64_MAX)
        append_mir(context->mir, mir_imm_to_reg(I_MOV, (int64_t)inst->imm, inst->result, r64), inst);
      else ICE("Unsupported integer literal immediate on x86_64 (out of range)");
    } else {
      if (type_sizeof(inst->type) <= 4)
        append_mir(context->mir, mir_imm_to_reg(I_MOV, (int64_t)inst->imm, inst->result, r32), inst);
      else if (type_sizeof(inst->type) <= 8)
        append_mir(context->mir, mir_imm_to_reg(I_MOV, (int64_t)inst->imm, inst->result, r64), inst);
      else ICE("Unsupported immediate size on x86_64: %Z", type_sizeof(inst->type));
    }
    break;
  case IR_NOT: {
    append_mir(context->mir, mir_reg(I_NOT, inst->operand->result, r64), inst);
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->operand->type));
    append_mir(context->mir, mir_reg_to_reg(I_MOV, inst->operand->result, size, inst->result, size), inst);
  } break;
  case IR_ZERO_EXTEND: {
    usz operand_byte_size = type_sizeof(inst->operand->type);
    usz result_byte_size = type_sizeof(inst->type);
    ASSERT (result_byte_size > operand_byte_size, "Zero extension result must be larger than operand");

    enum RegSize operand_size = regsize_from_bytes(operand_byte_size);
    enum RegSize result_size = regsize_from_bytes(result_byte_size);

    append_mir(context->mir, mir_reg_to_reg(I_MOVZX, inst->operand->result, operand_size, inst->result, result_size), inst);
  } break;
  case IR_SIGN_EXTEND: {
    usz operand_byte_size = type_sizeof(inst->operand->type);
    usz result_byte_size = type_sizeof(inst->type);
    ASSERT (result_byte_size > operand_byte_size, "Sign extension result must be larger than operand");

    enum RegSize operand_size = regsize_from_bytes(operand_byte_size);
    enum RegSize result_size = regsize_from_bytes(result_byte_size);

    append_mir(context->mir, mir_reg_to_reg(I_MOVSX, inst->operand->result, operand_size, inst->result, result_size), inst);
  } break;
  case IR_TRUNCATE: {
    usz operand_byte_size = type_sizeof(inst->operand->type);
    usz result_byte_size = type_sizeof(inst->type);
    ASSERT (result_byte_size < operand_byte_size, "Truncation result must be smaller than operand");

    enum RegSize result_size = regsize_from_bytes(result_byte_size);

    append_mir(context->mir, mir_reg_to_reg(I_MOV, inst->operand->result, r64, inst->result, r64), inst);
    switch (result_size) {
    case r32:
      append_mir(context->mir, mir_imm_to_reg(I_AND, 0xffffffff, inst->result, r32), inst);
      break;
    case r16:
      append_mir(context->mir, mir_imm_to_reg(I_AND, 0xffff, inst->result, r32), inst);
      break;
    case r8:
      append_mir(context->mir, mir_imm_to_reg(I_AND, 0xff, inst->result, r32), inst);
      break;

    case r64:
    default:
      UNREACHABLE();
    }

  } break;

  case IR_BITCAST: break;

  case IR_COPY: {
    usz operand_byte_size = type_sizeof(inst->operand->type);
    usz result_byte_size = type_sizeof(inst->type);

    // TODO: Handle things larger than a register, somehow... may need
    // to push/pop registers...
    enum RegSize operand_size = regsize_from_bytes(operand_byte_size);
    enum RegSize result_size = regsize_from_bytes(result_byte_size);

    append_mir(context->mir, mir_reg_to_reg(I_MOV, inst->operand->result, operand_size, inst->result, result_size), inst);
  } break;
  case IR_CALL: {
    // Save caller saved registers used in caller function.
    ASSERT(inst->parent_block, "call instruction null block");
    ASSERT(inst->parent_block->function, "block has null function");

    // Tail call.
    if (inst->call.tail_call) {
      // Restore the frame pointer if we have one.
      codegen_epilogue(context, inst->parent_block->function);
      if (inst->call.is_indirect)
        append_mir(context->mir, mir_reg(I_JMP, inst->call.callee_instruction->result, r64), inst);
      else append_mir(context->mir, mir_name(I_JMP, inst->call.callee_function->name.data), inst);
      if (inst->parent_block) inst->parent_block->done = true;
      break;
    }

    size_t func_regs = inst->parent_block->function->registers_in_use;
    size_t regs_pushed_count = 0;

    // Save return register.
    // FIXME: Use calling convention for return register.
    if (func_regs & REG_RAX) {
      append_mir(context->mir, mir_reg(I_PUSH, REG_RAX, r64), inst);
      // NOTE: regs_pushed_count for this push is updated below, as the
      // mask isn't unset.
    }

    // Count registers used in function.
    size_t x = func_regs;
    while (x) {
      regs_pushed_count++;
      x &= x - 1;
    }

    // Align stack pointer before call, if necessary.
    if (regs_pushed_count & 0b1)
      append_mir(context->mir, mir_imm_to_reg(I_SUB, 8, REG_RSP, r64), inst);

    // TODO: Don't push registers that are used for arguments.
    for (Register i = REG_RAX + 1; i < sizeof(func_regs) * 8; ++i)
      if (func_regs & (1 << i) && is_caller_saved(i))
        append_mir(context->mir, mir_reg(I_PUSH, i, r64), inst);

    usz bytes_pushed = 0;
    // Shadow stack
    if (context->call_convention == CG_CALL_CONV_MSWIN) {
      append_mir(context->mir, mir_imm_to_reg(I_SUB, 32, REG_RSP, r64), inst);
      bytes_pushed += 32;
    }

    // Push argument addresses, if need be.
    foreach_ptr (IRInstruction *, arg, inst->call.arguments) {
      // If argument is passed on stack due to ABI.
      if (arg->kind == IR_ALLOCA) {
        append_mir(context->mir, mir_reg(I_PUSH, REG_RBP, r64), inst);
        bytes_pushed += 8;
        append_mir(context->mir, mir_imm_to_mem(I_SUB, (i64)arg->alloca.offset, REG_RSP, 0), inst);
      }
    }

    if (inst->call.is_indirect)
      append_mir(context->mir, mir_reg(I_CALL, inst->call.callee_instruction->result, r64), inst);
    else append_mir(context->mir, mir_name(I_CALL, inst->call.callee_function->name.data), inst);

    // Restore stack
    if (bytes_pushed)
      append_mir(context->mir, mir_imm_to_reg(I_ADD, (int64_t)bytes_pushed, REG_RSP, r64), inst);

    // Restore caller saved registers used in called function.
    for (Register i = sizeof(func_regs) * 8 - 1; i > REG_RAX; --i)
      if (func_regs & (1 << i) && is_caller_saved(i))
        append_mir(context->mir, mir_reg(I_POP, i, r64), inst);

    // Restore stack pointer from stack alignment, if necessary.
    if (regs_pushed_count & 0b1)
      append_mir(context->mir, mir_imm_to_reg(I_ADD, 8, REG_RSP, r64), inst);

    append_mir(context->mir, mir_reg_to_reg(I_MOV, REG_RAX, r64, inst->result, r64), inst);

    // Restore return register.
    // FIXME: Use calling convention for return register.
    if (func_regs & REG_RAX)
      append_mir(context->mir, mir_reg(I_POP, REG_RAX, r64), inst);

  } break;

  case IR_RETURN:
    // Restore callee-saved registers used in the function.
    for (Register i = sizeof(inst->parent_block->function->registers_in_use) * 8 - 1; i > 0; --i)
      if (inst->parent_block->function->registers_in_use & ((size_t)1 << i) && is_callee_saved(i))
        append_mir(context->mir, mir_reg(I_POP, i, r64), inst);

    codegen_epilogue(context, inst->parent_block->function);
    append_mir(context->mir, mir_none(I_RET), inst);
    if (optimise && inst->parent_block) inst->parent_block->done = true;
    break;

  case IR_BRANCH:
    /// Only emit a jump if the target isn’t the next block.
    if (!optimise || (inst->parent_block && inst->destination_block != inst->parent_block->next && !inst->parent_block->done))
      append_mir(context->mir, mir_name(I_JMP, inst->destination_block->name.data), inst);
    if (optimise && inst->parent_block) inst->parent_block->done = true;
    break;
  case IR_BRANCH_CONDITIONAL: {
    IRBranchConditional *branch = &inst->cond_br;

    append_mir(context->mir, mir_reg_to_reg(I_TEST, branch->condition->result, r64, branch->condition->result, r64), inst);

    /// If either target is the next block, arrange the jumps in such a way
    /// that we can save one and simply fallthrough to the next block.
    if (optimise && branch->then == inst->parent_block->next)
      append_mir(context->mir, mir_jcc(JUMP_TYPE_Z, branch->else_->name.data), inst);
    else if (optimise && branch->else_ == inst->parent_block->next)
      append_mir(context->mir, mir_jcc(JUMP_TYPE_NZ, branch->then->name.data), inst);
    else {
      append_mir(context->mir, mir_jcc(JUMP_TYPE_Z, branch->else_->name.data), inst);
      append_mir(context->mir, mir_name(I_JMP, branch->then->name.data), inst);
    }

    if (optimise && inst->parent_block) inst->parent_block->done = true;
  } break;
  case IR_EQ: // FALLTHROUGH to case IR_GE
  case IR_NE: // FALLTHROUGH to case IR_GE
  case IR_LT: // FALLTHROUGH to case IR_GE
  case IR_GT: // FALLTHROUGH to case IR_GE
  case IR_LE: // FALLTHROUGH to case IR_GE
  case IR_GE: {
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    codegen_comparison(context, inst, inst->lhs->result, inst->rhs->result, inst->result, size);
  } break;
  case IR_ADD: {
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    append_mir(context->mir, mir_reg_to_reg(I_ADD, inst->lhs->result, size, inst->rhs->result, size), inst);
    append_mir(context->mir, mir_reg_to_reg(I_MOV, inst->rhs->result, size, inst->result, size), inst);
  } break;
  case IR_SUB: {
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    append_mir(context->mir, mir_reg_to_reg(I_SUB, inst->rhs->result, size, inst->lhs->result, size), inst);
    append_mir(context->mir, mir_reg_to_reg(I_MOV, inst->lhs->result, size, inst->result, size), inst);
  } break;
  case IR_MUL: {
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    append_mir(context->mir, mir_reg_to_reg(I_IMUL, inst->lhs->result, size, inst->rhs->result, size), inst);
    append_mir(context->mir, mir_reg_to_reg(I_MOV, inst->rhs->result, size, inst->result, size), inst);
  } break;
  case IR_DIV: {
    divmod(context, inst);
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    append_mir(context->mir, mir_reg_to_reg(I_MOV, REG_RAX, size, inst->result, size), inst);
  } break;
  case IR_MOD: {
    divmod(context, inst);
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    append_mir(context->mir, mir_reg_to_reg(I_MOV, REG_RDX, size, inst->result, size), inst);
  } break;
  case IR_SHL: {
    ASSERT(inst->lhs->result != REG_RCX,
           "Register allocation must not allocate RCX to result of lhs of shift.");
    enum RegSize rhs_size = regsize_from_bytes(type_sizeof(inst->rhs->type));
    append_mir(context->mir, mir_reg_to_reg(I_MOV, inst->rhs->result, rhs_size, REG_RCX, r64), inst);
    append_mir(context->mir, mir_reg(I_SHL, inst->lhs->result, r64), inst);
    enum RegSize lhs_size = regsize_from_bytes(type_sizeof(inst->lhs->type));
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    append_mir(context->mir, mir_reg_to_reg(I_MOV, inst->lhs->result, lhs_size, inst->result, size), inst);
  } break;
  case IR_SHR: {
    enum RegSize rhs_size = regsize_from_bytes(type_sizeof(inst->rhs->type));
    append_mir(context->mir, mir_reg_to_reg(I_MOV, inst->rhs->result, rhs_size, REG_RCX, r64), inst);
    append_mir(context->mir, mir_reg(I_SHR, inst->lhs->result, r64), inst);
    enum RegSize lhs_size = regsize_from_bytes(type_sizeof(inst->lhs->type));
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    append_mir(context->mir, mir_reg_to_reg(I_MOV, inst->lhs->result, lhs_size, inst->result, size), inst);
  } break;
  case IR_SAR: {
    enum RegSize rhs_size = regsize_from_bytes(type_sizeof(inst->rhs->type));
    append_mir(context->mir, mir_reg_to_reg(I_MOV, inst->rhs->result, rhs_size, REG_RCX, r64), inst);
    append_mir(context->mir, mir_reg(I_SAR, inst->lhs->result, r64), inst);
    enum RegSize lhs_size = regsize_from_bytes(type_sizeof(inst->lhs->type));
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    append_mir(context->mir, mir_reg_to_reg(I_MOV, inst->lhs->result, lhs_size, inst->result, size), inst);
  } break;
  case IR_AND: {
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    enum RegSize lhs_size = regsize_from_bytes(type_sizeof(inst->lhs->type));
    enum RegSize rhs_size = regsize_from_bytes(type_sizeof(inst->rhs->type));
    append_mir(context->mir, mir_reg_to_reg(I_AND, inst->lhs->result, lhs_size, inst->rhs->result, size), inst);
    append_mir(context->mir, mir_reg_to_reg(I_MOV, inst->rhs->result, rhs_size, inst->result, size), inst);
  } break;
  case IR_OR: {
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    enum RegSize lhs_size = regsize_from_bytes(type_sizeof(inst->lhs->type));
    enum RegSize rhs_size = regsize_from_bytes(type_sizeof(inst->rhs->type));
    append_mir(context->mir, mir_reg_to_reg(I_OR, inst->lhs->result, lhs_size, inst->rhs->result, size), inst);
    append_mir(context->mir, mir_reg_to_reg(I_MOV, inst->rhs->result, rhs_size, inst->result, size), inst);
  } break;

  case IR_LOAD: {
    RegSize size = (RegSize)-1u;
    /// Load from a static variable.
    if (inst->operand->kind == IR_STATIC_REF) {
      size = regsize_from_bytes(type_sizeof(inst->operand->type->pointer.to));
      if (size == r8 || size == r16) append_mir(context->mir, mir_reg_to_reg(I_XOR, inst->result, r32, inst->result, r32), inst);
      append_mir(context->mir, mir_name_to_reg(I_MOV, REG_RIP, inst->operand->static_ref->name.data, inst->result, size), inst);
    }

    /// Load from a local.
    else if (inst->operand->kind == IR_ALLOCA) {
      size = regsize_from_bytes(inst->operand->alloca.size);
      if (size == r8 || size == r16) append_mir(context->mir, mir_reg_to_reg(I_XOR, inst->result, r32, inst->result, r32), inst);
      append_mir(context->mir, mir_mem_to_reg(I_MOV, REG_RBP, - (i64)inst->operand->alloca.offset, inst->result, size), inst);
    }

    /// Load from a pointer
    else {
      size = regsize_from_bytes(type_sizeof(inst->operand->type));
      if (size == r8 || size == r16) append_mir(context->mir, mir_reg_to_reg(I_XOR, inst->result, r32, inst->result, r32), inst);
      append_mir(context->mir, mir_mem_to_reg(I_MOV, inst->operand->result, 0, inst->result, size), inst);
    }
  } break;

  case IR_STORE: {
    RegSize size = regsize_from_bytes(type_sizeof(inst->store.value->type));

    /// Store to a static variable.
    if (inst->store.addr->kind == IR_STATIC_REF)
      append_mir(context->mir, mir_reg_to_name(I_MOV, inst->store.value->result, size, REG_RIP, inst->store.addr->static_ref->name.data), inst);

    /// Store to a local.
    else if (inst->store.addr->kind == IR_ALLOCA)
      append_mir(context->mir, mir_reg_to_mem(I_MOV, inst->store.value->result, size, REG_RBP, - (i64)inst->store.addr->alloca.offset), inst);

    /// Store to a pointer.
    else append_mir(context->mir, mir_reg_to_mem(I_MOV, inst->store.value->result, size, inst->store.addr->result, 0), inst);

  } break;

  case IR_STATIC_REF: {
    if (inst->result) append_mir(context->mir, mir_name_to_reg(I_LEA, REG_RIP, inst->static_ref->name.data, inst->result, r64), inst);
  } break;
  case IR_FUNC_REF: {
    if (inst->result) append_mir(context->mir, mir_name_to_reg(I_LEA, REG_RIP, inst->function_ref->name.data, inst->result, r64), inst);
  } break;
  case IR_ALLOCA: {
    append_mir(context->mir, mir_mem_to_reg(I_LEA, REG_RBP, - (i64)inst->alloca.offset, inst->result, r64), inst);
  } break;

  default:
    ir_femit_instruction(stderr, inst);
    TODO("Handle IRtype %d\n", inst->kind);
    break;
  }
}

static void emit_block(CodegenContext *context, IRBlock *block) {
  MIRValue_x86_64 mir_block = {0};
  mir_block.instruction_form = I_FORM_IRBLOCK;
  mir_block.ir_block = block;
  append_mir(context->mir, mir_block, NULL);

  list_foreach (IRInstruction*, instruction, block->instructions) {
    emit_instruction(context, instruction);
  }
}

static void emit_function(CodegenContext *context, IRFunction *function) {
  codegen_prologue(context, function);
  // Save all callee-saved registers in use in the function.
  for (Register i = 1; i < sizeof(function->registers_in_use) * 8; ++i) {
    if ((size_t)function->registers_in_use & ((size_t)1 << i) && is_callee_saved(i))
      append_mir(context->mir, mir_reg(I_PUSH, i, r64), NULL);
  }
  list_foreach (IRBlock*, block, function->blocks) { emit_block(context, block); }
  // NOTE: Epilogue is generated by `return` instruction.
}

MIRVector select_instructions(CodegenContext *context) {
  MIRVector mir = {0};
  context->mir = &mir;
  foreach_ptr (IRFunction*, function, context->functions) {
    MIRValue_x86_64 mir_function = {0};
    mir_function.instruction_form = I_FORM_IRFUNCTION;
    mir_function.ir_function = function;
    append_mir(context->mir, mir_function, NULL);
    if (!function->is_extern) emit_function(context, function);
  }
  return mir;
}

