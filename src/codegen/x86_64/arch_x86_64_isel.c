#include <codegen/x86_64/arch_x86_64_isel.h>

#include <codegen.h>
#include <codegen/intermediate_representation.h>
#include <codegen/machine_ir.h>
#include <codegen/x86_64/arch_x86_64_common.h>
#include <codegen/x86_64/arch_x86_64.h>
#include <opt.h>
#include <utils.h>

const char *mir_x86_64_opcode_mnemonic(uint32_t opcode) {
  STATIC_ASSERT(MX64_COUNT == 29, "Exhaustive handling of x86_64 opcodes (string conversion)");
  ASSERT(opcode >= MIR_ARCH_START && opcode < MX64_END, "Opcode is not x86_64 opcode");
  switch ((MIROpcodex86_64)opcode) {
  case MX64_ADD: return "add";
  case MX64_SUB: return "sub";
  case MX64_IMUL: return "imul";
  case MX64_DIV: return "div";
  case MX64_IDIV: return "idiv";
  case MX64_XOR: return "xor";
  case MX64_CMP: return "cmp";
  case MX64_TEST: return "test";
  case MX64_CWD: return "cwd";
  case MX64_CDQ: return "cdq";
  case MX64_CQO: return "cqo";
  case MX64_SETCC: return "setcc";
  case MX64_SAL: return "sal";
  case MX64_SAR: return "sar";
  case MX64_SHR: return "shr";
  case MX64_AND: return "and";
  case MX64_OR: return "or";
  case MX64_NOT: return "not";
  case MX64_PUSH: return "push";
  case MX64_POP: return "pop";
  case MX64_CALL: return "call";
  case MX64_JMP: return "jmp";
  case MX64_RET: return "ret";
  case MX64_JCC: return "jcc";
  case MX64_MOV: return "mov";
  case MX64_LEA: return "lea";
  case MX64_MOVSX: return "movsx";
  case MX64_MOVZX: return "movzx";
  case MX64_XCHG: return "xchg";
  case MX64_END: return "end";
  case MX64_COUNT: return "count";
  default: break;
  }
  ICE("Unhandled x86_64 MIR instruction in string conversion");
}

static MIROpcodex86_64 gmir_binop_to_x64(MIROpcodeCommon opcode) {
  DBGASSERT(opcode < MIR_COUNT, "Argument is meant to be a general MIR instruction opcode.");
  STATIC_ASSERT(MIR_COUNT == 39, "Exhaustive handling of binary operator machine instruction opcodes for x86_64 backend");
  switch (opcode) {
  case MIR_ADD: return MX64_ADD;
  case MIR_SUB: return MX64_SUB;
  case MIR_MUL: return MX64_IMUL;
  case MIR_AND: return MX64_AND;
  case MIR_OR: return MX64_OR;
  default: ICE("Unhandled binary operator general MIRInstruction opcode: %u (%s)",
               (unsigned)opcode, mir_common_opcode_mnemonic((uint32_t)opcode));
  }
}

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

static void append_mir(MIRInstructionVector *mir, MIRValue_x86_64 value, IRInstruction *origin) {
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
    if (inst->imm <= UINT32_MAX)
      append_mir(context->mir, mir_imm_to_reg(I_MOV, (int64_t)inst->imm, inst->result, r32), inst);
    else if (inst->imm <= UINT64_MAX)
      append_mir(context->mir, mir_imm_to_reg(I_MOV, (int64_t)inst->imm, inst->result, r64), inst);
    else ICE("Unsupported integer literal immediate on x86_64 (out of range)");
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
  case IR_EQ: FALLTHROUGH;
  case IR_NE: FALLTHROUGH;
  case IR_LT: FALLTHROUGH;
  case IR_GT: FALLTHROUGH;
  case IR_LE: FALLTHROUGH;
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

MIRInstructionVector select_instructions(CodegenContext *context) {
  MIRInstructionVector mir = {0};
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

MIRFunctionVector select_instructions2(MIRFunctionVector input) {
  MIRFunctionVector mir = {0};
  foreach_ptr (MIRFunction*, mir_f, input) {
    size_t extra_instruction_reg = mir_f->inst_count + MIR_ARCH_START;
    // Create new function
    MIRFunction *f = mir_function(mir_f->origin);
    vector_push(mir, f);
    if (!mir_f->blocks.size) continue;
    foreach_ptr (MIRBlock*, mir_bb, mir_f->blocks) {
      // Create new block
      (void)mir_block(f, mir_bb->origin);
      foreach_ptr (MIRInstruction*, inst, mir_bb->instructions) {
        // We have to pass through register-assigned instructions, assumedly.
        // TODO: Enable this once we wait until after isel to do RA.
        //if (inst->origin && inst->origin->result) {
        //  MIRInstruction *something = mir_makecopy(inst);
        //  inst->lowered = something;
        //  mir_push_with_reg(f, something, inst->reg);
        //  continue;
        //}

        switch ((MIROpcodeCommon)inst->opcode) {

        case MIR_REGISTER:
        case MIR_IMMEDIATE:
          break;

        case MIR_BITCAST:
        case MIR_BLOCK:
        case MIR_STATIC_REF:
        case MIR_FUNC_REF: {
          MIRInstruction *something = mir_makecopy(inst);
          inst->lowered = something;
          mir_push_with_reg(f, something, inst->reg);
        } break;

        case MIR_CALL: {
          MIROperand *op = mir_get_op(inst, 0);
          MIRInstruction *call = mir_makenew(MX64_CALL);
          call->origin = inst->origin;
          inst->lowered = call;
          switch (op->kind) {
          case MIR_OP_NAME: {
            call->x64.instruction_form = I_FORM_NAME;
            mir_add_op(call, mir_op_name(op->value.name));
            mir_push_with_reg(f, call, inst->reg);
          } break;
          case MIR_OP_BLOCK: {
            call->x64.instruction_form = I_FORM_NAME;
            mir_add_op(call, mir_op_name(op->value.block->name.data));
            mir_push_with_reg(f, call, inst->reg);
          } break;
          case MIR_OP_FUNCTION: {
            call->x64.instruction_form = I_FORM_NAME;
            mir_add_op(call, mir_op_name(op->value.function->name.data));
            mir_push_with_reg(f, call, inst->reg);
          } break;
          case MIR_OP_REGISTER: {
            call->x64.instruction_form = I_FORM_REG;
            mir_add_op(call, *op);
            mir_push_with_reg(f, call, inst->reg);
          } break;
          default: ICE("Unhandled call operand kind: %d", (int)op->kind);
          }
        } break;

        case MIR_LOAD: {
          RegSize size = (RegSize)-1u;
          MIRInstruction *ref = mir_find_by_vreg(f, mir_get_op(inst, 0)->value.reg.value);
          /// Load from a static variable.
          if (ref->opcode == MIR_STATIC_REF) {
            IRInstruction *static_ref = ref->origin;
            size = regsize_from_bytes(type_sizeof(static_ref->type->pointer.to));
            if (size == r8 || size == r16) {
              MIRInstruction *xor_zero = mir_makenew(MX64_XOR);
              xor_zero->origin = inst->origin;
              xor_zero->x64.instruction_form = I_FORM_REG_TO_REG;
              MIROperand reg = mir_op_register(inst->origin->result, r32);
              mir_add_op(xor_zero, reg);
              mir_add_op(xor_zero, reg);
              mir_push_with_reg(f, xor_zero, (MIRRegister)extra_instruction_reg++);
            }
            MIRInstruction *move = mir_makenew(MX64_MOV);
            inst->lowered = move;
            move->origin = inst->origin;
            move->x64.instruction_form = I_FORM_NAME_TO_REG;
            MIROperand address_register = mir_op_register(REG_RIP, r64);
            MIROperand name = mir_op_name(static_ref->static_ref->name.data);
            mir_add_op(move, address_register);
            mir_add_op(move, name);
            mir_add_op(move, mir_op_immediate(0));
            mir_push_with_reg(f, move, inst->reg);
            *mir_get_op(move, 2) = mir_op_reference(move);
          }

            /// Load from a local.
          else if (ref->opcode == MIR_ALLOCA) {
            size = regsize_from_bytes((size_t)mir_get_op(inst, 0)->value.imm);
            if (size == r8 || size == r16) {
              MIRInstruction *xor_zero = mir_makenew(MX64_XOR);
              xor_zero->origin = inst->origin;
              xor_zero->x64.instruction_form = I_FORM_REG_TO_REG;
              MIROperand reg = mir_op_register(inst->origin->result, r32);
              mir_add_op(xor_zero, reg);
              mir_add_op(xor_zero, reg);
              mir_push_with_reg(f, xor_zero, (MIRRegister)extra_instruction_reg++);
            }
            MIRInstruction *move = mir_makenew(MX64_MOV);
            inst->lowered = move;
            move->origin = inst->origin;
            move->x64.instruction_form = I_FORM_MEM_TO_REG;
            MIROperand address_register = mir_op_register(REG_RBP, r64);
            // TODO: This is bad. Maybe we should store offset in MIR
            // alloca? Or maybe we should be calculating offsets as we go,
            // here.
            MIROperand offset = mir_op_immediate(- (i64)inst->origin->operand->alloca.offset);
            mir_add_op(move, address_register);
            mir_add_op(move, offset);
            mir_add_op(move, mir_op_immediate(0)); // fake entry
            mir_push_with_reg(f, move, inst->reg);
            *mir_get_op(move, 2) = mir_op_reference(move);
          }

          /// Load from a pointer
          else {
            size = regsize_from_bytes(type_sizeof(inst->origin->operand->type));
            if (size == r8 || size == r16) {
              MIRInstruction *xor_zero = mir_makenew(MX64_XOR);
              xor_zero->origin = inst->origin;
              xor_zero->x64.instruction_form = I_FORM_REG_TO_REG;
              MIROperand reg = mir_op_reference(xor_zero);
              mir_add_op(xor_zero, reg);
              mir_add_op(xor_zero, reg);
              mir_push_with_reg(f, xor_zero, (MIRRegister)extra_instruction_reg++);
            }
            MIRInstruction *move = mir_makenew(MX64_MOV);
            inst->lowered = move;
            move->origin = inst->origin;
            move->x64.instruction_form = I_FORM_MEM_TO_REG;
            MIROperand address = mir_op_reference(inst->origin->operand->machine_inst);
            MIROperand offset = mir_op_immediate(0);
            mir_add_op(move, address);
            mir_add_op(move, offset);
            mir_add_op(move, mir_op_immediate(0)); // fake entry
            mir_push_with_reg(f, move, inst->reg);
            *mir_get_op(move, 2) = mir_op_reference(move);
          }
        } break;

        case MIR_STORE: {
          RegSize size = regsize_from_bytes(type_sizeof(inst->origin->store.value->type));

          MIRInstruction *store = mir_makenew(MX64_MOV);
          inst->lowered = store;
          store->origin = inst->origin;

          MIROperand *op_value = mir_get_op(inst, 0);
          mir_add_op(store, *op_value);

          MIROperand *op_address = mir_get_op(inst, 1);
          switch (op_address->kind) {
          case MIR_OP_STATIC_REF: {
            /// Store to a static variable.
            store->x64.instruction_form = I_FORM_REG_TO_NAME;
            MIROperand name = mir_op_name(op_address->value.static_ref->name.data);
            MIROperand offset = mir_op_immediate(0);
            mir_add_op(store, name);
            mir_add_op(store, offset);
          } break;
          case MIR_OP_LOCAL_REF: {
            /// Store to a local.
            store->x64.instruction_form = I_FORM_REG_TO_MEM;
            MIROperand address = mir_op_register(REG_RBP, r64);
            MIROperand offset = mir_op_immediate(- (i64)op_address->value.local_ref->offset);
            mir_add_op(store, address);
            mir_add_op(store, offset);
          } break;
          default: {
            /// Store to a pointer.
            store->x64.instruction_form = I_FORM_REG_TO_MEM;
            mir_add_op(store, *op_address);
            mir_add_op(store, mir_op_immediate(0));
          } break;
          }

          mir_push_with_reg(f, store, inst->reg);

        } break;

        case MIR_RETURN: {
          if (inst->operand_count) {
            MIROperand *op = mir_get_op(inst, 0);
            MIRInstruction *move = mir_makenew(MX64_MOV);
            inst->lowered = move;
            move->origin = inst->origin;
            move->x64.instruction_form = I_FORM_REG_TO_REG;
            mir_add_op(move, *op);
            // FIXME: This should be the return register based on calling convention.
            mir_add_op(move, mir_op_register(REG_RAX, r64));
            mir_push_with_reg(f, move, (MIRRegister)extra_instruction_reg++);
          }
          MIRInstruction *ret = mir_makenew(MX64_RET);
          inst->lowered = ret;
          ret->origin = inst->origin;
          ret->x64.instruction_form = I_FORM_NONE;
          mir_push_with_reg(f, ret, inst->reg);
        } break;

        case MIR_COPY: {
          MIRInstruction *move = mir_makenew(MX64_MOV);
          inst->lowered = move;
          move->origin = inst->origin;
          move->x64.instruction_form = I_FORM_REG_TO_REG;
          mir_add_op(move, *mir_get_op(inst, 0));
          mir_add_op(move, mir_op_immediate(0)); // fake entry
          mir_push_with_reg(f, move, inst->reg);
          *mir_get_op(move, 1) = mir_op_reference(move);
        } break;

        case MIR_BRANCH: {
          /// Only emit a jump if the target isn’t the next block.
          if (!optimise || (inst->origin->parent_block && inst->origin->destination_block != inst->origin->parent_block->next && !inst->origin->parent_block->done)) {
            MIRInstruction *jmp = mir_makenew(MX64_JMP);
            inst->lowered = jmp;
            jmp->origin = inst->origin;
            jmp->x64.instruction_form = I_FORM_NAME;
            mir_add_op(jmp, mir_op_name(mir_get_op(inst, 0)->value.block->name.data));
            mir_push_with_reg(f, jmp, inst->reg);
          }
          if (optimise && inst->origin->parent_block) inst->origin->parent_block->done = true;
        } break;
        case MIR_BRANCH_CONDITIONAL: {
          //IRBranchConditional *branch = &inst->origin->cond_br;

          MIROperand *cond = mir_get_op(inst, 0);
          MIROperand *then = mir_get_op(inst, 1);
          MIROperand *else_ = mir_get_op(inst, 2);

          MIRInstruction *test = mir_makenew(MX64_TEST);
          inst->lowered = test;
          test->origin = inst->origin;
          test->x64.instruction_form = I_FORM_REG_TO_REG;

          mir_add_op(test, *cond);
          mir_add_op(test, *cond);
          mir_push_with_reg(f, test, inst->reg);

          /// If either target is the next block, arrange the jumps in such a way
          /// that we can save one and simply fallthrough to the next block.
          if (optimise && then->value.block->origin == inst->origin->parent_block->next) {
            MIRInstruction *jcc = mir_makenew(MX64_JCC);
            jcc->origin = inst->origin;
            jcc->x64.instruction_form = I_FORM_JCC;
            mir_add_op(jcc, mir_op_immediate(JUMP_TYPE_Z));
            mir_add_op(jcc, *else_);
            mir_push_with_reg(f, jcc, (MIRRegister)extra_instruction_reg++);
          }
          else if (optimise && else_->value.block->origin == inst->origin->parent_block->next) {
            MIRInstruction *jcc = mir_makenew(MX64_JCC);
            jcc->origin = inst->origin;
            jcc->x64.instruction_form = I_FORM_JCC;
            mir_add_op(jcc, mir_op_immediate(JUMP_TYPE_NZ));
            mir_add_op(jcc, *then);
            mir_push_with_reg(f, jcc, (MIRRegister)extra_instruction_reg++);
          }
          else {

            MIRInstruction *jcc = mir_makenew(MX64_JCC);
            jcc->origin = inst->origin;
            jcc->x64.instruction_form = I_FORM_JCC;
            mir_add_op(jcc, mir_op_immediate(JUMP_TYPE_Z));
            mir_add_op(jcc, *else_);

            MIRInstruction *jmp = mir_makenew(MX64_JMP);
            jmp->origin = inst->origin;
            jmp->x64.instruction_form = I_FORM_NAME;
            mir_add_op(jmp, *then);

            mir_push_with_reg(f, jcc, (MIRRegister)extra_instruction_reg++);
            mir_push_with_reg(f, jmp, (MIRRegister)extra_instruction_reg++);
          }

          if (optimise && inst->origin->parent_block) inst->origin->parent_block->done = true;
        } break;

        case MIR_ALLOCA: {
          MIRInstruction *alloca = mir_makenew(MX64_LEA);
          alloca->origin = inst->origin;
          inst->lowered = alloca;
          alloca->x64.instruction_form = I_FORM_MEM_TO_REG;
          MIROperand address_register = mir_op_register(REG_RBP, r64);
          MIROperand offset = mir_op_immediate(- (i64)inst->origin->alloca.offset);
          mir_add_op(alloca, address_register);
          mir_add_op(alloca, offset);
          mir_add_op(alloca, mir_op_immediate(0)); // fake entry
          mir_push_with_reg(f, alloca, inst->reg);
          *mir_get_op(alloca, 2) = mir_op_reference(alloca);
        } break;

        // Single-instruction commutative binary operations
        case MIR_MUL: FALLTHROUGH;
        case MIR_OR: FALLTHROUGH;
        case MIR_AND: FALLTHROUGH;
        case MIR_ADD: {
          RegSize size = regsize_from_bytes(type_sizeof(inst->origin->type));

          MIROperand lhs = *mir_get_op(inst, 0);
          MIROperand rhs = *mir_get_op(inst, 1);

          // To add two immediates, we must first load one into a register.
          if (lhs.kind == MIR_OP_IMMEDIATE && rhs.kind == MIR_OP_IMMEDIATE) {
            MIRInstruction *load_imm = mir_makenew(MX64_MOV);
            load_imm->origin = inst->origin;
            load_imm->x64.instruction_form = I_FORM_IMM_TO_REG;
            mir_add_op(load_imm, rhs);
            mir_add_op(load_imm, mir_op_immediate(0)); // fake entry
            mir_push_with_reg(f, load_imm, (MIRRegister)extra_instruction_reg++);
            *mir_get_op(load_imm, 1) = mir_op_reference(load_imm);
            rhs = mir_op_reference(load_imm);
          }
          else if (rhs.kind == MIR_OP_IMMEDIATE) {
            MIROperand tmp = lhs;
            lhs = rhs;
            rhs = tmp;
          }

          MIRInstruction *m_inst = mir_makenew((uint32_t)gmir_binop_to_x64((MIROpcodeCommon)inst->opcode));
          m_inst->origin = inst->origin;
          inst->lowered = m_inst;
          mir_add_op(m_inst, lhs);
          mir_add_op(m_inst, rhs);
          mir_push_with_reg(f, m_inst, inst->reg);
        } break;

        // Single-instruction non-commutative operations
        case MIR_SUB: {
          RegSize size = regsize_from_bytes(type_sizeof(inst->origin->type));

          MIROperand lhs = *mir_get_op(inst, 0);
          MIROperand rhs = *mir_get_op(inst, 1);

          // To subtract two immediates, we must first load the left hand side into a register.
          if (lhs.kind == MIR_OP_IMMEDIATE && rhs.kind == MIR_OP_IMMEDIATE) {
            MIRInstruction *load_imm = mir_makenew(MX64_MOV);
            load_imm->origin = inst->origin;
            load_imm->x64.instruction_form = I_FORM_IMM_TO_REG;
            mir_add_op(load_imm, lhs);
            mir_add_op(load_imm, mir_op_immediate(0)); // fake entry
            mir_push_with_reg(f, load_imm, (MIRRegister)extra_instruction_reg++);
            *mir_get_op(load_imm, 1) = mir_op_reference(load_imm);
            lhs = mir_op_reference(load_imm);
          }
          else if (lhs.kind == MIR_OP_IMMEDIATE) {
            TODO("Handle immediate - value form of non-commutative binary operations");
          }

          MIRInstruction *m_inst = mir_makenew((uint32_t)gmir_binop_to_x64((MIROpcodeCommon)inst->opcode));
          m_inst->origin = inst->origin;
          inst->lowered = m_inst;
          mir_add_op(m_inst, lhs);
          mir_add_op(m_inst, rhs);
          mir_push_with_reg(f, m_inst, inst->reg);
        } break;

        // Comparisons
        case MIR_EQ: FALLTHROUGH;
        case MIR_NE: FALLTHROUGH;
        case MIR_LT: FALLTHROUGH;
        case MIR_GT: FALLTHROUGH;
        case MIR_LE: FALLTHROUGH;
        case MIR_GE: {

          enum ComparisonType type = COMPARE_COUNT;
          switch (inst->opcode) {
          case MIR_EQ: type = COMPARE_EQ; break;
          case MIR_NE: type = COMPARE_NE; break;
          case MIR_LT: type = COMPARE_LT; break;
          case MIR_GT: type = COMPARE_GT; break;
          case MIR_LE: type = COMPARE_LE; break;
          case MIR_GE: type = COMPARE_GE; break;
          default: ICE("Unsupported IR instruction in codegen_comparison: %d (%s)",
                       inst->opcode, mir_common_opcode_mnemonic(inst->opcode));
          }

          // Perform the comparison.
          // TODO: register size
          MIRInstruction *cmp = mir_makenew(MX64_CMP);
          cmp->origin = inst->origin;
          cmp->x64.instruction_form = I_FORM_REG_TO_REG;
          mir_add_op(cmp, *mir_get_op(inst, 1));
          mir_add_op(cmp, *mir_get_op(inst, 0));

          mir_push_with_reg(f, cmp, (MIRRegister)extra_instruction_reg++);

          // IF YOU REPLACE THIS WITH A XOR IT WILL BREAK HORRIBLY
          // We use MOV because it doesn't set flags.
          // TODO: 32-bit move
          MIRInstruction *move = mir_makenew(MX64_MOV);
          move->origin = inst->origin;
          move->x64.instruction_form = I_FORM_IMM_TO_REG;
          mir_add_op(move, mir_op_immediate(0));
          mir_add_op(move, mir_op_immediate(0)); // fake entry
          mir_push_with_reg(f, move, (MIRRegister)extra_instruction_reg++);

          MIRInstruction *setcc = mir_makenew(MX64_SETCC);
          inst->lowered = setcc;
          setcc->origin = inst->origin;
          setcc->x64.instruction_form = I_FORM_SETCC;
          mir_add_op(setcc, mir_op_immediate(type));
          mir_add_op(setcc, mir_op_immediate(0));
          mir_push_with_reg(f, setcc, inst->reg);
          // Destination of move is same vreg that setcc operates on.
          *mir_get_op(setcc, 1) = mir_op_reference(setcc);
          *mir_get_op(move, 1) = mir_op_reference(setcc);

        } break;

        case MIR_NOT: {
          MIROperand op = *mir_get_op(inst, 0);

          // To not (reverse bits) of an immediate, we must first load it into a register.
          if (op.kind == MIR_OP_IMMEDIATE) {
            MIRInstruction *load_imm = mir_makenew(MX64_MOV);
            load_imm->origin = inst->origin;
            load_imm->x64.instruction_form = I_FORM_IMM_TO_REG;
            mir_add_op(load_imm, op);
            mir_add_op(load_imm, mir_op_immediate(0)); // fake entry
            mir_push_with_reg(f, load_imm, (MIRRegister)extra_instruction_reg++);
            *mir_get_op(load_imm, 1) = mir_op_reference(load_imm);
            op = mir_op_reference(load_imm);
          }

          MIRInstruction *m_inst = mir_makenew(MX64_NOT);
          m_inst->origin = inst->origin;
          inst->lowered = m_inst;
          mir_add_op(m_inst, op);
          mir_push_with_reg(f, m_inst, inst->reg);
        } break;

        case MIR_UNREACHABLE:
        case MIR_DIV:
        case MIR_MOD:
        case MIR_SHL:
        case MIR_SAR:
        case MIR_SHR:
        case MIR_ZERO_EXTEND:
        case MIR_SIGN_EXTEND:
        case MIR_TRUNCATE:
          TODO("[x86_64]:isel: \"%s\" MIR instruction opcode", mir_common_opcode_mnemonic(inst->opcode));

        case MIR_ARCH_START:
        case MIR_PHI:
        case MIR_PARAMETER:
        case MIR_LIT_INTEGER:
        case MIR_LIT_STRING:
        case MIR_COUNT:
          UNREACHABLE();

        }
      }
    }
  }
  return mir;
}

