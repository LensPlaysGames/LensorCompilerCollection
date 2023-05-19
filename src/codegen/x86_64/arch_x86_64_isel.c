#include <codegen/x86_64/arch_x86_64_isel.h>

#include <codegen.h>
#include <codegen/register_allocation.h>
#include <codegen/intermediate_representation.h>
#include <codegen/instruction_selection.h>
#include <codegen/machine_ir.h>
#include <codegen/x86_64/arch_x86_64_common.h>
#include <codegen/x86_64/arch_x86_64.h>
#include <opt.h>
#include <utils.h>

const char *mir_x86_64_opcode_mnemonic(uint32_t opcode) {
  STATIC_ASSERT(MX64_COUNT == 29, "Exhaustive handling of x86_64 opcodes (string conversion)");
  //ASSERT(opcode >= MIR_ARCH_START && opcode < MX64_END, "Opcode is not x86_64 opcode");
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
  return mir_common_opcode_mnemonic(opcode);
}

static MIROpcodex86_64 gmir_binop_to_x64(MIROpcodeCommon opcode) {
  DBGASSERT(opcode < MIR_COUNT, "Argument is meant to be a general MIR instruction opcode.");
  STATIC_ASSERT(MIR_COUNT == 38, "Exhaustive handling of binary operator machine instruction opcodes for x86_64 backend");
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

/* FIXME: Remove (still need for reference...)
static void divmod(CodegenContext *context, IRInstruction *inst) {
  ASSERT(inst->kind == IR_DIV || inst->kind == IR_MOD, "divmod must be passed a div or mod instruction!");
  // Dividend of div/mod goes in rdx:rax; divisor must not be in those registers.
  ASSERT(inst->rhs->result != REG_RAX,
         "Register allocation must not allocate RAX to divisor.");
  ASSERT(inst->rhs->result != REG_RDX,
         "Register allocation must not allocate RDX to divisor.");
  RegSize lhs_size = regsize_from_bytes(type_sizeof(inst->lhs->type));
  RegSize rhs_size = regsize_from_bytes(type_sizeof(inst->rhs->type));

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
*/

static void emit_instruction(CodegenContext *context, IRInstruction *inst) {
  STATIC_ASSERT(IR_COUNT == 38, "Handle all IR instructions");

  if (annotate_code) {
    // TODO: Base comment syntax on dialect or smth.
    fprint(context->code, ";;#;");
    thread_use_colours = false;
    ir_femit_instruction(context->code, inst);
    thread_use_colours = true;
  }

  /*
  switch (inst->kind) {
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

  case IR_CALL: {
    // Save caller saved registers used in caller function.
    ASSERT(inst->parent_block, "call instruction null block");
    ASSERT(inst->parent_block->function, "block has null function");

    // Tail call.
    if (inst->call.tail_call) {
      // Restore the frame pointer if we have one.
      //codegen_epilogue(context, inst->parent_block->function);
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

    // Push caller saved registers
    // TODO: Don't push registers that are used for arguments.
    for (Register i = REG_RAX + 1; i < sizeof(func_regs) * 8; ++i)
      if (func_regs & ((usz)1 << i) && is_caller_saved(i))
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
      if (func_regs & ((usz)1 << i) && is_caller_saved(i))
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
  */
}

void isel_x86_64_env(ISelEnvironment *env) {
#define ADD_OPCODE(opcode) isel_env_add_opcode(env, STR(CAT(MX64_, opcode)), CAT(MX64_, opcode));
  ALL_MX64_INSTRUCTIONS(ADD_OPCODE)
#undef ADD_OPCODE
#define ADD_HWREG(reg, rname64, rname32, rname16, rname8)               \
    isel_env_add_register(env, rname64, (VReg){CAT(REG_, reg), r64});   \
    isel_env_add_register(env, rname32, (VReg){CAT(REG_, reg), r32});   \
    isel_env_add_register(env, rname16, (VReg){CAT(REG_, reg), r16});   \
    isel_env_add_register(env, rname8, (VReg){CAT(REG_, reg), r8});
  FOR_ALL_X86_64_REGISTERS(ADD_HWREG)
#undef ADD_HWREG
}
