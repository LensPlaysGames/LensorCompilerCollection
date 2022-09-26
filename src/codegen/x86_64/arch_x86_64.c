#include <codegen.h>
#include <codegen/ra.h>
#include <codegen/ir.h>
#include <codegen/ir_backend.h>
#include <codegen/x86_64/arch_x86_64.h>

#include <error.h>
#include <inttypes.h>
#include <parser.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <typechecker.h>

/// This is used for defining lookup tables etc. and
/// ensures that the registers are always in the correct
/// order
#define FOR_ALL_REGISTERS(F)            \
  /* Caller-saved */                    \
  F(RAX, "rax", "eax", "ax", "al")      \
  F(RDI, "rdi", "edi", "di", "dil")     \
  F(RSI, "rsi", "esi", "si", "sil")     \
  F(RDX, "rdx", "edx", "dx", "dl")      \
  F(RCX, "rcx", "ecx", "cx", "cl")      \
  F(R8,  "r8", "r8d", "r8w", "r8b")     \
  F(R9,  "r9", "r9d", "r9w", "r9b")     \
  F(R10, "r10", "r10d", "r10w", "r10b") \
  F(R11, "r11", "r11d", "r11w", "r11b") \
  /* Callee-saved */                    \
  F(RBX, "rbx", "ebx", "bx", "bl")      \
  F(R12, "r12", "r12d", "r12w", "r12b") \
  F(R13, "r13", "r13d", "r13w", "r13b") \
  F(R14, "r14", "r14d", "r14w", "r14b") \
  F(R15, "r15", "r15d", "r15w", "r15b") \
  /* Special */                         \
  F(RBP, "rbp", "ebp", "bp", "bpl")     \
  F(RSP, "rsp", "esp", "sp", "spl")     \
  F(RIP, "rip", "eip", "ip", "ipl")

#define GENERAL_PURPOSE_REGISTER_COUNT (13)
#define FIRST_CALLER_SAVED_REGISTER REG_RAX
#define LAST_CALLER_SAVED_REGISTER REG_R11

#define DEFINE_REGISTER_ENUM(name, ...) REG_##name,
#define REGISTER_NAME_64(ident, name, ...) name,
#define REGISTER_NAME_32(ident, name, name_32, ...) name_32,
#define REGISTER_NAME_16(ident, name, name_32, name_16, ...) name_16,
#define REGISTER_NAME_8(ident, name, name_32, name_16, name_8, ...) name_8,

/// Lookup tables for register names.
#define DEFINE_REGISTER_NAME_LOOKUP_FUNCTION(name, bits)                                        \
  MAYBE_UNUSED static const char *name(Register descriptor) {                                   \
    static const char* register_names[] = { FOR_ALL_REGISTERS(REGISTER_NAME_##bits) };          \
    if (descriptor == REG_NONE || descriptor >= REG_COUNT) {                                    \
      PANIC("Could not find register with descriptor of %d\n", descriptor);                     \
    }                                                                                           \
    return register_names[descriptor - 1];                                                          \
  }

enum Registers {
  REG_NONE,
  FOR_ALL_REGISTERS(DEFINE_REGISTER_ENUM)
  REG_COUNT
};

static const Register linux_argument_registers[] = {
    REG_RDI, REG_RSI, REG_RDX, REG_RCX, REG_R8, REG_R9
};

static const Register mswin_argument_registers[] = {
    REG_RCX, REG_RDX, REG_R8, REG_R9
};

/// Define register_name and friends.
DEFINE_REGISTER_NAME_LOOKUP_FUNCTION(register_name, 64)
DEFINE_REGISTER_NAME_LOOKUP_FUNCTION(register_name_32, 32)
DEFINE_REGISTER_NAME_LOOKUP_FUNCTION(register_name_16, 16)
DEFINE_REGISTER_NAME_LOOKUP_FUNCTION(register_name_8, 8)

#undef REGISTER_NAME_64
#undef REGISTER_NAME_32
#undef REGISTER_NAME_16
#undef REGISTER_NAME_8

#undef DEFINE_REGISTER_ENUM
#undef DEFINE_REGISTER_NAME_LOOKUP_FUNCTION


/// Creates a context for the X86_64 architecture.
CodegenContext *codegen_context_x86_64_create(CodegenContext *parent) {
  CodegenContext *cg_ctx = calloc(1,sizeof(CodegenContext));

  // If this is the top level context, create the registers.
  // Otherwise, shallow copy register pool to child context.
  if (!parent) {
    // FIXME(Sirraide): These heap allocations are jank.
    cg_ctx->func_count = calloc(1, sizeof(size_t));
    cg_ctx->block_count = calloc(1, sizeof(size_t));
  } else {
    *cg_ctx = *parent;
  }

  cg_ctx->parent = parent;
  cg_ctx->locals = environment_create(NULL);
  cg_ctx->locals_base_offset = 32;
  return cg_ctx;
}

/// Free a context created by context_create.
void codegen_context_x86_64_free(CodegenContext *ctx) {
  // TODO(sirraide): Free environment.
  free(ctx);
}

/// Types of conditional jump instructions (Jcc).
/// Do NOT reorder these.
enum IndirectJumpType {
  JUMP_TYPE_A,
  JUMP_TYPE_AE,
  JUMP_TYPE_B,
  JUMP_TYPE_BE,
  JUMP_TYPE_C,
  JUMP_TYPE_E,
  JUMP_TYPE_Z,
  JUMP_TYPE_G,
  JUMP_TYPE_GE,
  JUMP_TYPE_L,
  JUMP_TYPE_LE,
  JUMP_TYPE_NA,
  JUMP_TYPE_NAE,
  JUMP_TYPE_NB,
  JUMP_TYPE_NBE,
  JUMP_TYPE_NC,
  JUMP_TYPE_NE,
  JUMP_TYPE_NG,
  JUMP_TYPE_NGE,
  JUMP_TYPE_NL,
  JUMP_TYPE_NLE,
  JUMP_TYPE_NO,
  JUMP_TYPE_NP,
  JUMP_TYPE_NS,
  JUMP_TYPE_NZ,
  JUMP_TYPE_O,
  JUMP_TYPE_P,
  JUMP_TYPE_PE,
  JUMP_TYPE_PO,
  JUMP_TYPE_S,

  JUMP_TYPE_COUNT,
};

/// Do NOT reorder these.
static const char *jump_type_names[] = {
    "a",
    "ae",
    "b",
    "be",
    "c",
    "e",
    "z",
    "g",
    "ge",
    "l",
    "le",
    "na",
    "nae",
    "nb",
    "nbe",
    "nc",
    "ne",
    "ng",
    "nge",
    "nl",
    "nle",
    "no",
    "np",
    "ns",
    "nz",
    "o",
    "p",
    "pe",
    "po",
    "s",
};

// TODO: All instructions we use in x86_64 should be in this enum.
enum Instructions {
  /// Arithmetic instructions.
  I_ADD,
  I_SUB,
  // I_MUL,
  I_IMUL,
  // I_DIV,
  I_IDIV,
  I_XOR,
  I_CMP,
  I_TEST,
  I_CQO,
  I_SETCC,
  I_SAL, ///< Reg reg | Immediate imm, Reg reg
  I_SHL = I_SAL,
  I_SAR, ///< Reg reg | Immediate imm, Reg reg
  I_SHR, ///< Reg reg | Immediate imm, Reg reg

  /// Stack instructions.
  I_PUSH,
  I_POP,

  /// Control flow.
  I_CALL,
  I_JMP, ///< const char* label | Reg reg
  I_RET,
  I_JCC, ///< enum IndirectJumpType type, const char* label

  /// Memory stuff.
  I_MOV,
  I_LEA,

  /// Using this for anything other than Reg <-> Reg is a VERY bad
  /// idea unless you know what you're doing.
  I_XCHG,

  I_COUNT
};

enum InstructionOperands {
  IMMEDIATE, ///< int64_t imm
  MEMORY,    ///< Reg reg, int64_t offset
  REGISTER,  ///< Reg reg
  NAME,      ///< const char* name

  IMMEDIATE_TO_REGISTER, ///< int64_t imm, Reg dest
  IMMEDIATE_TO_MEMORY,   ///< int64_t imm, Reg address, int64_t offset
  MEMORY_TO_REGISTER,    ///< Reg address, int64_t offset, Reg dest
  NAME_TO_REGISTER,      ///< Reg address, const char* name, Reg dest
  REGISTER_TO_MEMORY,    ///< Reg src, Reg address, int64_t offset
  REGISTER_TO_REGISTER,  ///< Reg src, Reg dest
  REGISTER_TO_NAME,      ///< Reg src, Reg address, const char* name
};

const char *comparison_suffixes[COMPARE_COUNT] = {
    "e",
    "ne",
    "l",
    "le",
    "g",
    "ge",
};

MAYBE_UNUSED
static const char *instruction_mnemonic(CodegenContext *context, enum Instructions instruction) {
  STATIC_ASSERT(I_COUNT == 21, "Must exhaustively handle all instructions.");
  // x86_64 instructions that aren't different across syntaxes can go here!
  switch (instruction) {
    default: break;
    case I_ADD: return "add";
    case I_SUB: return "sub";
    // case I_MUL: return "mul";
    case I_IMUL: return "imul";
    // case I_DIV: return "div";
    case I_IDIV: return "idiv";
    case I_SAL: return "sal";
    case I_SAR: return "sar";
    case I_SHR: return "shr";
    case I_PUSH: return "push";
    case I_POP: return "pop";
    case I_XOR: return "xor";
    case I_CMP: return "cmp";
    case I_CALL: return "call";
    case I_JMP: return "jmp";
    case I_RET: return "ret";
    case I_MOV: return "mov";
    case I_XCHG: return "xchg";
    case I_LEA: return "lea";
    case I_SETCC: return "set";
    case I_TEST: return "test";
    case I_JCC: return "j";
  }

  switch (context->dialect) {
    default: PANIC("Unknown output format.");

    case CG_ASM_DIALECT_ATT:
    switch (instruction) {
      default: PANIC("Unknown instruction.");
      case I_CQO: return "cqto";
    }

    case CG_ASM_DIALECT_INTEL:
    switch (instruction) {
      default: PANIC("Unknown instruction.");
      case I_CQO: return "cqo";
    }
  }
}

static void femit
(CodegenContext *context,
 enum Instructions instruction,
 ...);

static void femit_imm_to_reg(CodegenContext *context, enum Instructions inst, va_list args) {
  int64_t immediate                        = va_arg(args, int64_t);
  Register destination_register  = va_arg(args, Register);

  const char *mnemonic = instruction_mnemonic(context, inst);
  const char *destination = register_name(destination_register);

  // Optimise zeroing out a register.
  if (inst == I_MOV && immediate == 0) {
    femit(context, I_XOR, REGISTER_TO_REGISTER, destination_register, destination_register);
    return;
  }

  switch (context->dialect) {
    case CG_ASM_DIALECT_ATT:
      fprintf(context->code, "    %s $%" PRId64 ", %%%s\n",
          mnemonic, immediate, destination);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprintf(context->code, "    %s %s, %" PRId64 "\n",
          mnemonic, destination, immediate);
      break;
    default: PANIC("Unsupported dialect %d", context->dialect);
  }
}

static void femit_imm_to_mem(CodegenContext *context, enum Instructions inst, va_list args) {
  int64_t immediate          = va_arg(args, int64_t);
  Register address_register  = va_arg(args, Register);
  int64_t offset             = va_arg(args, int64_t);

  const char *mnemonic = instruction_mnemonic(context, inst);
  const char *address = register_name(address_register);

  switch (context->dialect) {
    case CG_ASM_DIALECT_ATT:
      fprintf(context->code, "    %s $%" PRId64 ", %" PRId64 "(%%%s)\n",
          mnemonic, immediate, offset, address);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprintf(context->code, "    %s [%s + %" PRId64 "], %" PRId64 "\n",
          mnemonic, address, offset, immediate);
      break;
    default: PANIC("Unsupported dialect %d", context->dialect);
  }
}

static void femit_mem_to_reg(CodegenContext *context, enum Instructions inst, va_list args) {
  Register address_register      = va_arg(args, Register);
  int64_t offset                           = va_arg(args, int64_t);
  Register destination_register  = va_arg(args, Register);

  const char *mnemonic = instruction_mnemonic(context, inst);
  const char *address = register_name(address_register);
  const char *destination = register_name(destination_register);

  switch (context->dialect) {
    case CG_ASM_DIALECT_ATT:
      fprintf(context->code, "    %s -%" PRId64 "(%%%s), %%%s\n",
          mnemonic, offset, address, destination);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprintf(context->code, "    %s %s, [%s - %" PRId64 "]\n",
          mnemonic, destination, address, offset);
      break;
    default: PANIC("Unsupported dialect %d", context->dialect);
  }
}

static void femit_name_to_reg(CodegenContext *context, enum Instructions inst, va_list args) {
  Register address_register      = va_arg(args, Register);
  char *name                     = va_arg(args, char *);
  Register destination_register  = va_arg(args, Register);

  const char *mnemonic = instruction_mnemonic(context, inst);
  const char *address = register_name(address_register);
  const char *destination = register_name(destination_register);

  switch (context->dialect) {
    case CG_ASM_DIALECT_ATT:
      fprintf(context->code, "    %s %s(%%%s), %%%s\n",
          mnemonic, name, address, destination);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprintf(context->code, "    %s %s, [%s + %s]\n",
          mnemonic, destination, address, name);
      break;
    default: PANIC("Unsupported dialect %d", context->dialect);
  }
}

static void femit_reg_to_mem(CodegenContext *context, enum Instructions inst, va_list args) {
  Register source_register   = va_arg(args, Register);
  Register address_register  = va_arg(args, Register);
  int64_t offset             = va_arg(args, int64_t);

  const char *mnemonic = instruction_mnemonic(context, inst);
  const char *source = register_name(source_register);
  const char *address = register_name(address_register);

  switch (context->dialect) {
    case CG_ASM_DIALECT_ATT:
      if (offset) {
        fprintf(context->code, "    %s %%%s, -%" PRId64 "(%%%s)\n",
                mnemonic, source, offset, address);
      } else {
        fprintf(context->code, "    %s %%%s, (%%%s)\n",
                mnemonic, source, address);
      }
      break;
    case CG_ASM_DIALECT_INTEL:
      if (offset) {
        fprintf(context->code, "    %s [%s - %" PRId64 "], %s\n",
                mnemonic, address, offset, source);
      } else {
        fprintf(context->code, "    %s [%s], %s\n",
                mnemonic, address, source);
      }
      break;
    default: PANIC("Unsupported dialect %d", context->dialect);
  }
}

static void femit_reg_to_reg(CodegenContext *context, enum Instructions inst, va_list args) {
  Register source_register       = va_arg(args, Register);
  Register destination_register  = va_arg(args, Register);

  const char *mnemonic = instruction_mnemonic(context, inst);
  const char *source = register_name(source_register);
  const char *destination = register_name(destination_register);

  // Optimise away moves from a register to itself
  if (inst == I_MOV && source_register == destination_register) return;

  switch (context->dialect) {
    case CG_ASM_DIALECT_ATT:
      fprintf(context->code, "    %s %%%s, %%%s\n",
          mnemonic, source, destination);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprintf(context->code, "    %s %s, %s\n",
          mnemonic, destination, source);
      break;
    default: PANIC("Unsupported dialect %d", context->dialect);
  }
}

static void femit_reg_to_name(CodegenContext *context, enum Instructions inst, va_list args) {
  Register source_register  = va_arg(args, Register);
  Register address_register      = va_arg(args, Register);
  char *name                               = va_arg(args, char *);

  const char *mnemonic = instruction_mnemonic(context, inst);
  const char *source = register_name(source_register);
  const char *address = register_name(address_register);

  switch (context->dialect) {
    case CG_ASM_DIALECT_ATT:
      fprintf(context->code, "    %s %%%s, %s(%%%s)\n",
          mnemonic, source, name, address);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprintf(context->code, "    %s [%s + %s], %s\n",
          mnemonic, address, name, source);
      break;
    default: PANIC("Unsupported dialect %d", context->dialect);
  }
}

static void femit_mem(CodegenContext *context, enum Instructions inst, va_list args) {
  int64_t offset                           = va_arg(args, int64_t);
  Register address_register      = va_arg(args, Register);

  const char *mnemonic = instruction_mnemonic(context, inst);
  const char *address = register_name(address_register);

  switch (context->dialect) {
    case CG_ASM_DIALECT_ATT:
      fprintf(context->code, "    %s %" PRId64 "(%%%s)\n",
          mnemonic, offset, address);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprintf(context->code, "    %s [%s + %" PRId64 "]\n",
          mnemonic, address, offset);
      break;
    default: PANIC("Unsupported dialect %d", context->dialect);
  }
}

static void femit_reg(CodegenContext *context, enum Instructions inst, va_list args) {
  Register source_register   = va_arg(args, Register);

  const char *mnemonic = instruction_mnemonic(context, inst);
  const char *source = register_name(source_register);

  switch (context->dialect) {
    case CG_ASM_DIALECT_ATT:
      fprintf(context->code, "    %s %%%s\n",
          mnemonic, source);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprintf(context->code, "    %s %s\n",
          mnemonic, source);
      break;
    default: PANIC("Unsupported dialect %d", context->dialect);
  }
}

static void femit_imm(CodegenContext *context, enum Instructions inst, va_list args) {
  int64_t immediate = va_arg(args, int64_t);

  const char *mnemonic = instruction_mnemonic(context, inst);

  switch (context->dialect) {
    case CG_ASM_DIALECT_ATT:
      fprintf(context->code, "    %s $%" PRId64 "\n",
          mnemonic, immediate);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprintf(context->code, "    %s %" PRId64 "\n",
          mnemonic, immediate);
      break;
    default: PANIC("Unsupported dialect %d", context->dialect);
  }
}

static void femit_indirect_branch(CodegenContext *context, enum Instructions inst, va_list args) {
  Register address_register   = va_arg(args, Register);

  const char *mnemonic = instruction_mnemonic(context, inst);
  const char *address = register_name(address_register);

  switch (context->dialect) {
    case CG_ASM_DIALECT_ATT:
      fprintf(context->code, "    %s *%%%s\n",
          mnemonic, address);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprintf(context->code, "    %s %s\n",
          mnemonic, address);
      break;
    default: PANIC("Unsupported dialect %d", context->dialect);
  }
}

static void femit
(CodegenContext *context,
 enum Instructions instruction,
 ...)
{
  va_list args;
  va_start(args, instruction);

  ASSERT(context);
  STATIC_ASSERT(I_COUNT == 21, "femit() must exhaustively handle all x86_64 instructions.");

  switch (instruction) {
    case I_ADD:
    case I_SUB:
    case I_TEST:
    case I_XOR:
    case I_CMP:
    case I_MOV: {
      enum InstructionOperands operands = va_arg(args, enum InstructionOperands);
      switch (operands) {
        default: PANIC("Unhandled operand type %d in x86_64 code generation for %d.", operands, instruction);
        case IMMEDIATE_TO_REGISTER: femit_imm_to_reg(context, instruction, args); break;
        case IMMEDIATE_TO_MEMORY: femit_imm_to_mem(context, instruction, args); break;
        case MEMORY_TO_REGISTER: femit_mem_to_reg(context, instruction, args); break;
        case REGISTER_TO_MEMORY: femit_reg_to_mem(context, instruction, args); break;
        case REGISTER_TO_REGISTER: femit_reg_to_reg(context, instruction, args); break;
        case REGISTER_TO_NAME: femit_reg_to_name(context, instruction, args); break;
        case NAME_TO_REGISTER: femit_name_to_reg(context, instruction, args); break;
      }
    } break;

    case I_LEA: {
      enum InstructionOperands operands = va_arg(args, enum InstructionOperands);
      switch (operands) {
        default: PANIC("femit() only accepts MEMORY_TO_REGISTER or NAME_TO_REGISTER operand type with LEA instruction.");
        case MEMORY_TO_REGISTER: femit_mem_to_reg(context, instruction, args); break;
        case NAME_TO_REGISTER: femit_name_to_reg(context, instruction, args); break;
      }
    } break;

    case I_IMUL: {
      enum InstructionOperands operands = va_arg(args, enum InstructionOperands);
      switch (operands) {
        default: PANIC("femit() only accepts MEMORY_TO_REGISTER or REGISTER_TO_REGISTER operand type with IMUL instruction.");
        case MEMORY_TO_REGISTER: femit_mem_to_reg(context, instruction, args); break;
        case REGISTER_TO_REGISTER: femit_reg_to_reg(context, instruction, args); break;
      }
    } break;

    case I_IDIV: {
      enum InstructionOperands operand = va_arg(args, enum InstructionOperands);
      switch (operand) {
        default: PANIC("femit() only accepts MEMORY or REGISTER operand type with IDIV instruction.");
        case MEMORY: femit_mem(context, instruction, args); break;
        case REGISTER: femit_reg(context, instruction, args); break;
      }
    } break;

    case I_SAL:
    case I_SAR:
    case I_SHR: {
      enum InstructionOperands operand = va_arg(args, enum InstructionOperands);
      switch (operand) {
        default: PANIC("femit() only accepts REGISTER OR IMMEDIATE_TO_REGISTER operand type with shift instructions.");
        case IMMEDIATE_TO_REGISTER: femit_imm_to_reg(context, instruction, args); break;
        case REGISTER: {
          Register register_to_shift = va_arg(args, Register);
          const char *mnemonic = instruction_mnemonic(context, instruction);
          const char *cl = register_name_8(REG_RCX);

          switch (context->dialect) {
            case CG_ASM_DIALECT_ATT:
              fprintf(context->code, "    %s %%%s, %%%s\n",
                  mnemonic, cl, register_name(register_to_shift));
              break;
            case CG_ASM_DIALECT_INTEL:
              fprintf(context->code, "    %s %s, %s\n",
                  mnemonic, register_name(register_to_shift), cl);
              break;
            default: PANIC("Unsupported dialect %d for shift instruction", context->dialect);
          }
        } break;
      }
    } break;

    case I_JMP:
    case I_CALL: {
      enum InstructionOperands operand = va_arg(args, enum InstructionOperands);
      switch (operand) {
        default: PANIC("femit() only accepts REGISTER or NAME operand type with CALL/JMP instruction.");
        case REGISTER: femit_indirect_branch(context, instruction, args); break;
        case NAME: {
          char *label = va_arg(args, char *);
          const char *mnemonic = instruction_mnemonic(context, instruction);

          switch (context->dialect) {
            case CG_ASM_DIALECT_ATT:
            case CG_ASM_DIALECT_INTEL:
              fprintf(context->code, "    %s %s\n",
                  mnemonic, label);
              break;
            default: PANIC("Unsupported dialect %d for CALL/JMP instruction", context->dialect);
          }
        } break;
      }
    } break;

    case I_PUSH: {
      enum InstructionOperands operand = va_arg(args, enum InstructionOperands);
      switch (operand) {
        default: PANIC("femit() only accepts REGISTER, MEMORY, or IMMEDIATE operand type with PUSH instruction.");
        case REGISTER: femit_reg(context, instruction, args); break;
        case MEMORY: femit_mem(context, instruction, args); break;
        case IMMEDIATE: femit_imm(context, instruction, args); break;
      }
    } break;

    case I_POP: {
      enum InstructionOperands operand = va_arg(args, enum InstructionOperands);
      switch (operand) {
        default: PANIC("femit() only accepts REGISTER or MEMORY operand type with POP instruction.");
        case REGISTER: femit_reg(context, instruction, args); break;
        case MEMORY: femit_mem(context, instruction, args); break;
      }
    } break;

    case I_XCHG: {
      enum InstructionOperands operands = va_arg(args, enum InstructionOperands);
      switch (operands) {
        default: PANIC("Invalid operands for XCHG instruction: %d", operands);
        case REGISTER_TO_REGISTER: femit_reg_to_reg(context, instruction, args); break;
        case MEMORY_TO_REGISTER: femit_mem_to_reg(context, instruction, args); break;
      }
    } break;

    case I_SETCC: {
      enum ComparisonType comparison_type = va_arg(args, enum ComparisonType);
      Register value_register = va_arg(args, Register);

      const char *mnemonic = instruction_mnemonic(context, instruction);
      const char *value = register_name_8(value_register);

      switch (context->dialect) {
        case CG_ASM_DIALECT_ATT:
          fprintf(context->code, "    %s%s %%%s\n",
              mnemonic,
              comparison_suffixes[comparison_type], value);
          break;
        case CG_ASM_DIALECT_INTEL:
          fprintf(context->code, "    %s%s %s\n",
              mnemonic,
              comparison_suffixes[comparison_type], value);
          break;
        default: PANIC("Unsupported dialect %d", context->dialect);
      }
    } break;

    case I_JCC: {
      enum IndirectJumpType type = va_arg(args, enum IndirectJumpType);
      ASSERT(type < JUMP_TYPE_COUNT, "Invalid jump type %d", type);
      char *label = va_arg(args, char *);

      const char *mnemonic = instruction_mnemonic(context, I_JCC);

      switch (context->dialect) {
        case CG_ASM_DIALECT_ATT:
        case CG_ASM_DIALECT_INTEL:
          fprintf(context->code, "    %s%s %s\n",
              mnemonic, jump_type_names[type], label);
          break;
        default: PANIC("Unsupported dialect %d", context->dialect);
      }
    } break;

    case I_RET:
    case I_CQO: {
      const char *mnemonic = instruction_mnemonic(context, instruction);
      fprintf(context->code, "    %s\n", mnemonic);
    } break;

    default: PANIC("Unhandled instruction in x86_64 code generation: %d.", instruction);
  }

  va_end(args);
}

/// Load an immediate value into a new register.
static void load_immediate(CodegenContext *cg_context, Register reg, int64_t immediate) {
  femit(cg_context, I_MOV,
        IMMEDIATE_TO_REGISTER,
        immediate, reg);
}

/// Load the address of a global variable into a register
static void load_global_address
(CodegenContext *cg_context,
 Register reg,
 const char *name) {
  femit(cg_context, I_LEA, NAME_TO_REGISTER,
        REG_RIP, name,
        reg);
}

/// Load the address of a local variable into a register
static void load_local_address
(CodegenContext *cg_context,
 Register reg,
 int64_t offset)  {
  femit(cg_context, I_LEA, MEMORY_TO_REGISTER,
        REG_RBP, offset,
        reg);
}

/// Load the value of a global variable into a newly allocated register and return it.
static void load_global
(CodegenContext *cg_context,
 Register reg,
 const char *name) {
  femit(cg_context, I_MOV, NAME_TO_REGISTER,
        REG_RIP, name,
        reg);
}

/// Load the value of a local variable into a newly allocated register and return it.
static void load_local
(CodegenContext *cg_context,
 Register reg,
 int64_t offset)  {
  femit(cg_context, I_MOV, MEMORY_TO_REGISTER,
        REG_RBP, offset,
        reg);
}

/// Store a global variable.
static void store_global
(CodegenContext *cg_context,
 Register source,
 const char *name) {
  femit(cg_context, I_MOV, REGISTER_TO_NAME,
        source, REG_RIP,
        name);
}

/// Store a local variable.
static void store_local
(CodegenContext *cg_context,
 Register source,
 int64_t offset) {
  femit(cg_context, I_MOV, REGISTER_TO_MEMORY,
        source,
        REG_RBP, offset);
}

/// Store data in the memory pointed to by the given address.
static void store
(CodegenContext *cg_context,
 Register source,
 Register address) {
  femit(cg_context, I_MOV, REGISTER_TO_MEMORY,
        source,
        address, (int64_t)0);
}

/// Add an immediate value to a register.
static void add_immediate
(CodegenContext *cg_context,
 Register reg,
 int64_t immediate) {
  femit(cg_context, I_ADD, IMMEDIATE_TO_REGISTER,
        immediate, reg);
}

/// Branch to a label if a register is zero.
static void branch_if_zero
(CodegenContext *cg_context,
 Register reg,
 const char *label) {
  femit(cg_context, I_TEST, REGISTER_TO_REGISTER, reg, reg);
  femit(cg_context, I_JCC, JUMP_TYPE_Z, label);
}

/// Copy a register to another register.
static void copy_register
(CodegenContext *cg_context,
 Register src,
 Register dest) {
  femit(cg_context, I_MOV, REGISTER_TO_REGISTER, src, dest);
}

/// Generate a comparison between two registers.
static void comparison
(CodegenContext *cg_context,
 enum ComparisonType type,
 Register result,
 Register lhs,
 Register rhs) {
  ASSERT(type < COMPARE_COUNT, "Invalid comparison type");

  // Zero out result register.
  femit(cg_context, I_XOR, REGISTER_TO_REGISTER, result, result);

  // Perform the comparison.
  femit(cg_context, I_CMP, REGISTER_TO_REGISTER, rhs, lhs);
  femit(cg_context, I_SETCC, type, result);
}

/// Add two registers together.
static void add
(CodegenContext *cg_context,
 Register result,
 Register lhs,
 Register rhs) {
  // TODO: use LEA for a three-address add.
  // TODO: immediate values should be added directly rather than using a register.
  //       Also update interfering_regs() to reflect that.
  // TODO: Do the same for all arithmetic instructions.
  if (result == lhs) {
    femit(cg_context, I_ADD, REGISTER_TO_REGISTER, rhs, lhs);
  } else if (result == rhs) {
    femit(cg_context, I_ADD, REGISTER_TO_REGISTER, lhs, rhs);
  } else {
    femit(cg_context, I_MOV, REGISTER_TO_REGISTER, lhs, result);
    femit(cg_context, I_ADD, REGISTER_TO_REGISTER, rhs, result);
  }
}

/// Subtract rhs from lhs.
static void subtract
(CodegenContext *cg_context,
 Register result,
 Register lhs,
 Register rhs) {
  if (result == lhs) {
    femit(cg_context, I_SUB, REGISTER_TO_REGISTER, rhs, lhs);
  } else if (result == rhs) {
    femit(cg_context, I_XCHG, REGISTER_TO_REGISTER, lhs, rhs);
    femit(cg_context, I_SUB, REGISTER_TO_REGISTER, rhs, lhs);
  } else {
    femit(cg_context, I_MOV, REGISTER_TO_REGISTER, lhs, result);
    femit(cg_context, I_SUB, REGISTER_TO_REGISTER, rhs, result);
  }

}

/// Multiply two registers together.
static void multiply
(CodegenContext *cg_context,
 Register result,
 Register lhs,
 Register rhs) {
  if (result == lhs) {
    femit(cg_context, I_IMUL, REGISTER_TO_REGISTER, rhs, lhs);
  } else if (result == rhs) {
    femit(cg_context, I_IMUL, REGISTER_TO_REGISTER, lhs, rhs);
  } else {
    femit(cg_context, I_MOV, REGISTER_TO_REGISTER, lhs, result);
    femit(cg_context, I_IMUL, REGISTER_TO_REGISTER, rhs, result);
  }
}

/// Divide lhs by rhs. The RA already loads the dividend into RAX and stores
/// the result of the division/modulo operation in the right register.
static void divmod
(CodegenContext *cg_context,
 Register divisor) {
  // TODO: probably horribly broken.
  femit(cg_context, I_CQO);
  femit(cg_context, I_IDIV, REGISTER, divisor);
}

/// Shift lhs to the left by rhs.
static void shift_left
(CodegenContext *cg_context,
 Register result,
 Register value) {
  // TODO: shift by immediate should not use CL.
  //   This also needs to be changed in interfering_regs().
  if (result != value) { femit(cg_context, I_MOV, REGISTER_TO_REGISTER, value, result); }
  femit(cg_context, I_SAL, REGISTER, result);
}

/// Shift lhs to the right by rhs (arithmetic).
static void shift_right_arithmetic
(CodegenContext *cg_context,
 Register result,
 Register value) {
  if (result != value) { femit(cg_context, I_MOV, REGISTER_TO_REGISTER, value, result); }
  femit(cg_context, I_SAR, REGISTER, result);
}

/// Emit the function prologue.
static void function_prologue(CodegenContext *cg_context, Function *f) {
  fprintf(cg_context->code, "    endbr64\n");
  femit(cg_context, I_PUSH, REGISTER, REG_RBP);
  femit(cg_context, I_MOV, REGISTER_TO_REGISTER, REG_RSP, REG_RBP);
  femit(cg_context, I_SUB, IMMEDIATE_TO_REGISTER, f->locals_offset + cg_context->locals_base_offset, REG_RSP);
  // TODO: Align stack to 16 bytes.
}

/// Emit the function epilogue.
static void function_epilogue(CodegenContext *cg_context) {
  femit(cg_context, I_MOV, REGISTER_TO_REGISTER, REG_RBP, REG_RSP);
  femit(cg_context, I_POP, REGISTER, REG_RBP);
  femit(cg_context, I_RET);
}

static void emit_value(CodegenContext *context, Value *v);

// Save caller-saved registers and emit a call instruction. This is a horrible hack.
// This should only need to emit a call instruction once we have a register allocator
// that handles caller-saved registers.
// TODO: This currently only works with GAS because it involves GAS macros.
static void emit_call(CodegenContext *context, Value *call) {
  ASSERT(call->type == IR_INSTRUCTION_CALL, "Expected call instruction");

  fprintf(context->code, "    __call ");

  // Emit the call.
  if (call->call_value.type == FUNCTION_CALL_TYPE_INTERNAL) {
    if (context->dialect == CG_ASM_DIALECT_ATT) fprintf(context->code, "*%%");
    fprintf(context->code, "%s, ", register_name(call->call_value.callee->reg));
  } else {
    fprintf(context->code, "%s, ", call->call_value.external_callee);
  }

  if (context->dialect == CG_ASM_DIALECT_ATT) fprintf(context->code, "%%");
  fprintf(context->code, "%s\n", register_name(call->reg));
}

static void emit_value(CodegenContext *context, Value *value) {
  if (value->emitted) return;
  switch (value->type) {
    case IR_INSTRUCTION_ADD:
      add(context,
          value->reg,
          value->lhs->reg,
          value->rhs->reg);
      break;
    case IR_INSTRUCTION_SUB:
      subtract(context,
          value->reg,
          value->lhs->reg,
          value->rhs->reg);
      break;
    case IR_INSTRUCTION_MUL:
      multiply(context,
          value->reg,
          value->lhs->reg,
          value->rhs->reg);
      break;
    case IR_INSTRUCTION_DIV:
    case IR_INSTRUCTION_MOD:
      divmod(context, value->rhs->reg);
      break;
    case IR_INSTRUCTION_SHL:
      shift_left(context,
          value->reg,
          value->lhs->reg);
      break;
    case IR_INSTRUCTION_SAR:
      shift_right_arithmetic(context,
          value->reg,
          value->rhs->reg);
      break;
    case IR_INSTRUCTION_ALLOCA: break;
    case IR_INSTRUCTION_CALL:
      emit_call(context, value);
      break;
    case IR_INSTRUCTION_COMMENT:
      fprintf(context->code, "    ;;#; %s\n", value->comment_value);
      break;
    case IR_INSTRUCTION_COMPARISON:
      comparison(context,
          value->comparison.type,
          value->reg,
          value->comparison.lhs->reg,
          value->comparison.rhs->reg);
      break;
    case IR_INSTRUCTION_BRANCH: {
      char branch_buffer[64];
      snprintf(branch_buffer, sizeof branch_buffer, ".L%u", value->branch_target->id);
      femit(context, I_JMP, NAME, branch_buffer);
    } break;
    case IR_INSTRUCTION_BRANCH_IF: {
      char branch_buffer1[64], branch_buffer2[64];
      snprintf(branch_buffer1, sizeof branch_buffer1, ".L%u", value->cond_branch_value.true_branch->id);
      snprintf(branch_buffer2, sizeof branch_buffer2, ".L%u", value->cond_branch_value.false_branch->id);

      femit(context, I_TEST, REGISTER_TO_REGISTER,
          value->cond_branch_value.condition->reg,
          value->cond_branch_value.condition->reg);
      femit(context, I_JCC, JUMP_TYPE_NZ, branch_buffer1);
      femit(context, I_JMP, NAME, branch_buffer2);
    } break;
    case IR_INSTRUCTION_IMMEDIATE:
      load_immediate(context, value->reg, value->immediate);
      break;
    case IR_INSTRUCTION_PHI: break;
    case IR_INSTRUCTION_RETURN: break;
    case IR_INSTRUCTION_FUNCTION_REF: break;
    case IR_INSTRUCTION_GLOBAL_REF:
      load_global_address(context, value->reg, value->global_name);
      break;
    case IR_INSTRUCTION_GLOBAL_VAL:
      load_global(context, value->reg, value->global_name);
      break;
    case IR_INSTRUCTION_STORE_GLOBAL:
      store_global(context, value->global_store.value->reg, value->global_store.name);
      break;
    case IR_INSTRUCTION_LOCAL_REF:
      ASSERT(value->local_ref->type == IR_INSTRUCTION_ALLOCA);
      load_local_address(context, value->reg, value->local_ref->immediate);
      break;
    case IR_INSTRUCTION_LOCAL_VAL:
      ASSERT(value->local_ref->type == IR_INSTRUCTION_ALLOCA);
      load_local(context, value->reg, value->local_ref->immediate);
      break;
    case IR_INSTRUCTION_STORE_LOCAL:
      ASSERT(value->rhs->type == IR_INSTRUCTION_ALLOCA);
      store_local(context, value->lhs->reg, value->rhs->immediate);
      break;
    case IR_INSTRUCTION_STORE:
       store(context, value->lhs->reg, value->rhs->reg);
       break;
    case IR_INSTRUCTION_COPY:
      femit(context, I_MOV, REGISTER_TO_REGISTER,
          value->operand->reg,
          value->reg);
      break;

    case IR_INSTRUCTION_REGISTER: break;
    case IR_INSTRUCTION_PARAM_REF: ASSERT(0, "Param ref should have been lowered");
    case IR_INSTRUCTION_COUNT: break;
  }
}

static Value *create_register(Register reg) {
  Value *v = calloc(1, sizeof *v);
  v->type = IR_INSTRUCTION_REGISTER;
  v->reg = reg;
  return v;
}

static regmask_t interfering_regs(const CodegenContext *context, const Value *value){
  switch (value->type) {
    case IR_INSTRUCTION_DIV: return 1 << (REG_RDX - 1);
    case IR_INSTRUCTION_MOD: return 1 << (REG_RAX - 1);

    case IR_INSTRUCTION_SHL:
    case IR_INSTRUCTION_SAR:
      return 1 << (REG_RCX - 1);

    case IR_INSTRUCTION_CALL:
    switch (context->call_convention) {
      case CG_CALL_CONV_LINUX:
      case CG_CALL_CONV_MSWIN:
        return 1 << (REG_RAX - 1);
      case CG_CALL_CONV_COUNT: break;
    }
    PANIC("Unknown calling convention");
    default: return 0;
  }
}

/// Convert a param_ref to a register. Returns 0 if the argument is passed on the stack.
Register argument_register(CodegenContext *context, size_t index) {
  switch (context->call_convention) {
    case CG_CALL_CONV_MSWIN:
      return index < sizeof mswin_argument_registers / sizeof *mswin_argument_registers
        ? mswin_argument_registers[index]
        : 0;
    case CG_CALL_CONV_LINUX:
      return index < sizeof linux_argument_registers / sizeof *linux_argument_registers
        ? linux_argument_registers[index]
        : 0;
    default: PANIC("Unknown calling convention");
  }
}

static void lower_function(CodegenContext *context, Function *f) {
  (void) context;
  VALUE_FOREACH (val, bb, f) {
    switch (val->type) {
      default: break;
      case IR_INSTRUCTION_PARAM_REF: {
        Value *store = calloc(1, sizeof *store);
        store->type = IR_INSTRUCTION_STORE_LOCAL;
        Register r = argument_register(context, val->param_ref.index);
        if (r) {
          store->lhs = create_register(r);
          mark_used_by(store->lhs, store);
        } else {
          TODO("Handle stack arguments");
        }
        store->rhs = val;
        mark_used_by(store->rhs, store);
        insert_after(val, store);

        val->type = IR_INSTRUCTION_ALLOCA;
        val->immediate = 8;
      } break;
    }
  }
}

/// Add up all allocas.
static void sum_local_allocations(Function *f) {
  f->locals_offset = 8;
  VALUE_FOREACH_TYPE (val, bb, f, IR_INSTRUCTION_ALLOCA) {
    size_t immediate = val->immediate;
    val->immediate = f->locals_offset;
    f->locals_offset += immediate;
  }
}

static void get_argument_registers(RAInfo *info) {
  switch (info->context->call_convention) {
    case CG_CALL_CONV_LINUX: {
      info->arg_regs = linux_argument_registers;
      info->num_arg_regs = sizeof linux_argument_registers / sizeof *linux_argument_registers;
      return;
    }
    case CG_CALL_CONV_MSWIN: {
      info->arg_regs = mswin_argument_registers;
      info->num_arg_regs = sizeof mswin_argument_registers / sizeof *mswin_argument_registers;
      return;
    }
    case CG_CALL_CONV_COUNT: break;
  }
  PANIC("Unknown calling convention");
}

static void emit_function(CodegenContext *context, Function *f) {
  fprintf(context->code, "\n%s:\n", f->name);
  codegen_function_finalise(context, f);
  lower_function(context, f);
  sum_local_allocations(f);
  function_prologue(context, f);

  // TODO: Copy result of DIV/MOD

  // Copy the return value into %rax.
  if (f->return_value) {
    Value *copy = create_copy(context, f->return_value);
    copy->reg = REG_RAX;
    context->insert_point = f->return_block;
    insert_before(f->return_block->end, copy);
  }

  // Perform register allocation.
  RAInfo info = {0};
  info.context = context;
  info.function = f;
  info.num_regs = GENERAL_PURPOSE_REGISTER_COUNT;
  info.platform_interfering_regs = interfering_regs;
  get_argument_registers(&info);
  allocate_registers(&info);

  // Emit the rest of the function.
  LIST_FOREACH (block, f->entry) {
    fprintf(context->code, ".L%u:\n", block->id);
    LIST_FOREACH (value, block->values) {
      emit_value(context, value);
    }
  }

  function_epilogue(context);
}

void codegen_emit_x86_64(CodegenContext *context) {
  fprintf(context->code,
      "__return_value: .space 8\n\n"
      "%s"
      ".section .text\n"
      ".global main\n",
      context->dialect == CG_ASM_DIALECT_INTEL ? ".intel_syntax noprefix\n" : "");

  // Intrinsics.
  // TODO: This currently only works with GAS because it involves GAS macros.
  fprintf(context->code, "\n.macro __call func, result\n");
  for (Register r = FIRST_CALLER_SAVED_REGISTER; r <= LAST_CALLER_SAVED_REGISTER; r++) {
    femit(context, I_PUSH, REGISTER, (size_t) r);
  }
  fprintf(context->code, "    call \\func\n");
  store_global(context, REG_RAX, "__return_value");
  for (Register r = LAST_CALLER_SAVED_REGISTER; r >= FIRST_CALLER_SAVED_REGISTER; r--) {
    femit(context, I_POP, REGISTER, (size_t) r);
  }
  switch (context->dialect) {
    case CG_ASM_DIALECT_ATT:
      fprintf(context->code, "    mov __return_value(%%rip), \\result\n");
      break;
    case CG_ASM_DIALECT_INTEL:
      fprintf(context->code, "    mov \\result, [rip + __return_value]\n");
      break;
    default: PANIC("Unsupported dialect %d", context->dialect);
  }
  fprintf(context->code, ".endm\n");

  LIST_FOREACH (f, context->functions) { emit_function(context, f); }
}