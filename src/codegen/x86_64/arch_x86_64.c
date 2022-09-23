#include <codegen.h>
#include <codegen/x86_64/arch_x86_64.h>
#include <codegen/ir.h>
#include <codegen/ra.h>

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

typedef unsigned Register;

/// This is used for defining lookup tables etc. and
/// ensures that the registers are always in the correct
/// order
#ifndef _WIN64
#define FOR_ALL_REGISTERS(F)     \
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
#define FIRST_ARGUMENT_REGISTER REG_RDI
#define LAST_ARGUMENT_REGISTER REG_R9
#else
#define FOR_ALL_REGISTERS(F)     \
  /* Caller-saved */                    \
  F(RAX, "rax", "eax", "ax", "al")      \
  F(RCX, "rcx", "ecx", "cx", "cl")      \
  F(RDX, "rdx", "edx", "dx", "dl")      \
  F(R8,  "r8", "r8d", "r8w", "r8b")     \
  F(R9,  "r9", "r9d", "r9w", "r9b")     \
  F(RDI, "rdi", "edi", "di", "dil")     \
  F(RSI, "rsi", "esi", "si", "sil")     \
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
#define FIRST_ARGUMENT_REGISTER REG_RCX
#define LAST_ARGUMENT_REGISTER REG_R9
#endif

#define GENERAL_PURPOSE_REGISTER_COUNT (13)

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
    return register_names[descriptor];                                                          \
  }

enum Registers {
  REG_NONE,
  FOR_ALL_REGISTERS(DEFINE_REGISTER_ENUM)
  REG_COUNT
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


/// Creates a context for the CG_FMT_MSWIN architecture.
CodegenContext *codegen_context_x86_64_mswin_create(CodegenContext *parent) {
  CodegenContext *cg_ctx = calloc(1,sizeof(CodegenContext));

  // If this is the top level context, create the registers.
  // Otherwise, shallow copy register pool to child context.
  if (!parent) {
    // FIXME(Sirraide): These heap allocations are jank.
    cg_ctx->func_count = calloc(1, sizeof(size_t));
    cg_ctx->block_count = calloc(1, sizeof(size_t));
    cg_ctx->format = CG_FMT_x86_64_GAS;
    cg_ctx->call_convention = CG_CALL_CONV_MSWIN;
    cg_ctx->dialect = CG_ASM_DIALECT_ATT;
  } else {
    *cg_ctx = *parent;
  }

  cg_ctx->parent = parent;
  cg_ctx->locals = environment_create(NULL);
  cg_ctx->locals_offset = -32;
  return cg_ctx;
}

/// Free a context created by context_mswin_create.
void codegen_context_x86_64_mswin_free(CodegenContext *ctx) {
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
      fprintf(context->code, "%s $%" PRId64 ", %%%s\n",
          mnemonic, immediate, destination);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprintf(context->code, "%s %s, %" PRId64 "\n",
          mnemonic, destination, immediate);
      break;
    default: PANIC("Unsupported dialect %d", context->dialect);
  }
}

static void femit_imm_to_mem(CodegenContext *context, enum Instructions inst, va_list args) {
  int64_t immediate                    = va_arg(args, int64_t);
  Register address_register  = va_arg(args, Register);
  int64_t offset                       = va_arg(args, int64_t);

  const char *mnemonic = instruction_mnemonic(context, inst);
  const char *address = register_name(address_register);

  switch (context->dialect) {
    case CG_ASM_DIALECT_ATT:
      fprintf(context->code, "%s $%" PRId64 ", %" PRId64 "(%%%s)\n",
          mnemonic, immediate, offset, address);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprintf(context->code, "%s [%s + %" PRId64 "], %" PRId64 "\n",
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
      fprintf(context->code, "%s %" PRId64 "(%%%s), %%%s\n",
          mnemonic, offset, address, destination);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprintf(context->code, "%s %s, [%s + %" PRId64 "]\n",
          mnemonic, destination, address, offset);
      break;
    default: PANIC("Unsupported dialect %d", context->dialect);
  }
}

static void femit_name_to_reg(CodegenContext *context, enum Instructions inst, va_list args) {
  Register address_register      = va_arg(args, Register);
  char *name                               = va_arg(args, char *);
  Register destination_register  = va_arg(args, Register);

  const char *mnemonic = instruction_mnemonic(context, inst);
  const char *address = register_name(address_register);
  const char *destination = register_name(destination_register);

  switch (context->dialect) {
    case CG_ASM_DIALECT_ATT:
      fprintf(context->code, "%s %s(%%%s), %%%s\n",
          mnemonic, name, address, destination);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprintf(context->code, "%s %s, [%s + %s]\n",
          mnemonic, destination, address, name);
      break;
    default: PANIC("Unsupported dialect %d", context->dialect);
  }
}

static void femit_reg_to_mem(CodegenContext *context, enum Instructions inst, va_list args) {
  Register source_register   = va_arg(args, Register);
  Register address_register  = va_arg(args, Register);
  int64_t offset                       = va_arg(args, int64_t);

  const char *mnemonic = instruction_mnemonic(context, inst);
  const char *source = register_name(source_register);
  const char *address = register_name(address_register);

  switch (context->dialect) {
    case CG_ASM_DIALECT_ATT:
      if (offset) {
        fprintf(context->code, "%s %%%s, %" PRId64 "(%%%s)\n",
                mnemonic, source, offset, address);
      } else {
        fprintf(context->code, "%s %%%s, (%%%s)\n",
                mnemonic, source, address);
      }
      break;
    case CG_ASM_DIALECT_INTEL:
      if (offset) {
        fprintf(context->code, "%s [%s + %" PRId64 "], %s\n",
                mnemonic, address, offset, source);
      } else {
        fprintf(context->code, "%s [%s], %s\n",
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
      fprintf(context->code, "%s %%%s, %%%s\n",
          mnemonic, source, destination);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprintf(context->code, "%s %s, %s\n",
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
      fprintf(context->code, "%s %%%s, %s(%%%s)\n",
          mnemonic, source, name, address);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprintf(context->code, "%s [%s + %s], %s\n",
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
      fprintf(context->code, "%s %" PRId64 "(%%%s)\n",
          mnemonic, offset, address);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprintf(context->code, "%s [%s + %" PRId64 "]\n",
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
      fprintf(context->code, "%s %%%s\n",
          mnemonic, source);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprintf(context->code, "%s %s\n",
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
      fprintf(context->code, "%s $%" PRId64 "\n",
          mnemonic, immediate);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprintf(context->code, "%s %" PRId64 "\n",
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
      fprintf(context->code, "%s *%%%s\n",
          mnemonic, address);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprintf(context->code, "%s %s\n",
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
              fprintf(context->code, "%s %%%s, %%%s\n",
                  mnemonic, cl, register_name(register_to_shift));
              break;
            case CG_ASM_DIALECT_INTEL:
              fprintf(context->code, "%s %s, %s\n",
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
              fprintf(context->code, "%s %s\n",
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
          fprintf(context->code, "%s%s %%%s\n",
              mnemonic,
              comparison_suffixes[comparison_type], value);
          break;
        case CG_ASM_DIALECT_INTEL:
          fprintf(context->code, "%s%s %s\n",
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
          fprintf(context->code, "%s%s %s\n",
              mnemonic, jump_type_names[type], label);
          break;
        default: PANIC("Unsupported dialect %d", context->dialect);
      }
    } break;

    case I_RET:
    case I_CQO: {
      const char *mnemonic = instruction_mnemonic(context, instruction);
      fprintf(context->code, "%s\n", mnemonic);
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
  // TODO: Don't copy if lhs or rhs is the result;
  // TODO: use LEA for a three-address add.
  // TODO: immediate values should be added directly rather than using a register.
  //       Also update interfering_regs() to reflect that.
  // TODO: Do the same for all arithmetic instructions.
  if (result != lhs) { femit(cg_context, I_MOV, REGISTER_TO_REGISTER, lhs, result); }
  femit(cg_context, I_ADD, REGISTER_TO_REGISTER, rhs, result);
}

/// Subtract rhs from lhs.
static void subtract
(CodegenContext *cg_context,
 Register result,
 Register lhs,
 Register rhs) {
  if (result != lhs) { femit(cg_context, I_MOV, REGISTER_TO_REGISTER, lhs, result); }
  femit(cg_context, I_SUB, REGISTER_TO_REGISTER, rhs, result);
}

/// Multiply two registers together.
static void multiply
(CodegenContext *cg_context,
 Register result,
 Register lhs,
 Register rhs) {
  if (result != lhs) { femit(cg_context, I_MOV, REGISTER_TO_REGISTER, lhs, result); }
  femit(cg_context, I_IMUL, REGISTER_TO_REGISTER, rhs, result);
}

/// Divide lhs by rhs. The RA already loads the dividend into RAX and stores
/// the result of the division/modulo operation in the right register.
static void divmod
(CodegenContext *cg_context,
 Register divisor) {
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
  femit(cg_context, I_PUSH, REGISTER, REG_RBP);
  femit(cg_context, I_MOV, REGISTER_TO_REGISTER, REG_RSP, REG_RBP);
  femit(cg_context, I_SUB, IMMEDIATE_TO_REGISTER, f->locals_offset, REG_RSP);
  // TODO: Align stack to 16 bytes.
}

/// Emit the function epilogue.
static void function_epilogue(CodegenContext *cg_context) {
  femit(cg_context, I_MOV, REGISTER_TO_REGISTER, REG_RBP, REG_RSP);
  femit(cg_context, I_POP, REGISTER, REG_RBP);
  femit(cg_context, I_RET);
}

static void emit_value(CodegenContext *context, Value *v);

static void emit_call(CodegenContext *context, Value *call) {
  ASSERT(call->type == IR_INSTRUCTION_CALL, "Expected call instruction");
  // Emit the function reference if this is an internal call.
  if (call->call_value.type == FUNCTION_CALL_TYPE_INTERNAL) {
    emit_value(context, call->call_value.callee);
    femit(context, I_CALL, REGISTER, call->call_value.callee->reg);
  } else {
    femit(context, I_CALL, NAME, call->call_value.external_callee);
  }
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
      fprintf(context->code, ";;#; %s\n", value->comment_value);
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
      snprintf(branch_buffer, sizeof branch_buffer, ".L%zu", value->branch_target->id);
      femit(context, I_JMP, NAME, branch_buffer);
    } break;
    case IR_INSTRUCTION_BRANCH_IF: {
      char branch_buffer1[64], branch_buffer2[64];
      snprintf(branch_buffer1, sizeof branch_buffer1, ".L%zu", value->cond_branch_value.true_branch->id);
      snprintf(branch_buffer2, sizeof branch_buffer2, ".L%zu", value->cond_branch_value.false_branch->id);

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
    case IR_INSTRUCTION_RETURN:
      femit(context, I_RET);
      break;
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
      load_local_address(context, value->reg, value->local_ref->offset);
      break;
    case IR_INSTRUCTION_LOCAL_VAL:
      ASSERT(value->local_ref->type == IR_INSTRUCTION_ALLOCA);
      load_local(context, value->reg, value->local_ref->offset);
      break;
    case IR_INSTRUCTION_STORE_LOCAL:
      ASSERT(value->rhs->type == IR_INSTRUCTION_ALLOCA);
      store_local(context, value->lhs->reg, value->rhs->offset);
      break;
    case IR_INSTRUCTION_STORE:
       store(context, value->lhs->reg, value->rhs->reg);
       break;
    case IR_INSTRUCTION_PARAM_REF:
      if (value->param_ref.index + FIRST_ARGUMENT_REGISTER < FIRST_ARGUMENT_REGISTER ||
          value->param_ref.index + FIRST_ARGUMENT_REGISTER > LAST_ARGUMENT_REGISTER) {
        TODO("Handle arguments on the stack");
      } else {
        femit(context, I_MOV, REGISTER_TO_REGISTER,
            value->param_ref.index + FIRST_ARGUMENT_REGISTER,
            value->reg);
      }
      break;
    case IR_INSTRUCTION_COPY:
      femit(context, I_MOV, REGISTER_TO_REGISTER,
          value->operand->reg,
          value->reg);
      break;
    case IR_INSTRUCTION_COUNT: break;
  }
}

/*
static void insert_before(Value* value, Value *value_to_insert) {
  if (value->prev) { value->prev->next = value_to_insert; }
  value_to_insert->prev = value->prev;
  value_to_insert->next = value;
  value->prev = value_to_insert;
}

static void insert_after(Value *value, Value *value_to_insert) {
  if (value->next) { value->next->prev = value_to_insert; }
  value_to_insert->next = value->next;
  value_to_insert->prev = value;
  value->next = value_to_insert;
}
*/

static Value *create_copy(CodegenContext *context, Value *v) {
  (void) context;
  Value *copy = calloc(1, sizeof *copy);
  copy->type = IR_INSTRUCTION_COPY;
  copy->operand = v;
  return copy;
}

static regmask_t interfering_regs(const Value *value){
  switch (value->type) {
    case IR_INSTRUCTION_DIV: return 1 << (REG_RDX - 1);
    case IR_INSTRUCTION_MOD: return 1 << (REG_RAX - 1);

    case IR_INSTRUCTION_SHL:
    case IR_INSTRUCTION_SAR:
      return 1 << (REG_RCX - 1);

    case IR_INSTRUCTION_CALL:
      // TODO: Clobber caller-saved registers.
      return 0;

    default: return 0;
  }
}

static void emit_function(CodegenContext *context, Function *f) {
  fprintf(context->code, "%s:\n", f->name);
  context->locals_offset = 0;
  codegen_function_finalise(context, f);
  function_prologue(context, f);

  // TODO: Copy result of DIV/MOD

  // Copy the return value into %rax.
  if (f->return_value) {
    Value *copy = create_copy(context, f->return_value);
    copy->reg = REG_RAX;
    context->insert_point = f->return_block;
    insert(context, copy);
  }

  // Perform register allocation.
  // FIXME: pass all registers instead and put rbp, rsp, and rip at the very end
  //     of the registers enum.
  allocate_registers(context, f, GENERAL_PURPOSE_REGISTER_COUNT, interfering_regs);

  // Emit the rest of the function.
  for (BasicBlock *block = f->entry; block; block = block->next) {
    fprintf(context->code, ".L%zu:\n", block->id);
    for (Value *value = block->values; value; value = value->next) {
      emit_value(context, value);
    }
  }

  function_epilogue(context);
}

void codegen_emit_x86_64(CodegenContext *context) {
  fprintf(context->code,
      "%s"
      ".section .text\n"
      ".global main\n",
      context->dialect == CG_ASM_DIALECT_INTEL ? ".intel_syntax noprefix\n" : "");

  for (Function *f = context->functions; f; f = f->next) {
    emit_function(context, f);
  }
}