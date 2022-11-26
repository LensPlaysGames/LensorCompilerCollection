#include <codegen/x86_64/arch_x86_64.h>

#include <codegen.h>
#include <codegen/codegen_forward.h>
#include <codegen/intermediate_representation.h>
#include <codegen/register_allocation.h>
#include <error.h>
#include <inttypes.h>
#include <opt.h>
#include <parser.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <typechecker.h>

#define DEFINE_REGISTER_ENUM(name, ...) REG_##name,
#define REGISTER_NAME_64(ident, name, ...) name,
#define REGISTER_NAME_32(ident, name, name_32, ...) name_32,
#define REGISTER_NAME_16(ident, name, name_32, name_16, ...) name_16,
#define REGISTER_NAME_8(ident, name, name_32, name_16, name_8, ...) name_8,

/// Lookup tables for register names.
#define DEFINE_REGISTER_NAME_LOOKUP_FUNCTION(name, bits)                \
  static const char *name(RegisterDescriptor descriptor) {              \
    static const char* register_names[] =                               \
      { FOR_ALL_X86_64_REGISTERS(REGISTER_NAME_##bits) };               \
    if (descriptor <= 0 || descriptor > REG_COUNT) {                    \
      panic("ERROR::" #name "(): Could not find register with descriptor of %d\n", descriptor); \
    }                                                                   \
    return register_names[descriptor - 1];                              \
  }

enum Registers_x86_64 {
  REG_NONE,
  FOR_ALL_X86_64_REGISTERS(DEFINE_REGISTER_ENUM)
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

// TODO: This should probably be 13?
#define GENERAL_REGISTER_COUNT 14


static Register *caller_saved_registers = NULL;
static size_t caller_saved_register_count = 0;

char is_caller_saved(Register r) {
  for (int i = 0; i < caller_saved_register_count; ++i) {
    if (caller_saved_registers[i] == r) {
      return 1;
    }
  }
  return 0;
}

char is_callee_saved(Register r) {
  return !is_caller_saved(r);
}


/// Types of conditional jump instructions (Jcc).
/// Do NOT reorder these.
enum IndirectJumpType_x86_64 {
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
static const char *jump_type_names_x86_64[] = {
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
enum Instructions_x86_64 {
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

enum InstructionOperands_x86_64 {
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

const char *comparison_suffixes_x86_64[COMPARE_COUNT] = {
    "e",
    "ne",
    "l",
    "le",
    "g",
    "ge",
};

static const char *instruction_mnemonic_x86_64(CodegenContext *context, enum Instructions_x86_64 instruction) {
  ASSERT(I_COUNT == 21, "ERROR: instruction_mnemonic_x86_64() must exhaustively handle all instructions.");
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
    default: panic("instruction_mnemonic_x86_64(): Unknown output format.");

    case CG_ASM_DIALECT_ATT:
    switch (instruction) {
      default: panic("instruction_mnemonic_x86_64(): Unknown instruction.");
      case I_CQO: return "cqto";
    }

    case CG_ASM_DIALECT_INTEL:
    switch (instruction) {
      default: panic("instruction_mnemonic_x86_64(): Unknown instruction.");
      case I_CQO: return "cqo";
    }
  }
}

static void femit_x86_64_imm_to_reg(CodegenContext *context, enum Instructions_x86_64 inst, va_list args) {
  int64_t immediate                        = va_arg(args, int64_t);
  RegisterDescriptor destination_register  = va_arg(args, RegisterDescriptor);

  const char *mnemonic    = instruction_mnemonic_x86_64(context, inst);
  const char *destination = register_name(destination_register);

  switch (context->dialect) {
    case CG_ASM_DIALECT_ATT:
      fprintf(context->code, "%s $%" PRId64 ", %%%s\n",
          mnemonic, immediate, destination);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprintf(context->code, "%s %s, %" PRId64 "\n",
          mnemonic, destination, immediate);
      break;
    default: panic("ERROR: femit_x86_64_imm_to_reg(): Unsupported dialect %d", context->dialect);
  }
}

static void femit_x86_64_imm_to_mem(CodegenContext *context, enum Instructions_x86_64 inst, va_list args) {
  int64_t immediate                    = va_arg(args, int64_t);
  RegisterDescriptor address_register  = va_arg(args, RegisterDescriptor);
  int64_t offset                       = va_arg(args, int64_t);

  const char *mnemonic = instruction_mnemonic_x86_64(context, inst);
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
    default: panic("ERROR: femit_x86_64_imm_to_mem(): Unsupported dialect %d", context->dialect);
  }
}

static void femit_x86_64_mem_to_reg(CodegenContext *context, enum Instructions_x86_64 inst, va_list args) {
  RegisterDescriptor address_register      = va_arg(args, RegisterDescriptor);
  int64_t offset                           = va_arg(args, int64_t);
  RegisterDescriptor destination_register  = va_arg(args, RegisterDescriptor);

  const char *mnemonic = instruction_mnemonic_x86_64(context, inst);
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
    default: panic("ERROR: femit_x86_64_mem_to_reg(): Unsupported dialect %d", context->dialect);
  }
}

static void femit_x86_64_name_to_reg(CodegenContext *context, enum Instructions_x86_64 inst, va_list args) {
  RegisterDescriptor address_register      = va_arg(args, RegisterDescriptor);
  char *name                               = va_arg(args, char *);
  RegisterDescriptor destination_register  = va_arg(args, RegisterDescriptor);

  const char *mnemonic = instruction_mnemonic_x86_64(context, inst);
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
    default: panic("ERROR: femit_x86_64_name_to_reg(): Unsupported dialect %d", context->dialect);
  }
}

static void femit_x86_64_reg_to_mem(CodegenContext *context, enum Instructions_x86_64 inst, va_list args) {
  RegisterDescriptor source_register   = va_arg(args, RegisterDescriptor);
  RegisterDescriptor address_register  = va_arg(args, RegisterDescriptor);
  int64_t offset                       = va_arg(args, int64_t);

  const char *mnemonic = instruction_mnemonic_x86_64(context, inst);
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
    default: panic("ERROR: femit_x86_64_reg_to_mem(): Unsupported dialect %d", context->dialect);
  }
}

static void femit_x86_64_reg_to_reg(CodegenContext *context, enum Instructions_x86_64 inst, va_list args) {
  RegisterDescriptor source_register       = va_arg(args, RegisterDescriptor);
  RegisterDescriptor destination_register  = va_arg(args, RegisterDescriptor);

  const char *mnemonic = instruction_mnemonic_x86_64(context, inst);
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
    default: panic("ERROR: femit_x86_64_reg_to_reg(): Unsupported dialect %d", context->dialect);
  }
}

static void femit_x86_64_reg_to_name(CodegenContext *context, enum Instructions_x86_64 inst, va_list args) {
  RegisterDescriptor source_register  = va_arg(args, RegisterDescriptor);
  RegisterDescriptor address_register      = va_arg(args, RegisterDescriptor);
  char *name                               = va_arg(args, char *);

  const char *mnemonic = instruction_mnemonic_x86_64(context, inst);
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
    default: panic("ERROR: femit_x86_64_reg_to_name(): Unsupported dialect %d", context->dialect);
  }
}

static void femit_x86_64_mem(CodegenContext *context, enum Instructions_x86_64 inst, va_list args) {
  int64_t offset                           = va_arg(args, int64_t);
  RegisterDescriptor address_register      = va_arg(args, RegisterDescriptor);

  const char *mnemonic = instruction_mnemonic_x86_64(context, inst);
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
    default: panic("ERROR: femit_x86_64_mem(): Unsupported dialect %d", context->dialect);
  }
}

static void femit_x86_64_reg(CodegenContext *context, enum Instructions_x86_64 inst, va_list args) {
  RegisterDescriptor source_register   = va_arg(args, RegisterDescriptor);

  const char *mnemonic = instruction_mnemonic_x86_64(context, inst);
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
    default: panic("ERROR: femit_x86_64_reg(): Unsupported dialect %d", context->dialect);
  }
}

static void femit_x86_64_imm(CodegenContext *context, enum Instructions_x86_64 inst, va_list args) {
  int64_t immediate = va_arg(args, int64_t);

  const char *mnemonic = instruction_mnemonic_x86_64(context, inst);

  switch (context->dialect) {
    case CG_ASM_DIALECT_ATT:
      fprintf(context->code, "%s $%" PRId64 "\n",
          mnemonic, immediate);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprintf(context->code, "%s %" PRId64 "\n",
          mnemonic, immediate);
      break;
    default: panic("ERROR: femit_x86_64_imm(): Unsupported dialect %d", context->dialect);
  }
}

static void femit_x86_64_indirect_branch(CodegenContext *context, enum Instructions_x86_64 inst, va_list args) {
  RegisterDescriptor address_register   = va_arg(args, RegisterDescriptor);

  const char *mnemonic = instruction_mnemonic_x86_64(context, inst);
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
    default: panic("ERROR: femit_x86_64_indirect_branch(): Unsupported dialect %d", context->dialect);
  }
}

static void femit_x86_64
(CodegenContext *context,
 enum Instructions_x86_64 instruction,
 ...)
{
  va_list args;
  va_start(args, instruction);

  ASSERT(context);
  ASSERT(I_COUNT == 21, "femit_x86_64() must exhaustively handle all x86_64 instructions.");

  switch (instruction) {
    case I_ADD:
    case I_SUB:
    case I_TEST:
    case I_XOR:
    case I_CMP:
    case I_MOV: {
      enum InstructionOperands_x86_64 operands = va_arg(args, enum InstructionOperands_x86_64);
      switch (operands) {
        default: panic("Unhandled operand type %d in x86_64 code generation for %d.", operands, instruction);
        case IMMEDIATE_TO_REGISTER: femit_x86_64_imm_to_reg(context, instruction, args); break;
        case IMMEDIATE_TO_MEMORY: femit_x86_64_imm_to_mem(context, instruction, args); break;
        case MEMORY_TO_REGISTER: femit_x86_64_mem_to_reg(context, instruction, args); break;
        case REGISTER_TO_MEMORY: femit_x86_64_reg_to_mem(context, instruction, args); break;
        case REGISTER_TO_REGISTER: femit_x86_64_reg_to_reg(context, instruction, args); break;
        case REGISTER_TO_NAME: femit_x86_64_reg_to_name(context, instruction, args); break;
        case NAME_TO_REGISTER: femit_x86_64_name_to_reg(context, instruction, args); break;
      }
    } break;

    case I_LEA: {
      enum InstructionOperands_x86_64 operands = va_arg(args, enum InstructionOperands_x86_64);
      switch (operands) {
        default: panic("femit_x86_64() only accepts MEMORY_TO_REGISTER or NAME_TO_REGISTER operand type with LEA instruction.");
        case MEMORY_TO_REGISTER: femit_x86_64_mem_to_reg(context, instruction, args); break;
        case NAME_TO_REGISTER: femit_x86_64_name_to_reg(context, instruction, args); break;
      }
    } break;

    case I_IMUL: {
      enum InstructionOperands_x86_64 operands = va_arg(args, enum InstructionOperands_x86_64);
      switch (operands) {
        default: panic("femit_x86_64() only accepts MEMORY_TO_REGISTER or REGISTER_TO_REGISTER operand type with IMUL instruction.");
        case MEMORY_TO_REGISTER: femit_x86_64_mem_to_reg(context, instruction, args); break;
        case REGISTER_TO_REGISTER: femit_x86_64_reg_to_reg(context, instruction, args); break;
      }
    } break;

    case I_IDIV: {
      enum InstructionOperands_x86_64 operand = va_arg(args, enum InstructionOperands_x86_64);
      switch (operand) {
        default: panic("femit_x86_64() only accepts MEMORY or REGISTER operand type with IDIV instruction.");
        case MEMORY: femit_x86_64_mem(context, instruction, args); break;
        case REGISTER: femit_x86_64_reg(context, instruction, args); break;
      }
    } break;

    case I_SAL:
    case I_SAR:
    case I_SHR: {
      enum InstructionOperands_x86_64 operand = va_arg(args, enum InstructionOperands_x86_64);
      switch (operand) {
        default: panic("femit_x86_64() only accepts REGISTER OR IMMEDIATE_TO_REGISTER operand type with shift instructions.");
        case IMMEDIATE_TO_REGISTER: femit_x86_64_imm_to_reg(context, instruction, args); break;
        case REGISTER: {
          RegisterDescriptor register_to_shift = va_arg(args, RegisterDescriptor);
          const char *mnemonic = instruction_mnemonic_x86_64(context, instruction);
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
            default: panic("ERROR: femit_x86_64(): Unsupported dialect %d for shift instruction", context->dialect);
          }
        } break;
      }
    } break;

    case I_JMP:
    case I_CALL: {
      enum InstructionOperands_x86_64 operand = va_arg(args, enum InstructionOperands_x86_64);
      switch (operand) {
        default: panic("femit_x86_64() only accepts REGISTER or NAME operand type with CALL/JMP instruction.");
        case REGISTER: femit_x86_64_indirect_branch(context, instruction, args); break;
        case NAME: {
          char *label = va_arg(args, char *);
          const char *mnemonic = instruction_mnemonic_x86_64(context, instruction);

          switch (context->dialect) {
            case CG_ASM_DIALECT_ATT:
            case CG_ASM_DIALECT_INTEL:
              fprintf(context->code, "%s %s\n",
                  mnemonic, label);
              break;
            default: panic("ERROR: femit_x86_64(): Unsupported dialect %d for CALL/JMP instruction", context->dialect);
          }
        } break;
      }
    } break;

    case I_PUSH: {
      enum InstructionOperands_x86_64 operand = va_arg(args, enum InstructionOperands_x86_64);
      switch (operand) {
        default: panic("femit_x86_64() only accepts REGISTER, MEMORY, or IMMEDIATE operand type with PUSH instruction.");
        case REGISTER: femit_x86_64_reg(context, instruction, args); break;
        case MEMORY: femit_x86_64_mem(context, instruction, args); break;
        case IMMEDIATE: femit_x86_64_imm(context, instruction, args); break;
      }
    } break;

    case I_POP: {
      enum InstructionOperands_x86_64 operand = va_arg(args, enum InstructionOperands_x86_64);
      switch (operand) {
        default: panic("femit_x86_64() only accepts REGISTER or MEMORY operand type with POP instruction.");
        case REGISTER: femit_x86_64_reg(context, instruction, args); break;
        case MEMORY: femit_x86_64_mem(context, instruction, args); break;
      }
    } break;

    case I_XCHG: {
      enum InstructionOperands_x86_64 operands = va_arg(args, enum InstructionOperands_x86_64);
      switch (operands) {
        default: panic("femit_x86_64(): invalid operands for XCHG instruction: %d", operands);
        case REGISTER_TO_REGISTER: femit_x86_64_reg_to_reg(context, instruction, args); break;
        case MEMORY_TO_REGISTER: femit_x86_64_mem_to_reg(context, instruction, args); break;
      }
    } break;

    case I_SETCC: {
      enum ComparisonType comparison_type = va_arg(args, enum ComparisonType);
      RegisterDescriptor value_register = va_arg(args, RegisterDescriptor);

      const char *mnemonic = instruction_mnemonic_x86_64(context, instruction);
      const char *value = register_name_8(value_register);

      switch (context->dialect) {
        case CG_ASM_DIALECT_ATT:
          fprintf(context->code, "%s%s %%%s\n",
              mnemonic,
              comparison_suffixes_x86_64[comparison_type], value);
          break;
        case CG_ASM_DIALECT_INTEL:
          fprintf(context->code, "%s%s %s\n",
              mnemonic,
              comparison_suffixes_x86_64[comparison_type], value);
          break;
        default: panic("ERROR: femit_x86_64(): Unsupported dialect %d", context->dialect);
      }
    } break;

    case I_JCC: {
      enum IndirectJumpType_x86_64 type = va_arg(args, enum IndirectJumpType_x86_64);
      ASSERT(type < JUMP_TYPE_COUNT, "femit_x86_64_direct_branch(): Invalid jump type %d", type);
      char *label = va_arg(args, char *);

      const char *mnemonic = instruction_mnemonic_x86_64(context, I_JCC);

      switch (context->dialect) {
        case CG_ASM_DIALECT_ATT:
        case CG_ASM_DIALECT_INTEL:
          fprintf(context->code, "%s%s %s\n",
              mnemonic, jump_type_names_x86_64[type], label);
          break;
        default: panic("ERROR: femit_x86_64_direct_branch(): Unsupported dialect %d", context->dialect);
      }
    } break;

    case I_RET:
    case I_CQO: {
      const char *mnemonic = instruction_mnemonic_x86_64(context, instruction);
      fprintf(context->code, "%s\n", mnemonic);
    } break;

    default: panic("Unhandled instruction in x86_64 code generation: %d.", instruction);
  }

  va_end(args);
}

/// X86_64-specific code generation state.
typedef struct StackFrame {
  /// The type of function call that is currently being emitted.
  enum {
    FUNCTION_CALL_TYPE_NONE,
    FUNCTION_CALL_TYPE_INTERNAL,
    FUNCTION_CALL_TYPE_EXTERNAL,
  } call_type;
  /// The number of arguments emitted.
  size_t call_arg_count;
  char rax_in_use;
  char call_performed;
  struct StackFrame* parent;
} StackFrame;

typedef struct ArchData {
  StackFrame *current_call;
} ArchData;

/// Creates a context for the CG_FMT_x86_64_MSWIN architecture.
CodegenContext *codegen_context_x86_64_mswin_create(CodegenContext *parent) {
  RegisterPool pool;

  // If this is the top level context, create the registers.
  // Otherwise, shallow copy register pool to child context.
  if (!parent) {
    Register *registers = calloc(REG_COUNT, sizeof(Register));

    // Link to MSDN documentation (surely will fall away, but it's been Internet Archive'd).
    // https://docs.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-170#callercallee-saved-registers
    // https://web.archive.org/web/20220916164241/https://docs.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-170
    // "The x64 ABI considers the registers RAX, RCX, RDX, R8, R9, R10, R11, and XMM0-XMM5 volatile."
    // "The x64 ABI considers registers RBX, RBP, RDI, RSI, RSP, R12, R13, R14, R15, and XMM6-XMM15 nonvolatile."
    size_t number_of_scratch_registers = 7;
    Register **scratch_registers = calloc(number_of_scratch_registers, sizeof(Register *));
    scratch_registers[0] = registers + REG_RAX;
    scratch_registers[1] = registers + REG_RCX;
    scratch_registers[2] = registers + REG_RDX;
    scratch_registers[3] = registers + REG_R8;
    scratch_registers[4] = registers + REG_R9;
    scratch_registers[5] = registers + REG_R10;
    scratch_registers[6] = registers + REG_R11;

    pool.registers = registers;
    pool.scratch_registers = scratch_registers;
    pool.num_scratch_registers = number_of_scratch_registers;
    pool.num_registers = REG_COUNT;
  } else {
    pool = parent->register_pool;
  }

  CodegenContext *cg_ctx = calloc(1,sizeof(CodegenContext));

  // Shallow-copy state from the parent.
  if (parent) {
    cg_ctx->code = parent->code;
    cg_ctx->arch_data = parent->arch_data;
    cg_ctx->format = parent->format;
    cg_ctx->call_convention = parent->call_convention;
    cg_ctx->dialect = parent->dialect;
  } else {
    cg_ctx->arch_data = calloc(1, sizeof(ArchData));
    cg_ctx->format = CG_FMT_x86_64_GAS;
    cg_ctx->call_convention = CG_CALL_CONV_MSWIN;
    cg_ctx->dialect = CG_ASM_DIALECT_ATT;
  }

  cg_ctx->parent = parent;
  cg_ctx->locals = environment_create(NULL);
  cg_ctx->locals_offset = -32;
  cg_ctx->register_pool = pool;
  return cg_ctx;
}

/// Free a context created by codegen_context_x86_64_mswin_create.
void codegen_context_x86_64_mswin_free(CodegenContext *ctx) {
  // Only free the registers and arch data if this is the top-level context.
  if (!ctx->parent) {
    free(ctx->register_pool.registers);
    free(ctx->register_pool.scratch_registers);
    free(ctx->arch_data);
  }
  // TODO(sirraide): Free environment.
  free(ctx);
}

/// Save state before a function call.
void codegen_prepare_call_x86_64(CodegenContext *cg_context) {
  ArchData *arch_data = cg_context->arch_data;

  // Create a new stack frame and push it onto the call stack.
  StackFrame *parent = arch_data->current_call;
  StackFrame *frame = calloc(1, sizeof(StackFrame));
  frame->parent = parent;
  arch_data->current_call = frame;

  //arch_data->current_call->rax_in_use = cg_context->register_pool.registers[REG_RAX].in_use;
  if (arch_data->current_call->rax_in_use) femit_x86_64(cg_context, I_PUSH, REGISTER, REG_RAX);
}

/// Clean up after a function call.
void codegen_cleanup_call_x86_64(CodegenContext *cg_context) {
  ArchData *arch_data = cg_context->arch_data;
  ASSERT(arch_data->current_call, "Cannot clean up call if there is no call in progress.");
  ASSERT(arch_data->current_call->call_performed, "Cannot clean up call that hasn't been performed yet.");

  // Clean up stack from function call. This is only needed if
  // arguments were passed on the stack.
  switch (arch_data->current_call->call_type) {
    case FUNCTION_CALL_TYPE_INTERNAL:
      femit_x86_64(cg_context, I_ADD, IMMEDIATE_TO_REGISTER,
                   (int64_t)(arch_data->current_call->call_arg_count * 8), REG_RSP);
      break;
    case FUNCTION_CALL_TYPE_EXTERNAL:
      break;
    default: panic("No call to clean up");
  }

  // Restore rax if it was in use, because function return value clobbered it.
  if (arch_data->current_call->rax_in_use) {
    femit_x86_64(cg_context, I_POP, REGISTER, REG_RAX);
  }

  // Clean up the call state.
  StackFrame *parent = arch_data->current_call->parent;
  free(arch_data->current_call);
  arch_data->current_call = parent;
}

/// Load the address of a global variable into a newly allocated register and return it.
void codegen_load_global_address_into_x86_64
(CodegenContext *cg_context,
 const char *name,
 RegisterDescriptor target) {
  femit_x86_64(cg_context, I_LEA, NAME_TO_REGISTER,
      REG_RIP, name,
      target);
}

/// Load the address of a local variable into a newly allocated register and return it.
void codegen_load_local_address_into_x86_64
(CodegenContext *cg_context,
 long long int offset,
 RegisterDescriptor target)  {
  femit_x86_64(cg_context, I_LEA, MEMORY_TO_REGISTER,
               REG_RBP, offset,
               target);
}

/// Load the value of a global variable into a newly allocated register and return it.
void codegen_load_global_into_x86_64
(CodegenContext *cg_context,
 const char *name,
 RegisterDescriptor target) {
  femit_x86_64(cg_context, I_MOV, NAME_TO_REGISTER,
      REG_RIP, name,
      target);
}

/// Load the value of a local variable into a newly allocated register and return it.
void codegen_load_local_into_x86_64(CodegenContext *cg_context,
 long long int offset,
 RegisterDescriptor target)  {
  femit_x86_64(cg_context, I_MOV, MEMORY_TO_REGISTER,
      REG_RBP, offset,
      target);
}

/// Store a global variable.
void codegen_store_global_x86_64
(CodegenContext *cg_context,
 RegisterDescriptor source,
 const char *name) {
  femit_x86_64(cg_context, I_MOV, REGISTER_TO_NAME,
               source, REG_RIP, name);
}

/// Store a local variable.
void codegen_store_local_x86_64
(CodegenContext *cg_context,
 RegisterDescriptor source,
 long long int offset) {
  femit_x86_64(cg_context, I_MOV, REGISTER_TO_MEMORY,
               source, REG_RBP, offset);
}

/// Store data in the memory pointed to by the given address.
void codegen_store_x86_64
(CodegenContext *cg_context,
 RegisterDescriptor source,
 RegisterDescriptor address) {
  femit_x86_64(cg_context, I_MOV, REGISTER_TO_MEMORY, source, address, (int64_t)0);
}

/// Add an immediate value to a register.
void codegen_add_immediate_x86_64
(CodegenContext *cg_context,
 RegisterDescriptor reg,
 long long int immediate) {
  femit_x86_64(cg_context, I_ADD, IMMEDIATE_TO_REGISTER,
               immediate, reg);
}

/// Branch to a label if a register is zero.
void codegen_branch_if_zero_x86_64
(CodegenContext *cg_context,
 RegisterDescriptor reg,
 const char *label) {
  femit_x86_64(cg_context, I_TEST, REGISTER_TO_REGISTER, reg, reg);
  femit_x86_64(cg_context, I_JCC, JUMP_TYPE_Z, label);
}

/// Branch to a label.
void codegen_branch_x86_64
(CodegenContext *cg_context,
 const char *label) {
  femit_x86_64(cg_context, I_JMP, NAME, label);
}

/// Copy a register to another register.
void codegen_copy_register_x86_64
(CodegenContext *cg_context,
 RegisterDescriptor src,
 RegisterDescriptor dest) {
  femit_x86_64(cg_context, I_MOV, REGISTER_TO_REGISTER, src, dest);
}

/// Zero out a register.
void codegen_zero_register_x86_64
(CodegenContext *cg_context,
 RegisterDescriptor reg) {
  femit_x86_64(cg_context, I_XOR, REGISTER_TO_REGISTER, reg, reg);
}

/// Generate a comparison between two registers.
RegisterDescriptor codegen_comparison_x86_64
(CodegenContext *cg_context,
 enum ComparisonType type,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs,
 RegisterDescriptor result
 ) {
  ASSERT(type < COMPARE_COUNT, "Invalid comparison type");

  // Zero out result register.
  femit_x86_64(cg_context, I_XOR, REGISTER_TO_REGISTER, result, result);

  // Perform the comparison.
  femit_x86_64(cg_context, I_CMP, REGISTER_TO_REGISTER, rhs, lhs);
  femit_x86_64(cg_context, I_SETCC, type, result);

  return result;
}

/// Add two registers together.
RegisterDescriptor codegen_add_x86_64
(CodegenContext *cg_context,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs) {
  femit_x86_64(cg_context, I_ADD, REGISTER_TO_REGISTER, lhs, rhs);
  //register_deallocate(cg_context, lhs);
  return rhs;
}

/// Subtract rhs from lhs.
RegisterDescriptor codegen_subtract_x86_64
(CodegenContext *cg_context,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs) {
  femit_x86_64(cg_context, I_SUB, REGISTER_TO_REGISTER, rhs, lhs);
  //register_deallocate(cg_context, rhs);
  return lhs;
}

/// Multiply two registers together.
RegisterDescriptor codegen_multiply_x86_64
(CodegenContext *cg_context,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs) {
  femit_x86_64(cg_context, I_IMUL, REGISTER_TO_REGISTER, lhs, rhs);
  //register_deallocate(cg_context, lhs);
  return rhs;
}

/// Allocate space on the stack.
void codegen_stack_allocate_x86_64(CodegenContext *cg_context, long long int size) {
  femit_x86_64(cg_context, I_SUB, IMMEDIATE_TO_REGISTER, size, REG_RSP);
}

/// Emit the function prologue.
void codegen_prologue_x86_64(CodegenContext *cg_context, int64_t locals_offset) {
  if (optimise && !locals_offset) return;
  femit_x86_64(cg_context, I_PUSH, REGISTER, REG_RBP);
  femit_x86_64(cg_context, I_MOV, REGISTER_TO_REGISTER, REG_RSP, REG_RBP);
  femit_x86_64(cg_context, I_SUB, IMMEDIATE_TO_REGISTER, locals_offset, REG_RSP);
}

/// Emit the function epilogue.
void codegen_epilogue_x86_64(CodegenContext *cg_context, IRFunction *f) {
  if (!optimise || f->locals_total_size) {
    femit_x86_64(cg_context, I_MOV, REGISTER_TO_REGISTER, REG_RBP, REG_RSP);
    femit_x86_64(cg_context, I_POP, REGISTER, REG_RBP);
  }
  femit_x86_64(cg_context, I_RET);
}

/// Set the return value of a function.
void codegen_set_return_value_x86_64(CodegenContext *cg_context, RegisterDescriptor value) {
  femit_x86_64(cg_context, I_MOV, REGISTER_TO_REGISTER, value, REG_RAX);
}

void emit_instruction(CodegenContext *context, IRInstruction *instruction) {
  switch (instruction->type) {
  case IR_PHI:
  case IR_STACK_ALLOCATE:
  case IR_REGISTER:
    break;
  case IR_IMMEDIATE:
    femit_x86_64(context, I_MOV, IMMEDIATE_TO_REGISTER,
                 instruction->value.immediate,
                 instruction->result);
    break;
  case IR_COPY:
    femit_x86_64(context, I_MOV, REGISTER_TO_REGISTER,
                 instruction->value.reference->result,
                 instruction->result);
    break;
  case IR_GLOBAL_ADDRESS:
    femit_x86_64(context, I_LEA, NAME_TO_REGISTER,
                 REG_RIP, instruction->value.name,
                 instruction->result);
    break;
  case IR_GLOBAL_STORE:
    femit_x86_64(context, I_MOV, REGISTER_TO_NAME,
                 instruction->value.global_assignment.new_value->result,
                 REG_RIP,
                 instruction->value.global_assignment.name);
    break;
  case IR_GLOBAL_LOAD:
    femit_x86_64(context, I_MOV, NAME_TO_REGISTER,
                 REG_RIP,
                 instruction->value.name,
                 instruction->result);
    break;
  case IR_LOCAL_STORE:
    femit_x86_64(context, I_MOV, REGISTER_TO_MEMORY,
                 instruction->value.pair.cdr->result,
                 REG_RBP,
                 (int64_t)-instruction->value.pair.car->value.stack_allocation.offset);
    break;
  case IR_LOCAL_LOAD:
    femit_x86_64(context, I_MOV, MEMORY_TO_REGISTER,
                 REG_RBP,
                 (int64_t)-instruction->value.reference->value.stack_allocation.offset,
                 instruction->result);
    break;
  case IR_CALL:
    if (0) {}
    // Save caller saved registers used in caller function.
    ASSERT(instruction->block, "call instruction null block");
    ASSERT(instruction->block->function, "block has null function");

    // Tail call.
    if (instruction->value.call.tail_call) {
      // Restore the frame pointer if we have one.
      if (instruction->block->function->locals_total_size) {
        femit_x86_64(context, I_MOV, REGISTER_TO_REGISTER, REG_RBP, REG_RSP);
        femit_x86_64(context, I_POP, REGISTER, REG_RBP);
      }

      if (instruction->value.call.type == IR_CALLTYPE_INDIRECT) {
        femit_x86_64(context, I_JMP, REGISTER, instruction->value.call.value.callee->result);
      } else {
        femit_x86_64(context, I_JMP, NAME, instruction->value.call.value.name);
      }
      break;
    }

    int64_t func_regs = instruction->block->function->registers_in_use;
    for (int i = REG_RBX; i < sizeof(func_regs) * 8; ++i) {
      if (func_regs & (1 << i) && is_caller_saved(i)) {
        femit_x86_64(context, I_PUSH, REGISTER, i);
      }
    }
    if (instruction->value.call.type == IR_CALLTYPE_INDIRECT) {
      femit_x86_64(context, I_CALL, REGISTER,
                   instruction->value.call.value.callee->result);

    } else {
      femit_x86_64(context, I_CALL, NAME,
                   instruction->value.call.value.name);
    }
    // Restore caller saved registers used in called function.
    for (int i = sizeof(func_regs) * 8 - 1; i > REG_RAX; --i) {
      if (func_regs & (1 << i) && is_caller_saved(i)) {
        femit_x86_64(context, I_POP, REGISTER, i);
      }
    }
    femit_x86_64(context, I_MOV, REGISTER_TO_REGISTER,
                 REG_RAX,
                 instruction->result);
    break;
  case IR_RETURN:
    // Restore callee-saved registers used in the function.
    for (int i = sizeof(instruction->block->function->registers_in_use) * 8 - 1; i > 0; --i) {
      if (instruction->block->function->registers_in_use & ((size_t)1 << i) && is_callee_saved(i)) {
        femit_x86_64(context, I_POP, REGISTER, i);
      }
    }
    codegen_epilogue_x86_64(context, instruction->block->function);
    break;
  case IR_BRANCH:
    // TODO: If jumping to next block, don't generate.
    femit_x86_64(context, I_JMP, NAME, instruction->value.block->name);
    break;
  case IR_BRANCH_CONDITIONAL:
    femit_x86_64(context, I_TEST, REGISTER_TO_REGISTER,
                 instruction->value.conditional_branch.condition->result,
                 instruction->value.conditional_branch.condition->result);
    femit_x86_64(context, I_JCC, JUMP_TYPE_Z, instruction->value.conditional_branch.false_branch->name);
    femit_x86_64(context, I_JMP, NAME, instruction->value.conditional_branch.true_branch->name);
    break;
  case IR_COMPARISON:
    codegen_comparison_x86_64(context, instruction->value.comparison.type,
                              instruction->value.comparison.pair.car->result,
                              instruction->value.comparison.pair.cdr->result,
                              instruction->result);
    break;
  case IR_ADD:
    femit_x86_64(context, I_ADD, REGISTER_TO_REGISTER,
                 instruction->value.pair.cdr->result,
                 instruction->value.pair.car->result);
    femit_x86_64(context, I_MOV, REGISTER_TO_REGISTER,
                 instruction->value.pair.car->result,
                 instruction->result);
    break;
  case IR_SUBTRACT:
    femit_x86_64(context, I_SUB, REGISTER_TO_REGISTER,
                 instruction->value.pair.cdr->result,
                 instruction->value.pair.car->result);
    femit_x86_64(context, I_MOV, REGISTER_TO_REGISTER,
                 instruction->value.pair.car->result,
                 instruction->result);
    break;
  case IR_MULTIPLY:
    femit_x86_64(context, I_IMUL, REGISTER_TO_REGISTER,
                 instruction->value.pair.car->result,
                 instruction->value.pair.cdr->result);
    femit_x86_64(context, I_MOV, REGISTER_TO_REGISTER,
                 instruction->value.pair.cdr->result,
                 instruction->result);
    break;
  case IR_DIVIDE:
    ASSERT(instruction->value.pair.cdr->result != REG_RAX,
           "Register allocation must not allocate RAX to divisor.");
    femit_x86_64(context, I_MOV, REGISTER_TO_REGISTER,
                 instruction->value.pair.car->result,
                 REG_RAX);
    femit_x86_64(context, I_CQO);
    femit_x86_64(context, I_IDIV, REGISTER,
                 instruction->value.pair.cdr->result);
    femit_x86_64(context, I_MOV, REGISTER_TO_REGISTER,
                 REG_RAX,
                 instruction->result);
    break;
  case IR_MODULO:
    ASSERT(instruction->value.pair.cdr->result != REG_RAX,
           "Register allocation must not allocate RAX to divisor.");
    femit_x86_64(context, I_MOV, REGISTER_TO_REGISTER,
                 instruction->value.pair.car->result,
                 REG_RAX);
    femit_x86_64(context, I_CQO);
    femit_x86_64(context, I_IDIV, REGISTER,
                 instruction->value.pair.cdr->result);
    femit_x86_64(context, I_MOV, REGISTER_TO_REGISTER,
                 REG_RDX,
                 instruction->result);
    break;
  case IR_SHIFT_LEFT:
    femit_x86_64(context, I_MOV, REGISTER_TO_REGISTER,
                 instruction->value.pair.cdr->result,
                 REG_RCX);
    femit_x86_64(context, I_SHL, REGISTER,
                 instruction->value.pair.car->result);
    femit_x86_64(context, I_MOV, REGISTER_TO_REGISTER,
                 instruction->value.pair.car->result,
                 instruction->result);
    break;
  case IR_SHIFT_RIGHT_LOGICAL:
    femit_x86_64(context, I_MOV, REGISTER_TO_REGISTER,
                 instruction->value.pair.cdr->result,
                 REG_RCX);
    femit_x86_64(context, I_SHR, REGISTER,
                 instruction->value.pair.car->result);
    femit_x86_64(context, I_MOV, REGISTER_TO_REGISTER,
                 instruction->value.pair.car->result,
                 instruction->result);
    break;
  case IR_SHIFT_RIGHT_ARITHMETIC:
    femit_x86_64(context, I_MOV, REGISTER_TO_REGISTER,
                 instruction->value.pair.cdr->result,
                 REG_RCX);
    femit_x86_64(context, I_SAR, REGISTER,
                 instruction->value.pair.car->result);
    femit_x86_64(context, I_MOV, REGISTER_TO_REGISTER,
                 instruction->value.pair.car->result,
                 instruction->result);
    break;
  case IR_LOAD:
    femit_x86_64(context, I_MOV, MEMORY_TO_REGISTER,
                 instruction->value.reference->result,
                 (int64_t)0,
                 instruction->result);
    break;
  case IR_STORE:
    femit_x86_64(context, I_MOV, REGISTER_TO_MEMORY,
                 instruction->value.pair.car->result,
                 instruction->value.pair.cdr->result,
                 (int64_t)0);
    break;
  default:
    ir_femit_instruction(stderr, instruction);
    TODO("Handle IRType %d\n", instruction->type);
    break;
  }
}

void emit_block(CodegenContext *context, IRBlock *block) {
  // Generate block label.
  fprintf(context->code,
          "%s:\n",
          block->name);
  for (IRInstruction *instruction = block->instructions;
       instruction;
       instruction = instruction->next
       ) {
    emit_instruction(context, instruction);
  }

  emit_instruction(context, block->branch);
}

void emit_function(CodegenContext *context, IRFunction *function) {
  // Generate function entry.
  // TODO: Maybe make some functions not global.
  fprintf(context->code,
          ".global %s\n"
          "%s:\n",
          function->name,
          function->name);
  codegen_prologue_x86_64(context, function->locals_total_size);
  // Save all callee-saved registers in use in the function.
  for (size_t i = 1; i < sizeof(function->registers_in_use) * 8; ++i) {
    if ((size_t)function->registers_in_use & ((size_t)1 << i) && is_callee_saved(i)) {
      femit_x86_64(context, I_PUSH, REGISTER, i);
    }
  }
  for (IRBlock *block = function->first; block; block = block->next) {
    emit_block(context, block);
  }
  // NOTE: Epilogue is generated by `return` instruction.
}

void emit_entry(CodegenContext *context) {
  fprintf(context->code,
          "%s"
          ".section .text\n",
          context->dialect == CG_ASM_DIALECT_INTEL ? ".intel_syntax noprefix\n" : "");
}

static Register *argument_registers = NULL;
static size_t argument_register_count = 0;
static Register general[GENERAL_REGISTER_COUNT] = {
  REG_RAX,
  REG_RBX,
  REG_RCX,
  REG_RDX,
  REG_RSI,
  REG_RDI,
  REG_R8,
  REG_R9,
  REG_R10,
  REG_R11,
  REG_R12,
  REG_R13,
  REG_R14,
  REG_R15,
};

#define LINUX_ARGUMENT_REGISTER_COUNT 6
static Register linux_argument_registers[LINUX_ARGUMENT_REGISTER_COUNT] = {
  REG_RDI, REG_RSI, REG_RDX, REG_RCX, REG_R8, REG_R9
};

#define MSWIN_ARGUMENT_REGISTER_COUNT 4
static Register mswin_argument_registers[MSWIN_ARGUMENT_REGISTER_COUNT] = {
  REG_RCX, REG_RDX, REG_R8, REG_R9
};

#define MSWIN_CALLER_SAVED_REGISTER_COUNT 7
static Register mswin_caller_saved_registers[MSWIN_CALLER_SAVED_REGISTER_COUNT] = {
  REG_RAX, REG_RCX, REG_RDX, REG_R8, REG_R9, REG_R10, REG_R11
};

#define LINUX_CALLER_SAVED_REGISTER_COUNT 9
static Register linux_caller_saved_registers[LINUX_CALLER_SAVED_REGISTER_COUNT] = {
  REG_RAX, REG_RCX, REG_RDX, REG_R8, REG_R9, REG_R10, REG_R11, REG_RSI, REG_RDI
};

static void lower(CodegenContext *context) {
  ASSERT(argument_registers, "arch_x86_64 backend can not lower IR when argument registers have not been initialized.");
  for (IRFunction *function = context->function;
       function;
       function = function->next
       ) {
    for (IRBlock *block = function->first;
         block;
         block = block->next
         ) {
      // If block doesn't have a name, give it one!
      if (!block->name) {
        // TODO: Heap allocate or something (UGHGHGUOEHHGEH).
        // We could also have a static buffer where we write the block
        // id as a string to and then use that as a label all around.
        block->name = label_generate();
      }
      for (IRInstruction *instruction = block->instructions;
           instruction;
           instruction = instruction->next
           ) {
        switch (instruction->type) {
        case IR_PARAMETER_REFERENCE:
          if (instruction->value.immediate >= (int64_t)argument_register_count) {
            TODO("arch_x86_64 doesn't yet support passing arguments on the stack, sorry.");
          }

          // Create instruction to save parameter on function entry onto stack from register.
          IRInstruction *store = calloc(1, sizeof(IRInstruction));
          ASSERT(store, "Could not allocate IRInstruction");
          store->type = IR_LOCAL_STORE;

          IRInstruction *register_instruction = calloc(1, sizeof(IRInstruction));
          ASSERT(register_instruction, "Could not allocate IRInstruction");
          register_instruction->type = IR_REGISTER;
          register_instruction->result = argument_registers[instruction->value.immediate - 1];

          set_pair_and_mark(store, &store->value.pair, instruction, register_instruction);

          insert_instruction_after(store, instruction);
          insert_instruction_after(register_instruction, instruction);

          // Overwrite current instruction to allocate space for parameter on stack.
          instruction->type = IR_STACK_ALLOCATE;
          // TODO: Size here be dictated by size of type of parameter.
          // We would first have to know which function we are within,
          // that way we can lookup the parameters, that way we can
          // lookup the type of the parameter
          // (based on parameter index). Basically, lots of environment
          // lookups.
          instruction->value.stack_allocation.size = 8;
          break;
        default:
          break;
        }
      }
    }
  }
}

void calculate_stack_offsets(CodegenContext *context) {
  for (IRFunction *function = context->function;
       function;
       function = function->next
       ) {
    size_t offset = 0;
    for (IRBlock *block = function->first;
         block;
         block = block->next
         ) {
      for (IRInstruction *instruction = block->instructions;
           instruction;
           instruction = instruction->next
           ) {
        switch (instruction->type) {
        case IR_STACK_ALLOCATE:
          offset += instruction->value.stack_allocation.size;
          instruction->value.stack_allocation.offset = offset;
          break;
        default:
          break;
        }
      }
    }
    function->locals_total_size = offset;
  }
}

int64_t x86_64_instruction_register_interference(IRInstruction *instruction) {
  ASSERT(instruction, "Can not get register interference of NULL instruction.");
  int64_t mask = 0;
  switch(instruction->type) {
  case IR_SHIFT_LEFT:
  case IR_SHIFT_RIGHT_ARITHMETIC:
  case IR_SHIFT_RIGHT_LOGICAL:
    mask |= (1 << REG_RCX);
    break;
  case IR_DIVIDE:
  case IR_MODULO:
    mask |= (1 << REG_RAX);
    mask |= (1 << REG_RDX);
    break;
  default:
    break;
  }
  // Shift mask right because it doesn't include REG_NONE
  return mask >> 1;
}

void codegen_emit_x86_64(CodegenContext *context) {
  // Setup register allocation structures.
  switch (context->call_convention) {
  case CG_CALL_CONV_LINUX:
    caller_saved_register_count = LINUX_CALLER_SAVED_REGISTER_COUNT;
    caller_saved_registers = linux_caller_saved_registers;
    argument_register_count = LINUX_ARGUMENT_REGISTER_COUNT;
    argument_registers = linux_argument_registers;
    break;
  case CG_CALL_CONV_MSWIN:
    caller_saved_register_count = MSWIN_CALLER_SAVED_REGISTER_COUNT;
    caller_saved_registers = mswin_caller_saved_registers;
    argument_register_count = MSWIN_ARGUMENT_REGISTER_COUNT;
    argument_registers = mswin_argument_registers;
    break;
  case CG_CALL_CONV_COUNT:
  default:
    PANIC("Invalid call convention.");
    break;
  }

  // IR fixup for this specific backend.
  lower(context);

  // Generate global variables.
  fprintf(context->code, ".section .data\n");

  Binding *var_it = context->parse_context->variables->bind;
  Node *type_info = node_allocate();
  while (var_it) {
    Node *var_id = var_it->id;
    Node *type_id = node_allocate();
    *type_id = *var_it->value;
    // Do not emit "external" typed variables.
    // TODO: Probably should have external attribute rather than this nonsense!
    if (strcmp(type_id->value.symbol, "external function") != 0) {
      Error err = parse_get_type(context->parse_context, type_id, type_info);
      if (err.type) {
        print_node(type_id, 0);
        print_error(err);
        PANIC();
      }
      fprintf(context->code, "%s: .space %lld\n", var_id->value.symbol, type_info->children->value.integer);
    }
    var_it = var_it->next;
  }
  free(type_info);

  // Allocate registers to each temporary within the program.
  RegisterAllocationInfo *info = ra_allocate_info
    (context,
     REG_RAX,
     GENERAL_REGISTER_COUNT,
     general,
     argument_register_count,
     argument_registers,
     x86_64_instruction_register_interference
     );

  ir_set_ids(context);
  ir_femit(stdout, context);
  ra(info);

  calculate_stack_offsets(context);

  // TODO: Add entry only on entry function, not just the first one.
  emit_entry(context);
  for (IRFunction *function = context->all_functions; function; function = function->next) {
    emit_function(context, function);
  }
}
