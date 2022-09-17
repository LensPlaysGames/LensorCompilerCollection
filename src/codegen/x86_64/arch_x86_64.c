#include "arch_x86_64.h"

#include <codegen.h>
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

#define DEFINE_REGISTER_ENUM(name, ...) REG_##name,
#define REGISTER_NAME_64(ident, name, ...) name,
#define REGISTER_NAME_32(ident, name, name_32, ...) name_32,
#define REGISTER_NAME_16(ident, name, name_32, name_16, ...) name_16,
#define REGISTER_NAME_8(ident, name, name_32, name_16, name_8, ...) name_8,

/// Used when initializing Register arrays for RegisterPool.
#define INIT_REGISTER(ident, ...)  \
  ((registers)[REG_##ident] = (Register){.in_use = 0, .descriptor = (REG_##ident)});

/// Lookup tables for register names.
#define DEFINE_REGISTER_NAME_LOOKUP_FUNCTION(name, bits)                                        \
  static const char *name(RegisterDescriptor descriptor) {                                             \
    static const char* register_names[] = { FOR_ALL_X86_64_REGISTERS(REGISTER_NAME_##bits) };   \
    if (descriptor < 0 || descriptor >= REG_COUNT) {                                     \
      panic("ERROR::" #name "(): Could not find register with descriptor of %d\n", descriptor); \
    }                                                                                           \
    return register_names[descriptor];                                                          \
  }

enum Registers_x86_64 {
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

static const char *instruction_mnemonic_x86_64(enum CodegenOutputFormat fmt, enum Instructions_x86_64 instruction) {
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

  switch (fmt) {
    default: panic("instruction_mnemonic_x86_64(): Unknown output format.");

    case CG_FMT_x86_64_GAS:
      switch (instruction) {
        default: panic("instruction_mnemonic_x86_64(): Unknown instruction.");
        case I_CQO: return "cqto";
      }
  }
}

static void femit_x86_64_imm_to_reg(CodegenContext *context, enum Instructions_x86_64 inst, va_list args) {
  int64_t immediate                    = va_arg(args, int64_t);
  RegisterDescriptor destination_register  = va_arg(args, RegisterDescriptor);

  const char *mnemonic = instruction_mnemonic_x86_64(context->format, inst);
  const char *destination = register_name(destination_register);

  switch (context->format) {
    case CG_FMT_x86_64_GAS:
      fprintf(context->code, "%s $%" PRId64 ", %%%s\n",
          mnemonic, immediate, destination);
      break;
    default: panic("ERROR: femit_x86_64_imm_to_reg(): Unsupported format %d", context->format);
  }
}

static void femit_x86_64_imm_to_mem(CodegenContext *context, enum Instructions_x86_64 inst, va_list args) {
  int64_t immediate                    = va_arg(args, int64_t);
  RegisterDescriptor address_register  = va_arg(args, RegisterDescriptor);
  int64_t offset                       = va_arg(args, int64_t);

  const char *mnemonic = instruction_mnemonic_x86_64(context->format, inst);
  const char *address = register_name(address_register);

  switch (context->format) {
    case CG_FMT_x86_64_GAS:
      fprintf(context->code, "%s $%" PRId64 ", %" PRId64 "(%%%s)\n",
          mnemonic, immediate, offset, address);
      break;
    default: panic("ERROR: femit_x86_64_imm_to_mem(): Unsupported format %d", context->format);
  }
}

static void femit_x86_64_mem_to_reg(CodegenContext *context, enum Instructions_x86_64 inst, va_list args) {
  RegisterDescriptor address_register      = va_arg(args, RegisterDescriptor);
  int64_t offset                           = va_arg(args, int64_t);
  RegisterDescriptor destination_register  = va_arg(args, RegisterDescriptor);

  const char *mnemonic = instruction_mnemonic_x86_64(context->format, inst);
  const char *address = register_name(address_register);
  const char *destination = register_name(destination_register);

  switch (context->format) {
    case CG_FMT_x86_64_GAS:
      fprintf(context->code, "%s %" PRId64 "(%%%s), %%%s\n",
          mnemonic, offset, address, destination);
      break;
    default: panic("ERROR: femit_x86_64_mem_to_reg(): Unsupported format %d", context->format);
  }
}

static void femit_x86_64_name_to_reg(CodegenContext *context, enum Instructions_x86_64 inst, va_list args) {
  RegisterDescriptor address_register      = va_arg(args, RegisterDescriptor);
  char *name                               = va_arg(args, char *);
  RegisterDescriptor destination_register  = va_arg(args, RegisterDescriptor);

  const char *mnemonic = instruction_mnemonic_x86_64(context->format, inst);
  const char *address = register_name(address_register);
  const char *destination = register_name(destination_register);

  switch (context->format) {
    case CG_FMT_x86_64_GAS:
      fprintf(context->code, "%s %s(%%%s), %%%s\n",
          mnemonic, name, address, destination);
      break;
    default: panic("ERROR: femit_x86_64_name_to_reg(): Unsupported format %d", context->format);
  }
}

static void femit_x86_64_reg_to_mem(CodegenContext *context, enum Instructions_x86_64 inst, va_list args) {
  RegisterDescriptor source_register   = va_arg(args, RegisterDescriptor);
  RegisterDescriptor address_register  = va_arg(args, RegisterDescriptor);
  int64_t offset                       = va_arg(args, int64_t);

  const char *mnemonic = instruction_mnemonic_x86_64(context->format, inst);
  const char *source = register_name(source_register);
  const char *address = register_name(address_register);

  switch (context->format) {
    case CG_FMT_x86_64_GAS:
      fprintf(context->code, "%s %%%s, %" PRId64 "(%%%s)\n",
          mnemonic, source, offset, address);
      break;
    default: panic("ERROR: femit_x86_64_reg_to_mem(): Unsupported format %d", context->format);
  }
}

static void femit_x86_64_reg_to_reg(CodegenContext *context, enum Instructions_x86_64 inst, va_list args) {
  RegisterDescriptor source_register       = va_arg(args, RegisterDescriptor);
  RegisterDescriptor destination_register  = va_arg(args, RegisterDescriptor);

  const char *mnemonic = instruction_mnemonic_x86_64(context->format, inst);
  const char *source = register_name(source_register);
  const char *destination = register_name(destination_register);

  // Optimise away moves from a register to itself
  if (inst == I_MOV && source_register == destination_register) return;

  switch (context->format) {
    case CG_FMT_x86_64_GAS:
      fprintf(context->code, "%s %%%s, %%%s\n",
          mnemonic, source, destination);
      break;
    default: panic("ERROR: femit_x86_64_reg_to_reg(): Unsupported format %d", context->format);
  }
}

static void femit_x86_64_reg_to_name(CodegenContext *context, enum Instructions_x86_64 inst, va_list args) {
  RegisterDescriptor source_register  = va_arg(args, RegisterDescriptor);
  RegisterDescriptor address_register      = va_arg(args, RegisterDescriptor);
  char *name                               = va_arg(args, char *);

  const char *mnemonic = instruction_mnemonic_x86_64(context->format, inst);
  const char *source = register_name(source_register);
  const char *address = register_name(address_register);

  switch (context->format) {
    case CG_FMT_x86_64_GAS:
      fprintf(context->code, "%s %%%s, %s(%%%s)\n",
          mnemonic, source, name, address);
      break;
    default: panic("ERROR: femit_x86_64_reg_to_name(): Unsupported format %d", context->format);
  }
}

static void femit_x86_64_mem(CodegenContext *context, enum Instructions_x86_64 inst, va_list args) {
  int64_t offset                           = va_arg(args, int64_t);
  RegisterDescriptor address_register      = va_arg(args, RegisterDescriptor);

  const char *mnemonic = instruction_mnemonic_x86_64(context->format, inst);
  const char *address = register_name(address_register);

  switch (context->format) {
    case CG_FMT_x86_64_GAS:
      fprintf(context->code, "%s %" PRId64 "(%%%s)\n",
          mnemonic, offset, address);
      break;
    default: panic("ERROR: femit_x86_64_mem(): Unsupported format %d", context->format);
  }
}

static void femit_x86_64_reg(CodegenContext *context, enum Instructions_x86_64 inst, va_list args) {
  RegisterDescriptor source_register   = va_arg(args, RegisterDescriptor);

  const char *mnemonic = instruction_mnemonic_x86_64(context->format, inst);
  const char *source = register_name(source_register);

  switch (context->format) {
    case CG_FMT_x86_64_GAS:
      fprintf(context->code, "%s %%%s\n",
          mnemonic, source);
      break;
    default: panic("ERROR: femit_x86_64_reg(): Unsupported format %d", context->format);
  }
}

static void femit_x86_64_imm(CodegenContext *context, enum Instructions_x86_64 inst, va_list args) {
  int64_t immediate = va_arg(args, int64_t);

  const char *mnemonic = instruction_mnemonic_x86_64(context->format, inst);

  switch (context->format) {
    case CG_FMT_x86_64_GAS:
      fprintf(context->code, "%s $%" PRId64 "\n",
          mnemonic, immediate);
      break;
    default: panic("ERROR: femit_x86_64_imm(): Unsupported format %d", context->format);
  }
}

static void femit_x86_64_indirect_branch(CodegenContext *context, enum Instructions_x86_64 inst, va_list args) {
  RegisterDescriptor address_register   = va_arg(args, RegisterDescriptor);

  const char *mnemonic = instruction_mnemonic_x86_64(context->format, inst);
  const char *address = register_name(address_register);

  switch (context->format) {
    case CG_FMT_x86_64_GAS:
      fprintf(context->code, "%s *%%%s\n",
          mnemonic, address);
      break;
    default: panic("ERROR: femit_x86_64_indirect_branch(): Unsupported format %d", context->format);
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
          const char *mnemonic = instruction_mnemonic_x86_64(context->format, instruction);
          const char *cl = register_name_8(REG_RCX);

          switch (context->format) {
            case CG_FMT_x86_64_GAS:
              fprintf(context->code, "%s %%%s, %%%s\n",
                  mnemonic, cl, register_name(register_to_shift));
              break;
            default: panic("ERROR: femit_x86_64(): Unsupported format %d for shift instruction", context->format);
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
          const char *mnemonic = instruction_mnemonic_x86_64(context->format, instruction);

          switch (context->format) {
            case CG_FMT_x86_64_GAS:
              fprintf(context->code, "%s %s\n",
                  mnemonic, label);
              break;
            default: panic("ERROR: femit_x86_64(): Unsupported format %d for CALL/JMP instruction", context->format);
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

      const char *mnemonic = instruction_mnemonic_x86_64(context->format, instruction);
      const char *value = register_name_8(value_register);

      switch (context->format) {
        case CG_FMT_x86_64_GAS:
          fprintf(context->code, "%s%s %%%s\n",
              mnemonic,
              comparison_suffixes_x86_64[comparison_type], value);
          break;
        default: panic("ERROR: femit_x86_64(): Unsupported format %d", context->format);
      }
    } break;

    case I_JCC: {
      enum IndirectJumpType_x86_64 type = va_arg(args, enum IndirectJumpType_x86_64);
      ASSERT(type < JUMP_TYPE_COUNT, "femit_x86_64_direct_branch(): Invalid jump type %d", type);
      char *label = va_arg(args, char *);

      const char *mnemonic = instruction_mnemonic_x86_64(context->format, I_JCC);

      switch (context->format) {
        case CG_FMT_x86_64_GAS:
          fprintf(context->code, "%s%s %s\n",
              mnemonic, jump_type_names_x86_64[type], label);
          break;
        default: panic("ERROR: femit_x86_64_direct_branch(): Unsupported format %d", context->format);
      }
    } break;

    case I_RET:
    case I_CQO: {
      const char *mnemonic = instruction_mnemonic_x86_64(context->format, instruction);
      fprintf(context->code, "%s\n", mnemonic);
    } break;

    default: panic("Unhandled instruction in x86_64 code generation: %d.", instruction);
  }

  va_end(args);
}

/// X86_64-specific code generation state.
typedef struct ArchData {
  /// The type of function call that is currently being emitted.
  enum {
    FUNCTION_CALL_TYPE_NONE,
    FUNCTION_CALL_TYPE_INTERNAL,
    FUNCTION_CALL_TYPE_EXTERNAL,
  } current_call;
  /// The number of arguments emitted.
  size_t call_arg_count;
  char rax_in_use;
} ArchData;

/// Creates a context for the CG_FMT_x86_64_MSWIN architecture.
CodegenContext *codegen_context_x86_64_mswin_create(CodegenContext *parent) {
  RegisterPool pool;

  // If this is the top level context, create the registers.
  // Otherwise, shallow copy register pool to child context.
  if (!parent) {
    Register *registers = calloc(REG_COUNT, sizeof(Register));
    FOR_ALL_X86_64_REGISTERS(INIT_REGISTER)

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
  } else {
    cg_ctx->arch_data = calloc(1, sizeof(ArchData));
  }

  cg_ctx->parent = parent;
  cg_ctx->locals = environment_create(NULL);
  cg_ctx->locals_offset = -32;
  cg_ctx->register_pool = pool;
  cg_ctx->format = CG_FMT_x86_64_GAS;
  cg_ctx->call_convention = CG_CALL_CONV_MSWIN;
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

/// Load an immediate value into a new register.
RegisterDescriptor codegen_load_immediate_x86_64
(CodegenContext *cg_context,
 long long int immediate) {
  RegisterDescriptor result = register_allocate(cg_context);
  femit_x86_64(cg_context, I_MOV,
               IMMEDIATE_TO_REGISTER,
               immediate, result);
  return result;
}

/// Copy the return value from RAX into a new register.
static RegisterDescriptor copy_return_value(CodegenContext *cg_context) {
  ArchData *arch_data = cg_context->arch_data;
  ASSERT(arch_data->current_call != FUNCTION_CALL_TYPE_NONE);

  if (arch_data->rax_in_use) {
    RegisterDescriptor result = register_allocate(cg_context);
    femit_x86_64(cg_context, I_MOV,
                 REGISTER_TO_REGISTER,
                 REG_RAX, result);
    return result;
  }

  return REG_RAX;
}

/// Calculate quotient and remainder of lhs by rhs.
static RegisterDescriptor divmod
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 char wants_quotient,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs) {
  // Quotient is in RAX, Remainder in RDX; we must save and restore these
  // registers before and after divide, if they were in use, unless either
  // register is the lhs and rhs and we are allowed to clobber registers.
  Register* rax = cg_context->register_pool.registers + REG_RAX;
  Register* rdx = cg_context->register_pool.registers + REG_RDX;

  char rax_pushed = rax->in_use && (mode == CODEGEN_PRESERVE_OPERANDS || (lhs != REG_RAX && rhs != REG_RAX));
  char rdx_pushed = rdx->in_use && (mode == CODEGEN_PRESERVE_OPERANDS || (lhs != REG_RDX && rhs != REG_RDX));

  if (rax_pushed) { femit_x86_64(cg_context, I_PUSH, REGISTER, REG_RAX); }
  if (rdx_pushed) { femit_x86_64(cg_context, I_PUSH, REGISTER, REG_RDX); }

  // In the unfortunate case that the rhs is in RAX or RDX, we must move
  // it to a scratch register before we can divide.
  char actual_rhs_is_scratch = 0;
  RegisterDescriptor actual_rhs = rhs;
  if (rhs == REG_RAX || rhs == REG_RDX) {
    actual_rhs = register_allocate(cg_context);
    femit_x86_64(cg_context, I_MOV, REGISTER_TO_REGISTER, rhs, actual_rhs);
    actual_rhs_is_scratch = 1;
  }

  // Load RAX with left hand side of division operator, if needed.
  femit_x86_64(cg_context, I_MOV, REGISTER_TO_REGISTER, lhs, REG_RAX);

  // Sign-extend the value in RAX to RDX. RDX is treated as the
  // 8 high bytes of a 16-byte number stored in RDX:RAX.
  femit_x86_64(cg_context, I_CQO);

  // Call IDIV with right hand side of division operator.
  femit_x86_64(cg_context, I_IDIV, REGISTER, actual_rhs);

  // Copy the wanted result into a register. There are several optimisations
  // that we can employ here. We pick the fist one that we can use.
  //   - If either the lhs or rhs is RAX or RDX, depending on whether we want the
  //     quotient or remainder, respectively, and we are allowed to clobber
  //     registers, we can just return the corresponding register.
  //   - If we are allowed to clobber registers, we can move the result into
  //     either lhs or rhs.
  //   - If we have allocated a scratch register for the rhs, we can move the
  //     result into that register.
  //   - Otherwise, we must allocate a new register.
  RegisterDescriptor result;
  if (mode == CODEGEN_CLOBBER_OPERANDS) {
    if (wants_quotient && (lhs == REG_RAX || rhs == REG_RAX)) { result = REG_RAX; }
    else if (!wants_quotient && (lhs == REG_RDX || rhs == REG_RDX)) { result = REG_RDX; }
    else {
      femit_x86_64(cg_context, I_MOV, REGISTER_TO_REGISTER,
                   wants_quotient ? REG_RAX : REG_RDX,
                   lhs);
      result = lhs;
    }
  } else {
    result = actual_rhs_is_scratch ? actual_rhs : register_allocate(cg_context);
    femit_x86_64(cg_context, I_MOV, REGISTER_TO_REGISTER,
                 wants_quotient ? REG_RAX : REG_RDX,
                 result);
  }

  // Cleanup everything.
  // If we allocated a scratch register for the rhs, free it unless it is the result.
  if (actual_rhs_is_scratch && actual_rhs != result) {
    register_deallocate(cg_context, actual_rhs);
  }

  // We must restore RAX and RDX if we pushed them.
  if (rdx_pushed) { femit_x86_64(cg_context, I_POP, REGISTER, REG_RDX); }
  if (rax_pushed) { femit_x86_64(cg_context, I_POP, REGISTER, REG_RAX); }

  // At this point, we have already taken care of restoring everything
  // that we had to preserve. We can now free any registers that we
  // no longer need if we are allowed to clobber registers.
  if (mode == CODEGEN_CLOBBER_OPERANDS) {
    if (lhs != result) { register_deallocate(cg_context, lhs); }
    if (rhs != result) { register_deallocate(cg_context, rhs); }
  }

  return result;
}

/// Shift lhs by rhs.
static RegisterDescriptor shift
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 enum Instructions_x86_64 shift_instruction,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs) {
  Register* rcx = cg_context->register_pool.registers + REG_RCX;
  if (mode == CODEGEN_CLOBBER_OPERANDS) {
    // Save RCX if it's in use and not the same as lhs or rhs by swapping
    // it with rhs. Otherwise, if the lhs is RCX, swap it with the rhs.
    // Otherwise, move the rhs into RCX.
    char save_rcx = rcx->in_use && (lhs != REG_RCX && rhs != REG_RCX);
    if (save_rcx) {
      femit_x86_64(cg_context, I_XCHG, REGISTER_TO_REGISTER, REG_RCX, rhs);
    } else if (lhs == REG_RCX) {
      femit_x86_64(cg_context, I_XCHG, REGISTER_TO_REGISTER, lhs, rhs);
    } else {
      femit_x86_64(cg_context, I_MOV, REGISTER_TO_REGISTER, rhs, REG_RCX);
    }

    femit_x86_64(cg_context, shift_instruction, REGISTER, lhs);

    // If we saved RCX, restore it. Deallocate rhs since it's no longer
    // needed and return lhs.
    if (save_rcx) {
      femit_x86_64(cg_context, I_XCHG, REGISTER_TO_REGISTER, REG_RCX, rhs);
    }
    register_deallocate(cg_context, rhs);
    return lhs;
  } else {
    RegisterDescriptor result = register_allocate(cg_context);
    // If RCX is in use and not the result register, save it.
    char rcx_saved = rcx->in_use && result != REG_RCX;
    if (rcx_saved) { femit_x86_64(cg_context, I_PUSH, REGISTER, REG_RCX); }

    // Move the rhs into RCX.
    femit_x86_64(cg_context, I_MOV, REGISTER_TO_REGISTER, rhs, REG_RCX);

    // If the result is not RCX, move the lhs into it. Otherwise,
    // save lhs and restore it after the shift.
    if (result != REG_RCX) {
      femit_x86_64(cg_context, I_MOV, REGISTER_TO_REGISTER, lhs, result);
      femit_x86_64(cg_context, shift_instruction, REGISTER, result);
    } else {
      femit_x86_64(cg_context, I_PUSH, REGISTER, lhs);
      femit_x86_64(cg_context, shift_instruction, REGISTER, REG_RCX);
      femit_x86_64(cg_context, I_MOV, REGISTER_TO_REGISTER, lhs, result);
      femit_x86_64(cg_context, I_POP, REGISTER, lhs);
    }

    // If we saved RCX, restore it.
    if (rcx_saved) { femit_x86_64(cg_context, I_POP, REGISTER, REG_RCX); }
    return result;
  }
}

/// Save state before a function call.
void codegen_prepare_call_x86_64(CodegenContext *cg_context) {
  ArchData *arch_data = cg_context->arch_data;
  if (arch_data->current_call != FUNCTION_CALL_TYPE_NONE) {
    panic("Cannot prepare call if a call is already prepared");
  }

  arch_data->rax_in_use = cg_context->register_pool.registers[REG_RAX].in_use;
  if (arch_data->rax_in_use) femit_x86_64(cg_context, I_PUSH, REGISTER, REG_RAX);
}

/// Add an argument to the current function call.
void codegen_add_external_function_arg_x86_64(CodegenContext *cg_context, RegisterDescriptor arg) {
  ArchData *arch_data = cg_context->arch_data;
  switch (arch_data->current_call) {
    case FUNCTION_CALL_TYPE_NONE: arch_data->current_call = FUNCTION_CALL_TYPE_EXTERNAL; break;
    case FUNCTION_CALL_TYPE_EXTERNAL: break;
    default: panic("Cannot add external argument if internal call is prepared");
  }

  switch (cg_context->call_convention) {
    case CG_CALL_CONV_MSWIN: {
      switch (arch_data->call_arg_count++) {
        case 0: femit_x86_64(cg_context, I_MOV, REGISTER_TO_REGISTER, arg, REG_RCX); break;
        case 1: femit_x86_64(cg_context, I_MOV, REGISTER_TO_REGISTER, arg, REG_RDX); break;
        case 2: femit_x86_64(cg_context, I_MOV, REGISTER_TO_REGISTER, arg, REG_R8); break;
        case 3: femit_x86_64(cg_context, I_MOV, REGISTER_TO_REGISTER, arg, REG_R9); break;
        default: panic("Too many function arguments");
      }
    } break;

    case CG_CALL_CONV_LINUX: {
      ASSERT(0, "Not implemented.");
    } break;

    default: panic("Unsupported calling convention: %d.", cg_context->call_convention);
  }
}

/// Add an argument to the current function call.
void codegen_add_internal_function_arg_x86_64(CodegenContext *cg_context, RegisterDescriptor arg) {
  ArchData *arch_data = cg_context->arch_data;
  switch (arch_data->current_call) {
    case FUNCTION_CALL_TYPE_NONE: arch_data->current_call = FUNCTION_CALL_TYPE_INTERNAL; break;
    case FUNCTION_CALL_TYPE_INTERNAL: break;
    default: panic("Cannot add internal argument if external call is prepared");
  }

  arch_data->call_arg_count++;
  femit_x86_64(cg_context, I_PUSH, REGISTER, arg);
}

/// Call an external function. Returns the register containing the return value.
RegisterDescriptor codegen_perform_external_call_x86_64
(CodegenContext *cg_context,
 const char* function_name) {
  ArchData *arch_data = cg_context->arch_data;
  switch (arch_data->current_call) {
    case FUNCTION_CALL_TYPE_NONE: arch_data->current_call = FUNCTION_CALL_TYPE_EXTERNAL; break;
    case FUNCTION_CALL_TYPE_EXTERNAL: break;
    default: panic("Cannot perform external call after preparing internal call");
  }

  femit_x86_64(cg_context, I_CALL, NAME, function_name);
  return copy_return_value(cg_context);
}

/// Call an internal function. Return the register containing the return value.
RegisterDescriptor codegen_perform_internal_call_x86_64(CodegenContext *cg_context, RegisterDescriptor function) {
  ArchData *arch_data = cg_context->arch_data;
  switch (arch_data->current_call) {
    case FUNCTION_CALL_TYPE_NONE: arch_data->current_call = FUNCTION_CALL_TYPE_INTERNAL; break;
    case FUNCTION_CALL_TYPE_INTERNAL: break;
    default: panic("Cannot perform internal call after preparing external call");
  }

  femit_x86_64(cg_context, I_CALL, REGISTER, function);
  return copy_return_value(cg_context);
}

/// Clean up after a function call.
void codegen_cleanup_call_x86_64(CodegenContext *cg_context) {
  ArchData *arch_data = cg_context->arch_data;

  switch (arch_data->current_call) {
    // Pop arguments off the stack.
    case FUNCTION_CALL_TYPE_EXTERNAL:
      femit_x86_64(cg_context, I_ADD, IMMEDIATE_TO_REGISTER,
                   arch_data->call_arg_count * 8, REG_RSP);
      break;
    case FUNCTION_CALL_TYPE_INTERNAL: break;
    default: panic("No call to clean up");
  }

  // Restore rax if it was in use.
  if (arch_data->rax_in_use) femit_x86_64(cg_context, I_POP, REGISTER, REG_RAX);
  else femit_x86_64(cg_context, I_ADD, IMMEDIATE_TO_REGISTER,
                    (int64_t)8, REG_RSP);

  // Clean up the call state.
  arch_data->current_call = FUNCTION_CALL_TYPE_NONE;
  arch_data->call_arg_count = 0;
  arch_data->rax_in_use = 0;
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
void codegen_load_local_into_x86_64
(CodegenContext *cg_context,
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
  femit_x86_64(cg_context, I_MOV, REGISTER_TO_MEMORY, source, address, 0);
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
 enum CodegenBinaryOpMode mode,
 enum ComparisonType type,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs) {
  ASSERT(type < COMPARE_COUNT, "Invalid comparison type");
  RegisterDescriptor result = register_allocate(cg_context);

  // Zero out result register.
  femit_x86_64(cg_context, I_XOR, REGISTER_TO_REGISTER, result, result);

  // Perform the comparison.
  femit_x86_64(cg_context, I_CMP, REGISTER_TO_REGISTER, rhs, lhs);
  femit_x86_64(cg_context, I_SETCC, type, result);

  if (mode == CODEGEN_CLOBBER_OPERANDS) {
    register_deallocate(cg_context, lhs);
    register_deallocate(cg_context, rhs);
  }

  return result;
}

/// Add two registers together.
RegisterDescriptor codegen_add_x86_64
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs) {
  if (mode == CODEGEN_CLOBBER_OPERANDS) {
    femit_x86_64(cg_context, I_ADD, REGISTER_TO_REGISTER, lhs, rhs);
    register_deallocate(cg_context, lhs);
    return rhs;
  } else {
    RegisterDescriptor result = register_allocate(cg_context);
    femit_x86_64(cg_context, I_MOV, REGISTER_TO_REGISTER, rhs, result);
    femit_x86_64(cg_context, I_ADD, REGISTER_TO_REGISTER, lhs, result);
    return result;
  }
}

/// Subtract rhs from lhs.
RegisterDescriptor codegen_subtract_x86_64
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs) {
  if (mode == CODEGEN_CLOBBER_OPERANDS) {
    femit_x86_64(cg_context, I_SUB, REGISTER_TO_REGISTER, rhs, lhs);
    register_deallocate(cg_context, rhs);
    return lhs;
  } else {
    RegisterDescriptor result = register_allocate(cg_context);
    femit_x86_64(cg_context, I_MOV, REGISTER_TO_REGISTER, lhs, result);
    femit_x86_64(cg_context, I_SUB, REGISTER_TO_REGISTER, rhs, result);
    return result;
  }
}

/// Multiply two registers together.
RegisterDescriptor codegen_multiply_x86_64
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs) {
  if (mode == CODEGEN_CLOBBER_OPERANDS) {
    femit_x86_64(cg_context, I_IMUL, REGISTER_TO_REGISTER, lhs, rhs);
    register_deallocate(cg_context, lhs);
    return rhs;
  } else {
    RegisterDescriptor result = register_allocate(cg_context);
    femit_x86_64(cg_context, I_MOV, REGISTER_TO_REGISTER, rhs, result);
    femit_x86_64(cg_context, I_IMUL, REGISTER_TO_REGISTER, lhs, result);
    return result;
  }
}

/// Divide lhs by rhs.
RegisterDescriptor codegen_divide_x86_64
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs) {
  return divmod(cg_context, mode, 1, lhs, rhs);
}

/// Modulo lhs by rhs.
RegisterDescriptor codegen_modulo_x86_64
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs) {
  return divmod(cg_context, mode, 0, lhs, rhs);
}

/// Shift lhs to the left by rhs.
RegisterDescriptor codegen_shift_left_x86_64
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs) {
  return shift(cg_context, mode, I_SAL, lhs, rhs);
}

/// Shift lhs to the right by rhs (arithmetic).
RegisterDescriptor codegen_shift_right_arithmetic_x86_64
(CodegenContext *cg_context,
 enum CodegenBinaryOpMode mode,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs) {
  return shift(cg_context, mode, I_SAR, lhs, rhs);
}

/// Allocate space on the stack.
void codegen_alloca_x86_64(CodegenContext *cg_context, long long int size) {
  femit_x86_64(cg_context, I_SUB, IMMEDIATE_TO_REGISTER, size, REG_RSP);
}

/// Emit the function prologue.
void codegen_prologue_x86_64(CodegenContext *cg_context) {
  femit_x86_64(cg_context, I_PUSH, REGISTER, REG_RBP);
  femit_x86_64(cg_context, I_MOV, REGISTER_TO_REGISTER, REG_RSP, REG_RBP);
  femit_x86_64(cg_context, I_SUB, IMMEDIATE_TO_REGISTER, -cg_context->locals_offset, REG_RSP);
}

/// Emit the function epilogue.
void codegen_epilogue_x86_64(CodegenContext *cg_context) {
  femit_x86_64(cg_context, I_MOV, REGISTER_TO_REGISTER, REG_RBP, REG_RSP);
  femit_x86_64(cg_context, I_POP, REGISTER, REG_RBP);
  femit_x86_64(cg_context, I_RET);
}

/// Set the return value of a function.
void codegen_set_return_value_x86_64(CodegenContext *cg_context, RegisterDescriptor value) {
  femit_x86_64(cg_context, I_MOV, REGISTER_TO_REGISTER, value, REG_RAX);
}

/// Emit the entry point of the program.
void codegen_entry_point_x86_64(CodegenContext *cg_context) {
  fprintf(cg_context->code,
      ".section .text\n"
      ".global main\n"
      "main:\n");
  codegen_prologue_x86_64(cg_context);
}