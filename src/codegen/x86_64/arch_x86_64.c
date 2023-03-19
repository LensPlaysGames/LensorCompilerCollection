#include <codegen/x86_64/arch_x86_64.h>

#include <ast.h>
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
#include <vector.h>
#include <utils.h>

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
      ICE("ERROR::" #name "(): Could not find register with descriptor of %d\n", descriptor); \
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

static Register *argument_registers = NULL;
static size_t argument_register_count = 0;
static Register general[GENERAL_REGISTER_COUNT] = {
  REG_RAX,
  REG_RCX,
  REG_RDX,
  REG_RSI,
  REG_RDI,
  REG_R8,
  REG_R9,
  REG_R10,
  REG_R11,
  REG_R12,
  REG_RBX,
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


NODISCARD static bool is_caller_saved(Register r) {
  for (size_t i = 0; i < caller_saved_register_count; ++i) {
    if (caller_saved_registers[i] == r) {
      return 1;
    }
  }
  return 0;
}

NODISCARD static bool is_callee_saved(Register r) { return !is_caller_saved(r); }

span unreferenced_block_name = literal_span_raw("");

/// Types of conditional jump instructions (Jcc).
/// Do NOT reorder these.
enum IndirectJumpType {
  JUMP_TYPE_A,
  JUMP_TYPE_AE,
  JUMP_TYPE_B,
  JUMP_TYPE_BE,
  JUMP_TYPE_C,
  JUMP_TYPE_Z,
  JUMP_TYPE_E = JUMP_TYPE_Z,
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
  JUMP_TYPE_NZ = JUMP_TYPE_NE,
  JUMP_TYPE_NG,
  JUMP_TYPE_NGE,
  JUMP_TYPE_NL,
  JUMP_TYPE_NLE,
  JUMP_TYPE_NO,
  JUMP_TYPE_NP,
  JUMP_TYPE_NS,
  JUMP_TYPE_O,
  JUMP_TYPE_P,
  JUMP_TYPE_PE,
  JUMP_TYPE_PO,
  JUMP_TYPE_S,

  JUMP_TYPE_COUNT,
};

/// Do NOT reorder these.
static const char *jump_type_names_x86_64[JUMP_TYPE_COUNT] = {
    "a",
    "ae",
    "b",
    "be",
    "c",
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
    "nz",
    "ng",
    "nge",
    "nl",
    "nle",
    "no",
    "np",
    "ns",
    "o",
    "p",
    "pe",
    "po",
    "s",
};

// TODO: All instructions we use in x86_64 should be in this enum.
enum Instruction {
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
  I_AND, ///< Reg reg | Immediate imm, Reg reg
  I_OR,  ///< Reg reg | Immediate imm, Reg reg
  I_NOT,

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

  I_MOVSX,
  I_MOVZX,

  /// Using this for anything other than Reg <-> Reg is a VERY bad
  /// idea unless you know what you're doing.
  I_XCHG,

  I_COUNT
};

// Maximum size of parameter that can go in a register vs on the stack.
// TODO: Has to do with calling convention?
static const usz max_register_size = 8;

enum RegSize {
  r64,
  r32,
  r16,
  r8,
};

/// Return the corresponding RegSize enum value to the given amount of
/// bytes (smallest fit). ICE if can not contain.
static enum RegSize regsize_from_bytes(u64 bytes) {
  switch (bytes) {
  case 1: return r8;
  case 2: return r16;
  case 4: return r32;
  case 8: return r64;
  default:
    ICE("Byte size can not be converted into register size on x86_64: %U", bytes);
    break;
  }
}

/// Return the corresponding byte size of a valid RegSize enum value.
/// ICE if enum value invalid.
static usz regbytes_from_size(enum RegSize r) {
  switch (r) {
  case r8: return 1;
  case r16: return 2;
  case r32: return 4;
  case r64: return 8;
  default:
    ICE("Register size can not be converted into byte count on x86_64: %U", r);
    break;
  }
}

static const char * regname(RegisterDescriptor reg, enum RegSize size) {
  switch (size) {
  case r64: return register_name(reg);
  case r32: return register_name_32(reg);
  case r16: return register_name_16(reg);
  case r8:  return register_name_8(reg);
  default:
    UNREACHABLE();
    break;
  }
}

static const char * regname_from_bytes(RegisterDescriptor reg, u64 bytes) {
  return regname(reg, regsize_from_bytes(bytes));
}

const char *setcc_suffixes_x86_64[COMPARE_COUNT] = {
    "e",
    "ne",
    "l",
    "le",
    "g",
    "ge",
};

static const char *instruction_mnemonic(CodegenContext *context, enum Instruction instruction) {
  STATIC_ASSERT(I_COUNT == 26, "ERROR: instruction_mnemonic() must exhaustively handle all instructions.");
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
  case I_AND: return "and";
  case I_OR: return "or";
  case I_NOT: return "not";
  case I_PUSH: return "push";
  case I_POP: return "pop";
  case I_XOR: return "xor";
  case I_CMP: return "cmp";
  case I_CALL: return "call";
  case I_JMP: return "jmp";
  case I_RET: return "ret";
  case I_MOV: return "mov";
  case I_MOVSX: return "movsx";
  case I_MOVZX: return "movzx";
  case I_XCHG: return "xchg";
  case I_LEA: return "lea";
  case I_SETCC: return "set";
  case I_TEST: return "test";
  case I_JCC: return "j";
  }

  switch (context->dialect) {

  case CG_ASM_DIALECT_ATT: {
    switch (instruction) {
    case I_CQO: return "cqto";
    default: break;
    }
  } break;

  case CG_ASM_DIALECT_INTEL: {
    switch (instruction) {
    case I_CQO: return "cqo";
    default: break;
    } break;
  } break;

  default: break;
  }
  ICE("instruction_mnemonic(): Unknown instruction.");
}

static enum IndirectJumpType comparison_to_jump_type(enum ComparisonType comparison) {
  switch (comparison) {
    case COMPARE_EQ: return JUMP_TYPE_E;
    case COMPARE_NE: return JUMP_TYPE_NE;
    case COMPARE_LT: return JUMP_TYPE_L;
    case COMPARE_LE: return JUMP_TYPE_LE;
    case COMPARE_GT: return JUMP_TYPE_G;
    case COMPARE_GE: return JUMP_TYPE_GE;
    default: ICE("comparison_to_jump_type_x86_64(): Unknown comparison type.");
  }
}

static enum IndirectJumpType negate_jump(enum IndirectJumpType j) {
  switch (j) {
    case JUMP_TYPE_E: return JUMP_TYPE_NE;
    case JUMP_TYPE_NE: return JUMP_TYPE_E;
    case JUMP_TYPE_L: return JUMP_TYPE_GE;
    case JUMP_TYPE_LE: return JUMP_TYPE_G;
    case JUMP_TYPE_G: return JUMP_TYPE_LE;
    case JUMP_TYPE_GE: return JUMP_TYPE_L;
    default: ICE("negate_jump(): Unknown jump type.");
  }
}

static void femit_imm_to_reg(CodegenContext *context, enum Instruction inst, int64_t immediate, RegisterDescriptor destination_register, enum RegSize size) {
  const char *mnemonic    = instruction_mnemonic(context, inst);
  const char *destination = regname(destination_register, size);
  switch (context->dialect) {
    case CG_ASM_DIALECT_ATT:
      fprint(context->code, "    %s $%D, %%%s\n",
          mnemonic, immediate, destination);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprint(context->code, "    %s %s, %D\n",
          mnemonic, destination, immediate);
      break;
    default: ICE("ERROR: femit_imm_to_reg(): Unsupported dialect %d", context->dialect);
  }
}

static void femit_imm_to_mem(CodegenContext *context, enum Instruction inst, int64_t immediate, RegisterDescriptor address_register, int64_t offset) {
  const char *mnemonic = instruction_mnemonic(context, inst);
  const char *address = register_name(address_register);
  switch (context->dialect) {
    case CG_ASM_DIALECT_ATT:
      fprint(context->code, "    %s $%D, %D(%%%s)\n",
          mnemonic, immediate, offset, address);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprint(context->code, "    %s [%s + %D], %D\n",
          mnemonic, address, offset, immediate);
      break;
    default: ICE("ERROR: femit_imm_to_mem(): Unsupported dialect %d", context->dialect);
  }
}

static void femit_mem_to_reg(CodegenContext *context, enum Instruction inst, RegisterDescriptor address_register, int64_t offset, RegisterDescriptor destination_register, enum RegSize size) {
  const char *mnemonic = instruction_mnemonic(context, inst);
  const char *address = register_name(address_register);
  const char *destination = regname(destination_register, size);
  switch (context->dialect) {
    case CG_ASM_DIALECT_ATT:
      fprint(context->code, "    %s %D(%%%s), %%%s\n",
          mnemonic, offset, address, destination);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprint(context->code, "    %s %s, [%s + %D]\n",
          mnemonic, destination, address, offset);
      break;
    default: ICE("ERROR: femit_mem_to_reg(): Unsupported dialect %d", context->dialect);
  }
}

static void femit_name_to_reg(CodegenContext *context, enum Instruction inst, RegisterDescriptor address_register, const char *name, RegisterDescriptor destination_register, enum RegSize size) {
  const char *mnemonic = instruction_mnemonic(context, inst);
  const char *address = register_name(address_register);
  const char *destination = regname(destination_register, size);
  switch (context->dialect) {
    case CG_ASM_DIALECT_ATT:
      fprint(context->code, "    %s (%s)(%%%s), %%%s\n",
          mnemonic, name, address, destination);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprint(context->code, "    %s %s, [%s + %s]\n",
          mnemonic, destination, address, name);
      break;
    default: ICE("ERROR: femit_name_to_reg(): Unsupported dialect %d", context->dialect);
  }
}

static void femit_reg_to_mem(CodegenContext *context, enum Instruction inst, RegisterDescriptor source_register, enum RegSize size, RegisterDescriptor address_register, int64_t offset) {
  const char *mnemonic = instruction_mnemonic(context, inst);
  const char *source = regname(source_register, size);
  const char *address = register_name(address_register);
  switch (context->dialect) {
    case CG_ASM_DIALECT_ATT:
      if (offset) {
        fprint(context->code, "    %s %%%s, %D(%%%s)\n",
                mnemonic, source, offset, address);
      } else {
        fprint(context->code, "    %s %%%s, (%%%s)\n",
                mnemonic, source, address);
      }
      break;
    case CG_ASM_DIALECT_INTEL:
      if (offset) {
        fprint(context->code, "    %s [%s + %D], %s\n",
                mnemonic, address, offset, source);
      } else {
        fprint(context->code, "    %s [%s], %s\n",
                mnemonic, address, source);
      }
      break;
    default: ICE("ERROR: femit_reg_to_mem(): Unsupported dialect %d", context->dialect);
  }
}

static void femit_reg_to_reg
(CodegenContext *context,
 enum Instruction inst,
 RegisterDescriptor source_register, enum RegSize source_size,
 RegisterDescriptor destination_register, enum RegSize destination_size
 )
{
  // Always optimise away moves from a register to itself
  if (inst == I_MOV
      && source_register == destination_register
      && source_size == destination_size)
    {
      fprint(context->code, ";;#; skipping move from self to self\n");
      return;
    }

  const char *mnemonic = instruction_mnemonic(context, inst);
  const char *source = regname(source_register, source_size);
  const char *destination = regname(destination_register, destination_size);

  switch (context->dialect) {
  case CG_ASM_DIALECT_ATT:
    fprint(context->code, "    %s %%%s, %%%s\n",
           mnemonic, source, destination);
    break;
  case CG_ASM_DIALECT_INTEL:
    fprint(context->code, "    %s %s, %s\n",
           mnemonic, destination, source);
    break;
  default: ICE("ERROR: femit_reg_to_reg(): Unsupported dialect %d", context->dialect);
  }
}

static void femit_reg_to_name(CodegenContext *context, enum Instruction inst, RegisterDescriptor source_register, enum RegSize size, RegisterDescriptor address_register, const char *name) {
  const char *mnemonic = instruction_mnemonic(context, inst);
  const char *source = regname(source_register, size);
  const char *address = register_name(address_register);
  switch (context->dialect) {
    case CG_ASM_DIALECT_ATT:
      fprint(context->code, "    %s %%%s, (%s)(%%%s)\n",
          mnemonic, source, name, address);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprint(context->code, "    %s [%s + %s], %s\n",
          mnemonic, address, name, source);
      break;
    default: ICE("ERROR: femit_reg_to_name(): Unsupported dialect %d", context->dialect);
  }
}

static void femit_reg_to_offset_name(CodegenContext *context, enum Instruction inst, RegisterDescriptor source_register, enum RegSize size, RegisterDescriptor address_register, const char *name, usz offset) {
  const char *mnemonic = instruction_mnemonic(context, inst);
  const char *source = regname(source_register, size);
  const char *address = register_name(address_register);
  switch (context->dialect) {
    case CG_ASM_DIALECT_ATT:
      fprint(context->code, "    %s %%%s, (%s+%Z)(%%%s)\n",
          mnemonic, source, name, offset, address);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprint(context->code, "    %s %Z[%s + %s], %s\n",
          mnemonic, offset, name, address, source);
      break;
    default: ICE("ERROR: femit_reg_to_name(): Unsupported dialect %d", context->dialect);
  }
}

static void femit_mem(CodegenContext *context, enum Instruction inst, int64_t offset, RegisterDescriptor address_register) {
  const char *mnemonic = instruction_mnemonic(context, inst);
  const char *address = register_name(address_register);
  switch (context->dialect) {
    case CG_ASM_DIALECT_ATT:
      fprint(context->code, "    %s %D(%%%s)\n",
          mnemonic, offset, address);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprint(context->code, "    %s [%s + %D]\n",
          mnemonic, address, offset);
      break;
    default: ICE("ERROR: femit_mem(): Unsupported dialect %d", context->dialect);
  }
}

static void femit_reg_shift(CodegenContext *context, enum Instruction inst, RegisterDescriptor register_to_shift) {
  const char *mnemonic = instruction_mnemonic(context, inst);
  const char *cl = register_name_8(REG_RCX);
  switch (context->dialect) {
  case CG_ASM_DIALECT_ATT:
    fprint(context->code, "    %s %%%s, %%%s\n",
           mnemonic, cl, register_name(register_to_shift));
    break;
  case CG_ASM_DIALECT_INTEL:
    fprint(context->code, "    %s %s, %s\n",
           mnemonic, register_name(register_to_shift), cl);
    break;
  default: ICE("ERROR: femit(): Unsupported dialect %d for shift instruction", context->dialect);
  }
}

/// You should probably use `femit_reg`
static void femit_indirect_branch(CodegenContext *context, enum Instruction inst, RegisterDescriptor address_register) {
  const char *mnemonic = instruction_mnemonic(context, inst);
  const char *address = register_name(address_register);
  switch (context->dialect) {
    case CG_ASM_DIALECT_ATT:
      fprint(context->code, "    %s *%%%s\n",
          mnemonic, address);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprint(context->code, "    %s %s\n",
          mnemonic, address);
      break;
    default: ICE("ERROR: femit_indirect_branch(): Unsupported dialect %d", context->dialect);
  }
}

static void femit_reg(CodegenContext *context, enum Instruction inst, RegisterDescriptor reg) {
  if (inst == I_JMP || inst == I_CALL) {
    femit_indirect_branch(context, inst, reg);
    return;
  }
  if (inst == I_SAL || inst == I_SAR || inst == I_SHL || inst == I_SHR) {
    femit_reg_shift(context, inst, reg);
    return;
  }
  const char *mnemonic = instruction_mnemonic(context, inst);
  const char *source = register_name(reg);
  switch (context->dialect) {
    case CG_ASM_DIALECT_ATT:
      fprint(context->code, "    %s %%%s\n",
          mnemonic, source);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprint(context->code, "    %s %s\n",
          mnemonic, source);
      break;
    default: ICE("ERROR: femit_reg(): Unsupported dialect %d", context->dialect);
  }
}

static void femit_imm(CodegenContext *context, enum Instruction inst, int64_t immediate) {
  const char *mnemonic = instruction_mnemonic(context, inst);
  switch (context->dialect) {
    case CG_ASM_DIALECT_ATT:
      fprint(context->code, "    %s $%D\n",
          mnemonic, immediate);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprint(context->code, "    %s %D\n",
          mnemonic, immediate);
      break;
    default: ICE("ERROR: femit_imm(): Unsupported dialect %d", context->dialect);
  }
}

static void femit_name(CodegenContext *context, enum Instruction inst, const char *name) {
  ASSERT(name, "NAME must not be NULL.");
  const char *mnemonic = instruction_mnemonic(context, inst);
  switch (context->dialect) {
  case CG_ASM_DIALECT_ATT:
    fprint(context->code, "    %s (%s)\n",
           mnemonic, name);
    break;
  case CG_ASM_DIALECT_INTEL:
    fprint(context->code, "    %s %s\n",
           mnemonic, name);
    break;
  default: ICE("ERROR: femit(): Unsupported dialect %d for CALL/JMP instruction", context->dialect);
  }
}

static void femit
(CodegenContext *context,
 enum Instruction instruction,
 ...)
{
  va_list args;
  va_start(args, instruction);

  ASSERT(context);
  STATIC_ASSERT(I_COUNT == 26, "femit() must exhaustively handle all x86_64 instructions.");

  // TODO: Extract setcc and jcc to their own functions, get rid of varargs
  switch (instruction) {
    case I_SETCC: {
      enum ComparisonType comparison_type = va_arg(args, enum ComparisonType);
      RegisterDescriptor value_register = va_arg(args, RegisterDescriptor);

      const char *mnemonic = instruction_mnemonic(context, instruction);
      const char *value = register_name_8(value_register);

      switch (context->dialect) {
        case CG_ASM_DIALECT_ATT:
          fprint(context->code, "    %s%s %%%s\n",
              mnemonic,
              setcc_suffixes_x86_64[comparison_type], value);
          break;
        case CG_ASM_DIALECT_INTEL:
          fprint(context->code, "    %s%s %s\n",
              mnemonic,
              setcc_suffixes_x86_64[comparison_type], value);
          break;
        default: ICE("ERROR: femit(): Unsupported dialect %d", context->dialect);
      }
    } break;

    case I_JCC: {
      enum IndirectJumpType type = va_arg(args, enum IndirectJumpType);
      ASSERT(type < JUMP_TYPE_COUNT, "femit_direct_branch(): Invalid jump type %d", type);
      char *label = va_arg(args, char *);
      ASSERT(label, "JCC label must not be NULL.");

      const char *mnemonic = instruction_mnemonic(context, I_JCC);

      switch (context->dialect) {
        case CG_ASM_DIALECT_ATT:
        case CG_ASM_DIALECT_INTEL:
          fprint(context->code, "    %s%s %s\n",
              mnemonic, jump_type_names_x86_64[type], label);
          break;
        default: ICE("ERROR: femit_direct_branch(): Unsupported dialect %d", context->dialect);
      }
    } break;

    case I_RET:
    case I_CQO: {
      const char *mnemonic = instruction_mnemonic(context, instruction);
      fprint(context->code, "    %s\n", mnemonic);
    } break;

  default:
    ICE("Unhandled instruction in femit(): %d (%s)\n"
        "  Consider using femit_x() or femit_x_to_x()",
        instruction, instruction_mnemonic(context, instruction));
  }

  va_end(args);
}

/// Creates a context for the CG_FMT_x86_64_MSWIN architecture.
CodegenContext *codegen_context_x86_64_mswin_create() {
  RegisterPool pool;

  /// Create the registers.
  Register *registers = calloc(REG_COUNT, sizeof(Register));

  /// Link to MSDN documentation (surely will fall away, but it's been Internet Archive'd).
  /// https://docs.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-170#callercallee-saved-registers
  /// https://web.archive.org/web/20220916164241/https://docs.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-170
  /// "The x64 ABI considers the registers RAX, RCX, RDX, R8, R9, R10, R11, and XMM0-XMM5 volatile."
  /// "The x64 ABI considers registers RBX, RBP, RDI, RSI, RSP, R12, R13, R14, R15, and XMM6-XMM15 nonvolatile."
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

  caller_saved_register_count = MSWIN_CALLER_SAVED_REGISTER_COUNT;
  caller_saved_registers = mswin_caller_saved_registers;
  argument_register_count = MSWIN_ARGUMENT_REGISTER_COUNT;
  argument_registers = mswin_argument_registers;

  CodegenContext *cg_ctx = calloc(1,sizeof(CodegenContext));
  cg_ctx->format = CG_FMT_x86_64_GAS;
  cg_ctx->call_convention = CG_CALL_CONV_MSWIN;
  cg_ctx->dialect = CG_ASM_DIALECT_ATT;
  cg_ctx->register_pool = pool;
  return cg_ctx;
}

/// Creates a context for the x86_64/CG_CALL_CONV_LINUX.
CodegenContext *codegen_context_x86_64_linux_create() {
  RegisterPool pool;

  /// Create the registers.
  Register *registers = calloc(REG_COUNT, sizeof(Register));

  /// Registers %rbp, %rbx and %r12 through %r15 “belong” to the calling function
  /// and the called function is required to preserve their values.
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

  caller_saved_register_count = LINUX_CALLER_SAVED_REGISTER_COUNT;
  caller_saved_registers = linux_caller_saved_registers;
  argument_register_count = LINUX_ARGUMENT_REGISTER_COUNT;
  argument_registers = linux_argument_registers;

  CodegenContext *cg_ctx = calloc(1,sizeof(CodegenContext));

  // Shallow-copy state from the parent.
  cg_ctx->format = CG_FMT_x86_64_GAS;
  cg_ctx->call_convention = CG_CALL_CONV_LINUX;
  cg_ctx->dialect = CG_ASM_DIALECT_ATT;
  cg_ctx->register_pool = pool;
  return cg_ctx;
}

/// Free a context created by codegen_context_x86_64_mswin_create.
void codegen_context_x86_64_mswin_free(CodegenContext *ctx) {
  free(ctx->register_pool.registers);
  free(ctx->register_pool.scratch_registers);
}

void codegen_context_x86_64_linux_free(CodegenContext *ctx) {
  free(ctx->register_pool.registers);
  free(ctx->register_pool.scratch_registers);
}

/// Generate a comparison between two registers.
static RegisterDescriptor codegen_comparison
(CodegenContext *cg_context,
 enum ComparisonType type,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs,
 RegisterDescriptor result,
 enum RegSize size)
{
  ASSERT(type < COMPARE_COUNT, "Invalid comparison type");

  // Zero out result register.

  // Perform the comparison.
  femit_reg_to_reg(cg_context, I_CMP, rhs, size, lhs, size);
  // IF YOU REPLACE THIS WITH A XOR IT WILL BREAK HORRIBLY
  // We use MOV because it doesn't set flags.
  femit_imm_to_reg(cg_context, I_MOV, 0, result, r32);
  femit(cg_context, I_SETCC, type, result);

  return result;
}

enum StackFrameKind {
  FRAME_FULL,
  FRAME_MINIMAL,
  FRAME_NONE,
};

static enum StackFrameKind stack_frame_kind(CodegenContext *context, IRFunction *f) {
  (void) context;

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
  enum StackFrameKind frame_kind = stack_frame_kind(cg_context, f);
  switch (frame_kind) {
    case FRAME_NONE: break;

    case FRAME_FULL: {
      size_t locals_offset = f->locals_total_size;

      femit_reg(cg_context, I_PUSH, REG_RBP);
      femit_reg_to_reg(cg_context, I_MOV, REG_RSP, r64, REG_RBP, r64);
      switch (cg_context->call_convention) {
        ///> Even if the called function has fewer than 4 parameters, these 4
        ///> stack locations are effectively owned by the called function, and
        ///> may be used by the called function for other purposes besides
        ///> saving parameter register values.
        ///  – https://learn.microsoft.com/en-us/cpp/build/stack-usage?view=msvc-170
        case CG_CALL_CONV_MSWIN:
          locals_offset += 4 * 8 + 8;
          break;
        case CG_CALL_CONV_LINUX: break;
        default: ICE("Unknown calling convention");
      }
      femit_imm_to_reg(cg_context, I_SUB, (i64) locals_offset, REG_RSP, r64);
    } break;

    case FRAME_MINIMAL: {
      switch (cg_context->call_convention) {
        /// See comment above.
        case CG_CALL_CONV_MSWIN:
          femit_imm_to_reg(cg_context, I_SUB, 4 * 8 + 8, REG_RSP, r64);
          break;
        case CG_CALL_CONV_LINUX:
          femit_reg(cg_context, I_PUSH, REG_RBP);
          break;
        default: ICE("Unknown calling convention");
      }
    }
  }
}

/// Emit the function epilogue.
static void codegen_epilogue(CodegenContext *cg_context, IRFunction *f) {
  enum StackFrameKind frame_kind = stack_frame_kind(cg_context, f);
  switch (frame_kind) {
    case FRAME_NONE: break;

    case FRAME_FULL: {
      femit_reg_to_reg(cg_context, I_MOV, REG_RBP, r64, REG_RSP, r64);
      femit_reg(cg_context, I_POP, REG_RBP);
    } break;

    case FRAME_MINIMAL: {
      switch (cg_context->call_convention) {
        /// See comment above.
        case CG_CALL_CONV_MSWIN:
          femit_imm_to_reg(cg_context, I_ADD, 4 * 8 + 8, REG_RSP, r64);
          break;
        case CG_CALL_CONV_LINUX:
          femit_reg(cg_context, I_POP, REG_RBP);
          break;
        default: ICE("Unknown calling convention");
      }
    }
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
      if (inst->imm <= UINT32_MAX) {
        femit_imm_to_reg(context, I_MOV, (i64) inst->imm, inst->result, r32);
      } else if (inst->imm <= UINT64_MAX) {
        femit_imm_to_reg(context, I_MOV, (i64) inst->imm, inst->result, r64);
      } else {
        ICE("Unsupported integer literal immediate on x86_64 (out of range)");
      }
    } else {
      if (type_sizeof(inst->type) <= 4) {
        femit_imm_to_reg(context, I_MOV, (i64) inst->imm, inst->result, r32);
      } else if (type_sizeof(inst->type) <= 8) {
        femit_imm_to_reg(context, I_MOV, (i64) inst->imm, inst->result, r64);
      } else {
        ICE("Unsupported immediate size on x86_64: %Z", type_sizeof(inst->type));
      }
    }
    break;
  case IR_NOT: {
    femit_reg(context, I_NOT, inst->operand->result);
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->operand->type));
    femit_reg_to_reg(context, I_MOV, inst->operand->result, size, inst->result, size);
  } break;
  case IR_ZERO_EXTEND: {
    usz operand_byte_size = type_sizeof(inst->operand->type);
    usz result_byte_size = type_sizeof(inst->type);
    ASSERT (result_byte_size > operand_byte_size, "Zero extension result must be larger than operand");

    enum RegSize operand_size = regsize_from_bytes(operand_byte_size);
    enum RegSize result_size = regsize_from_bytes(result_byte_size);

    femit_reg_to_reg(context, I_MOVZX, inst->operand->result, operand_size, inst->result, result_size);
  } break;
  case IR_SIGN_EXTEND: {
    usz operand_byte_size = type_sizeof(inst->operand->type);
    usz result_byte_size = type_sizeof(inst->type);
    ASSERT (result_byte_size > operand_byte_size, "Sign extension result must be larger than operand");

    enum RegSize operand_size = regsize_from_bytes(operand_byte_size);
    enum RegSize result_size = regsize_from_bytes(result_byte_size);

    femit_reg_to_reg(context, I_MOVSX, inst->operand->result, operand_size, inst->result, result_size);
  } break;
  case IR_TRUNCATE: {
    usz operand_byte_size = type_sizeof(inst->operand->type);
    usz result_byte_size = type_sizeof(inst->type);
    ASSERT (result_byte_size < operand_byte_size, "Truncation result must be smaller than operand");

    enum RegSize result_size = regsize_from_bytes(result_byte_size);

    femit_reg_to_reg(context, I_MOV, inst->operand->result, r64, inst->result, r64);
    switch (result_size) {
    case r32:
      femit_imm_to_reg(context, I_AND, 0xffffffff, inst->result, r32);
      break;
    case r16:
      femit_imm_to_reg(context, I_AND, 0xffff, inst->result, r32);
      break;
    case r8:
      femit_imm_to_reg(context, I_AND, 0xff, inst->result, r32);
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

    femit_reg_to_reg(context, I_MOV, inst->operand->result, operand_size, inst->result, result_size);
  } break;
  case IR_CALL: {
    // Save caller saved registers used in caller function.
    ASSERT(inst->parent_block, "call instruction null block");
    ASSERT(inst->parent_block->function, "block has null function");

    // Tail call.
    if (inst->call.tail_call) {
      // Restore the frame pointer if we have one.
      codegen_epilogue(context, inst->parent_block->function);
      if (inst->call.is_indirect) femit_reg(context, I_JMP, inst->call.callee_instruction->result);
      else femit_name(context, I_JMP, inst->call.callee_function->name.data);
      if (inst->parent_block) inst->parent_block->done = true;
      break;
    }

    size_t func_regs = inst->parent_block->function->registers_in_use;
    size_t regs_pushed_count = 0;

    // Save return register.
    // FIXME: Use calling convention for return register.
    if (func_regs & REG_RAX) {
      femit_reg(context, I_PUSH, REG_RAX);
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
      femit_imm_to_reg(context, I_SUB, 8, REG_RSP, r64);

    for (Register i = REG_RAX + 1; i < sizeof(func_regs) * 8; ++i)
      if (func_regs & (1 << i) && is_caller_saved(i))
        // TODO: Don't push registers that are used for arguments.
        femit_reg(context, I_PUSH, i);

    // Shadow stack
    if (context->call_convention == CG_CALL_CONV_MSWIN)
      femit_imm_to_reg(context, I_SUB, 32, REG_RSP, r64);

    if (inst->call.is_indirect) femit_reg(context, I_CALL, inst->call.callee_instruction->result);
    else femit_name(context, I_CALL, inst->call.callee_function->name.data);

    // Restore shadow stack
    if (context->call_convention == CG_CALL_CONV_MSWIN)
      femit_imm_to_reg(context, I_ADD, 32, REG_RSP, r64);

    // Restore caller saved registers used in called function.
    for (Register i = sizeof(func_regs) * 8 - 1; i > REG_RAX; --i)
      if (func_regs & (1 << i) && is_caller_saved(i))
        femit_reg(context, I_POP, i);

    // Restore stack pointer from stack alignment, if necessary.
    if (regs_pushed_count & 0b1)
      femit_imm_to_reg(context, I_ADD, 8, REG_RSP, r64);

    femit_reg_to_reg(context, I_MOV, REG_RAX, r64, inst->result, r64);

    // Restore return register.
    // FIXME: Use calling convention for return register.
    if (func_regs & REG_RAX)
      femit_reg(context, I_POP, REG_RAX);

  } break;

  case IR_RETURN:
    // Restore callee-saved registers used in the function.
    for (Register i = sizeof(inst->parent_block->function->registers_in_use) * 8 - 1; i > 0; --i) {
      if (inst->parent_block->function->registers_in_use & ((size_t)1 << i) && is_callee_saved(i)) {
        femit_reg(context, I_POP, i);
      }
    }
    codegen_epilogue(context, inst->parent_block->function);
    femit(context, I_RET);
    if (optimise && inst->parent_block) inst->parent_block->done = true;
    break;

  case IR_BRANCH:
    /// Only emit a jump if the target isn’t the next block.
    if (!optimise || (inst->parent_block
          && inst->destination_block != inst->parent_block->next && !inst->parent_block->done)) {
      femit_name(context, I_JMP, inst->destination_block->name.data);
    }
    if (optimise && inst->parent_block) inst->parent_block->done = true;
    break;
  case IR_BRANCH_CONDITIONAL: {
    IRBranchConditional *branch = &inst->cond_br;

    femit_reg_to_reg(context, I_TEST, branch->condition->result, r64, branch->condition->result, r64);

    /// If either target is the next block, arrange the jumps in such a way
    /// that we can save one and simply fallthrough to the next block.
    if (optimise && branch->then == inst->parent_block->next) {
      femit(context, I_JCC, JUMP_TYPE_Z, branch->else_->name.data);
    } else if (optimise && branch->else_ == inst->parent_block->next) {
      femit(context, I_JCC, JUMP_TYPE_NZ, branch->then->name.data);
    } else {
      femit(context, I_JCC, JUMP_TYPE_Z, branch->else_->name.data);
      femit_name(context, I_JMP, branch->then->name.data);
    }

    if (optimise && inst->parent_block) inst->parent_block->done = true;
  } break;
  case IR_LE: {
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    codegen_comparison(context, COMPARE_LE, inst->lhs->result, inst->rhs->result, inst->result, size);
  } break;
  case IR_LT: {
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    codegen_comparison(context, COMPARE_LT, inst->lhs->result, inst->rhs->result, inst->result, size);
  } break;
  case IR_GE: {
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    codegen_comparison(context, COMPARE_GE, inst->lhs->result, inst->rhs->result, inst->result, size);
  } break;
  case IR_GT: {
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    codegen_comparison(context, COMPARE_GT, inst->lhs->result, inst->rhs->result, inst->result, size);
  } break;
  case IR_EQ: {
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    codegen_comparison(context, COMPARE_EQ, inst->lhs->result, inst->rhs->result, inst->result, size);
  } break;
  case IR_NE: {
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    codegen_comparison(context, COMPARE_NE, inst->lhs->result, inst->rhs->result, inst->result, size);
  } break;
  case IR_ADD: {
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    femit_reg_to_reg(context, I_ADD, inst->lhs->result, size, inst->rhs->result, size);
    femit_reg_to_reg(context, I_MOV, inst->rhs->result, size, inst->result, size);
  } break;
  case IR_SUB: {
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    femit_reg_to_reg(context, I_SUB, inst->rhs->result, size, inst->lhs->result, size);
    femit_reg_to_reg(context, I_MOV, inst->lhs->result, size, inst->result, size);
  } break;
  case IR_MUL: {
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    femit_reg_to_reg(context, I_IMUL, inst->lhs->result, size, inst->rhs->result, size);
    femit_reg_to_reg(context, I_MOV, inst->rhs->result, size, inst->result, size);
  } break;
  case IR_DIV: {
    ASSERT(inst->rhs->result != REG_RAX,
           "Register allocation must not allocate RAX to divisor.");
    enum RegSize lhs_size = regsize_from_bytes(type_sizeof(inst->lhs->type));
    femit_reg_to_reg(context, I_MOV, inst->lhs->result, lhs_size, REG_RAX, r64);
    femit(context, I_CQO);
    femit_reg(context, I_IDIV, inst->rhs->result);
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    femit_reg_to_reg(context, I_MOV, REG_RAX, size, inst->result, size);
  } break;
  case IR_MOD: {
    ASSERT(inst->rhs->result != REG_RAX,
           "Register allocation must not allocate RAX to divisor.");
    enum RegSize rhs_size = regsize_from_bytes(type_sizeof(inst->rhs->type));
    femit_reg_to_reg(context, I_MOV, inst->rhs->result, rhs_size, REG_RAX, r64);
    femit(context, I_CQO);
    femit_reg(context, I_IDIV, inst->rhs->result);
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    femit_reg_to_reg(context, I_MOV, REG_RDX, size, inst->result, size);
  } break;
  case IR_SHL: {
    ASSERT(inst->lhs->result != REG_RCX,
           "Register allocation must not allocate RCX to result of lhs of shift.");
    enum RegSize rhs_size = regsize_from_bytes(type_sizeof(inst->rhs->type));
    femit_reg_to_reg(context, I_MOV, inst->rhs->result, rhs_size, REG_RCX, r64);
    femit_reg(context, I_SHL, inst->lhs->result);
    enum RegSize lhs_size = regsize_from_bytes(type_sizeof(inst->lhs->type));
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    femit_reg_to_reg(context, I_MOV, inst->lhs->result, lhs_size, inst->result, size);
  } break;
  case IR_SHR: {
    enum RegSize rhs_size = regsize_from_bytes(type_sizeof(inst->rhs->type));
    femit_reg_to_reg(context, I_MOV, inst->rhs->result, rhs_size, REG_RCX, r64);
    femit_reg(context, I_SHR, inst->lhs->result);
    enum RegSize lhs_size = regsize_from_bytes(type_sizeof(inst->lhs->type));
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    femit_reg_to_reg(context, I_MOV, inst->lhs->result, lhs_size, inst->result, size);
  } break;
  case IR_SAR: {
    enum RegSize rhs_size = regsize_from_bytes(type_sizeof(inst->rhs->type));
    femit_reg_to_reg(context, I_MOV, inst->rhs->result, rhs_size, REG_RCX, r64);
    femit_reg(context, I_SAR, inst->lhs->result);
    enum RegSize lhs_size = regsize_from_bytes(type_sizeof(inst->lhs->type));
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    femit_reg_to_reg(context, I_MOV, inst->lhs->result, lhs_size, inst->result, size);
  } break;
  case IR_AND: {
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    enum RegSize lhs_size = regsize_from_bytes(type_sizeof(inst->lhs->type));
    enum RegSize rhs_size = regsize_from_bytes(type_sizeof(inst->rhs->type));
    femit_reg_to_reg(context, I_AND, inst->lhs->result, lhs_size, inst->rhs->result, size);
    femit_reg_to_reg(context, I_MOV, inst->rhs->result, rhs_size, inst->result, size);
  } break;
  case IR_OR: {
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    enum RegSize lhs_size = regsize_from_bytes(type_sizeof(inst->lhs->type));
    enum RegSize rhs_size = regsize_from_bytes(type_sizeof(inst->rhs->type));
    femit_reg_to_reg(context, I_OR, inst->lhs->result, lhs_size, inst->rhs->result, size);
    femit_reg_to_reg(context, I_MOV, inst->rhs->result, rhs_size, inst->result, size);
  } break;

  case IR_LOAD: {
      enum RegSize size = -1u;
    // TODO: Handle size of type and stuff
    /// Load from a static variable.
    if (inst->operand->kind == IR_STATIC_REF) {
      // TODO: Should this array to pointer decay happen here? Or higher up in codegen?
      if (inst->operand->type->kind == TYPE_ARRAY || inst->operand->type->pointer.to->kind == TYPE_ARRAY)
        size = regsize_from_bytes(type_sizeof(t_void_ptr));
      else size = regsize_from_bytes(type_sizeof(inst->operand->type));
      if (size == r8 || size == r16) femit_reg_to_reg(context, I_XOR, inst->result, r32, inst->result, r32);
      if (inst->operand->type->kind == TYPE_ARRAY || inst->operand->type->pointer.to->kind == TYPE_ARRAY)
        femit_name_to_reg(context, I_LEA, REG_RIP, inst->operand->static_ref->name.data, inst->result, size);
      else
        femit_name_to_reg(context, I_MOV, REG_RIP, inst->operand->static_ref->name.data, inst->result, size);
    }

    /// Load from a local.
    else if (inst->operand->kind == IR_ALLOCA) {
      // TODO: Should this array to pointer decay happen here? Or higher up in codegen?
      if (inst->operand->type->kind == TYPE_ARRAY || inst->operand->type->pointer.to->kind == TYPE_ARRAY)
        size = regsize_from_bytes(type_sizeof(t_void_ptr));
      else size = regsize_from_bytes(inst->operand->alloca.size);
      if (size == r8 || size == r16) femit_reg_to_reg(context, I_XOR, inst->result, r32, inst->result, r32);
      if (inst->operand->type->kind == TYPE_ARRAY || inst->operand->type->pointer.to->kind == TYPE_ARRAY)
        femit_mem_to_reg(context, I_LEA, REG_RBP, - (i64)inst->operand->alloca.offset, inst->result, size);
      else
        femit_mem_to_reg(context, I_MOV, REG_RBP, - (i64)inst->operand->alloca.offset, inst->result, size);
    }

    /// Load from a pointer
    else {
      // TODO: Should this array to pointer decay happen here? Or higher up in codegen?
      if (inst->operand->type->kind == TYPE_ARRAY) size = regsize_from_bytes(type_sizeof(t_void_ptr));
      // TODO: We are "supposed" to be loading sizeof pointed to type
      // here, but that causes segfaults when handling arrays.
      else size = regsize_from_bytes(type_sizeof(inst->operand->type));
      if (size == r8 || size == r16) femit_reg_to_reg(context, I_XOR, inst->result, r32, inst->result, r32);
      if (inst->operand->type->kind == TYPE_ARRAY)
        femit_mem_to_reg(context, I_LEA, inst->operand->result, 0, inst->result, size);
      else
        femit_mem_to_reg(context, I_MOV, inst->operand->result, 0, inst->result, size);
    }
  } break;

  case IR_STORE: {
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->store.value->type));
    /// Store to a static variable.
    if (inst->store.addr->kind == IR_STATIC_REF) {
      femit_reg_to_name(context, I_MOV, inst->store.value->result, size, REG_RIP, inst->store.addr->static_ref->name.data);
    }

    /// Store to a local.
    else if (inst->store.addr->kind == IR_ALLOCA) {
      femit_reg_to_mem(context, I_MOV, inst->store.value->result, size, REG_RBP, - (i64)inst->store.addr->alloca.offset);
    }

    /// Store to a pointer.
    else {
      femit_reg_to_mem(context, I_MOV, inst->store.value->result, size, inst->store.addr->result, 0);
    }
  } break;

  case IR_STATIC_REF: {
    if (inst->result) femit_name_to_reg(context, I_LEA, REG_RIP, inst->static_ref->name.data, inst->result, r64);
  } break;
  case IR_FUNC_REF: {
    if (inst->result) femit_name_to_reg(context, I_LEA, REG_RIP, inst->function_ref->name.data, inst->result, r64);
  } break;
  case IR_ALLOCA: {
    femit_mem_to_reg(context, I_LEA, REG_RBP, - (i64)inst->alloca.offset, inst->result, r64);
  } break;

  default:
    ir_femit_instruction(stderr, inst);
    TODO("Handle IRtype %d\n", inst->kind);
    break;
  }
}

void emit_block(CodegenContext *context, IRBlock *block) {
  /// Emit block label if it is used.
  if (block->name.size) {
    fprint(context->code,
            "%s:\n",
            block->name.data);
  }

  list_foreach (IRInstruction*, instruction, block->instructions) {
    emit_instruction(context, instruction);
  }
}

void emit_function(CodegenContext *context, IRFunction *function) {
  // Generate function entry.
  fprint(context->code,
          "\n%s:\n",
          function->name.data);
  codegen_prologue(context, function);
  // Save all callee-saved registers in use in the function.
  for (Register i = 1; i < sizeof(function->registers_in_use) * 8; ++i) {
    if ((size_t)function->registers_in_use & ((size_t)1 << i) && is_callee_saved(i)) {
      femit_reg(context, I_PUSH, i);
    }
  }
  list_foreach (IRBlock*, block, function->blocks) { emit_block(context, block); }
  // NOTE: Epilogue is generated by `return` instruction.
}

void emit_entry(CodegenContext *context) {
  fprint(context->code,
          "%s"
          ".section .text\n",
          context->dialect == CG_ASM_DIALECT_INTEL ? ".intel_syntax noprefix\n" : "");

  fprint(context->code, "\n");
  foreach_ptr (IRFunction*, function, context->functions) {
    if (!function->attr_global) continue;
    fprint(context->code, ".global %S\n", function->name);
  }
}

typedef enum Clobbers {
  CLOBBERS_NEITHER,
  CLOBBERS_REFERENCE,
  CLOBBERS_LEFT,
  CLOBBERS_RIGHT,
  CLOBBERS_BOTH,
  CLOBBERS_OTHER,
} Clobbers;

Clobbers does_clobber(IRInstruction *instruction) {
  STATIC_ASSERT(IR_COUNT == 38, "Exhaustive handling of IR instruction types that correspond to two-address instructions in x86_64.");
  switch (instruction->kind) {
  case IR_ADD:
  case IR_DIV:
  case IR_MUL:
  case IR_MOD:
  case IR_AND:
  case IR_OR:
    return CLOBBERS_RIGHT;

  case IR_SUB:
  case IR_SHL:
  case IR_SHR:
  case IR_SAR:
    return CLOBBERS_LEFT;

  case IR_NOT:
    return CLOBBERS_REFERENCE;

  default:
    break;
  }
  return CLOBBERS_NEITHER;
}

static usz emit_memcpy_impl(CodegenContext *context, IRInstruction *to, IRInstruction *from, usz byte_size, usz iter_amount, IRInstruction *insert_before_this) {
  for (; byte_size >= iter_amount; byte_size -= iter_amount) {
    // Load iter_amount bytes from "from" address, and store iter_amount bytes into "to" address.
    INSTRUCTION(load, IR_LOAD);
    { // Generate load of element...
      load->operand = from;

      Type *t = type_canonical(from->type);
      if (!(t && type_is_pointer(t))) {
        //print("from type: %T\n", from->type);
        ir_femit_instruction(stdout, from);
        if (t) ICE("Can not emit IR_LOAD from type %T as it is not a pointer", t);
        else ICE("Can not emit IR_LOAD to NULL canonical type!");
      }
      if (type_is_pointer(t)) load->type = t->pointer.to;
      else load->type = t;

      mark_used(from, load);
    }
    insert_instruction_before(load, insert_before_this);

    INSTRUCTION(store, IR_STORE);
    { // Store loaded element into local stack array...
      store->store.addr = to;
      store->store.value = load;
      mark_used(to, store);
      mark_used(load, store);
    }
    insert_instruction_before(store, insert_before_this);

    if (byte_size - iter_amount < iter_amount || byte_size - iter_amount > byte_size) {
      // Do this iteration, then break.
      byte_size -= iter_amount;
      break;
    }

    { // Iterate "from" and "to" addresses by iter_amount bytes.

      // Generate an immediate corresponding to the byte size of this member
      INSTRUCTION(byte_size_immediate, IR_IMMEDIATE);
      byte_size_immediate->type = t_integer_literal;
      byte_size_immediate->imm = iter_amount;
      insert_instruction_before(byte_size_immediate, insert_before_this);

      INSTRUCTION(add, IR_ADD);
      add->type = from->type;
      set_pair_and_mark(add, from, byte_size_immediate);
      insert_instruction_before(add, insert_before_this);
      from = add;

      INSTRUCTION(dest_add, IR_ADD);
      dest_add->type = to->type;
      set_pair_and_mark(dest_add, to, byte_size_immediate);
      insert_instruction_before(dest_add, insert_before_this);
      to = dest_add;
    }
  }
  return byte_size;
}

static void emit_memcpy(CodegenContext *context, IRInstruction *to_, IRInstruction *from_, usz byte_size, IRInstruction *insert_before_this) {
  // Create two copies, one of each address: "from" and "to".
  // Cache address we are loading from.
  IRInstruction *from = ir_copy(context, from_);
  insert_instruction_before(from, insert_before_this);
  // Switch type to reflect loading 8 bytes.
  from->type = ast_make_type_pointer(context->ast, t_integer->source_location, t_integer);
  // Cache address we are storing to.
  IRInstruction *to = ir_copy(context, to_);
  insert_instruction_before(to, insert_before_this);
  // Switch type to reflect storing 8 bytes.
  to->type = ast_make_type_pointer(context->ast, t_integer->source_location, t_integer);

  if ((byte_size = emit_memcpy_impl(context, to, from, byte_size, 8, insert_before_this))) {
    // Switch type to reflect storing 1 byte.
    from->type = ast_make_type_pointer(context->ast, t_byte->source_location, t_byte);
    to->type = ast_make_type_pointer(context->ast, t_byte->source_location, t_byte);
    emit_memcpy_impl(context, to, from, byte_size, 1, insert_before_this);
  }
}

/// Given an `IR_LOAD` instruction, return true iff the load has been altered.
/// Otherwise, return false.
static bool lower_load(CodegenContext *context, IRInstruction *instruction) {
  ASSERT(context, "x86_64: lower_load(): Context must not be NULL");
  ASSERT(instruction, "x86_64: lower_load(): IRInstruction must not be NULL");
  Type *type = type_canonical(instruction->type);
  usz byte_size = type_sizeof(type);
  if (byte_size > max_register_size) {
    // TODO: Just copy the whole thing in one register if it is <= the max_register_size

    // Create space for a copy on the stack.
    INSTRUCTION(alloca, IR_ALLOCA);
    alloca->alloca.size = byte_size;
    alloca->type = ast_make_type_pointer(context->ast, type->source_location, type);
    ir_set_backend_flag(alloca, STORE_UNDERLYING);

    // Replace `load` with `alloca`.
    ir_replace_uses(instruction, alloca);
    insert_instruction_before(alloca, instruction);

    emit_memcpy(context, alloca, instruction->operand, type_sizeof(instruction->type), instruction);

    ir_remove(instruction);
    return true;
  }
  return false;
}

/// Return true iff the given store instruction has been altered.
/// Otherwise, return false.
static bool lower_store(CodegenContext *context, IRInstruction *instruction) {
  // If the value is an `alloca`, we are copying from a local
  // variable to somewhere. This means we need to emit the equivalent
  // of `memcpy(address, value, sizeof(*value));`
  if (ir_get_backend_flag(instruction->store.value, STORE_UNDERLYING)) {
    // `inst->store.value->result` register contains an address we should store from.
    // `inst->store.addr->result` register contains an address we should store to.

    ASSERT(instruction->store.value->type->kind == TYPE_POINTER,
           "ALLOCA must be of pointer type to store it properly... What did you do?");
    usz byte_size = type_sizeof(instruction->store.value->type->pointer.to);
    emit_memcpy(context, instruction->store.addr, instruction->store.value, byte_size, instruction);

    ir_remove(instruction);
    return true;
  }
  return false;
}

static void lower(CodegenContext *context) {
  ASSERT(argument_registers, "arch_x86_64 backend can not lower IR when argument registers have not been initialized.");
  FOREACH_INSTRUCTION (context) {
    switch (instruction->kind) {
    case IR_LOAD: {
      lower_load(context, instruction);
    } break;
    case IR_STORE: {
      lower_store(context, instruction);
    } break;

    case IR_ALLOCA: {
      // Worry about stack alignment
      if (instruction->alloca.size < 8)
        instruction->alloca.size = 8;
    } break;

    case IR_CALL: {
      usz idx = 0;
      foreach_ptr (IRInstruction *, argument, instruction->call.arguments) {
        if (idx >= argument_register_count) break;
        switch (context->call_convention) {
        case CG_CALL_CONV_LINUX: {
          TODO("x86_64 backend doesn't yet support SYSV recursive algorithm stuff, sorry.");
        } break;
        case CG_CALL_CONV_MSWIN: {
          Type *type = type_canonical(argument->type);
          if ((type->kind == TYPE_STRUCT || type->kind == TYPE_ARRAY) && type_sizeof(type) > 8) {
            INSTRUCTION(alloca, IR_ALLOCA);
            alloca->alloca.size = type_sizeof(type);
            alloca->type = ast_make_type_pointer(context->ast, argument->type->source_location, argument->type);
            insert_instruction_before(alloca, instruction);

            INSTRUCTION(store, IR_STORE);

            store->store.addr = alloca;
            mark_used(alloca, store);

            store->store.value = argument;
            mark_used(argument, store);

            insert_instruction_before(store, instruction);
          }
        } break;
        }
        ++idx;
      }

      size_t argcount = instruction->call.arguments.size;
      if (argcount >= argument_register_count) {
        switch (context->call_convention) {
        case CG_CALL_CONV_LINUX: {
          TODO("x86_64 backend doesn't yet support SYSV stack arguments, sorry.");
        } break;
        case CG_CALL_CONV_MSWIN: {
          usz i = instruction->call.arguments.size - 1;
          foreach_ptr_rev (IRInstruction *, argument, instruction->call.arguments) {
            if (i < argument_register_count) break;

            INSTRUCTION(alloca, IR_ALLOCA);
            alloca->alloca.size = type_sizeof(argument->type);
            alloca->type = ast_make_type_pointer(context->ast, argument->type->source_location, argument->type);
            insert_instruction_before(alloca, instruction);

            INSTRUCTION(store, IR_STORE);

            store->store.addr = alloca;
            mark_used(alloca, store);

            store->store.value = argument;
            mark_used(argument, store);

            insert_instruction_before(store, instruction);

            *argument_ptr = alloca;

            --i;
          }
        } break;
        }
      }
    } break;

    case IR_PARAMETER: {
      switch (context->call_convention) {

      case CG_CALL_CONV_LINUX: {
        if (instruction->type->kind == TYPE_ARRAY || instruction->type->kind == TYPE_STRUCT
            || type_sizeof(instruction->type) > 8 || instruction->imm >= argument_register_count) {
          TODO("x86_64 backend does not yet support passing complex parameters with sysv ABI, sorry.");
        }
        instruction->kind = IR_REGISTER;
        instruction->result = argument_registers[instruction->imm];
      } break;

      case CG_CALL_CONV_MSWIN: {
        Type *type = type_canonical(instruction->type);
        // NOTE: Arrays and strings in Intercept are passed like
        // structs in C, so this doesn't apply to *Intercept*
        // arrays/strings.
        // __m128 types, arrays, and strings are never passed by immediate value.
        // Structs and unions of size 8, 16, 32, or 64 bits, and __m64
        // types, are passed as if they were integers of the same size.
        if (instruction->imm >= argument_register_count || type_sizeof(type) > 8) {
          // Lower type to a pointer, because that's how the calls have
          // been altered as well.
          INSTRUCTION(rbp, IR_REGISTER);
          rbp->result = REG_RBP;
          rbp->type = t_integer;
          insert_instruction_before(rbp, instruction);

          usz parameter_index = instruction->imm;

          INSTRUCTION(offset, IR_IMMEDIATE);
          offset->type = t_integer;
          usz i = instruction->parent_block->function->type->function.parameters.size - 1;
          foreach_rev (Parameter, param, instruction->parent_block->function->type->function.parameters) {
            if (i == parameter_index) break;
            offset->imm += type_sizeof(param->type);
            --i;
          }
          insert_instruction_before(offset, instruction);

          instruction->kind = IR_ADD;
          instruction->lhs = rbp;
          instruction->rhs = offset;
          instruction->type = ast_make_type_pointer(context->ast, instruction->type->source_location, instruction->type);
        } else {
          instruction->kind = IR_REGISTER;
          instruction->result = argument_registers[instruction->imm];
        }
      } break;

      default: ICE("Unhandled call convention for parameter lowering.");
      }
    } break;

    case IR_BITCAST: {
      instruction->kind = IR_COPY;
    } break;

    default:
      break;
    }
  }

  FOREACH_INSTRUCTION (context) {
    Clobbers status;
    if ((status = does_clobber(instruction))) {
      switch (status) {
      case CLOBBERS_BOTH:
        TODO("Handle clobbering of both registers by a two address instruction.");
      case CLOBBERS_REFERENCE: {
        // TODO: Reduce code duplication.
        IRInstruction *copy = ir_copy(context, instruction->operand);
        ir_remove_use(instruction->operand, instruction);
        mark_used(copy, instruction);
        insert_instruction_before(copy, instruction);
        instruction->operand = copy;
      } break;
      case CLOBBERS_LEFT: {
        IRInstruction *copy = ir_copy(context, instruction->lhs);
        ir_remove_use(instruction->lhs, instruction);
        mark_used(copy, instruction);
        insert_instruction_before(copy, instruction);
        instruction->lhs = copy;
      } break;
      case CLOBBERS_RIGHT: {
        IRInstruction *copy = ir_copy(context, instruction->rhs);
        ir_remove_use(instruction->rhs, instruction);
        mark_used(copy, instruction);
        insert_instruction_before(copy, instruction);
        instruction->rhs = copy;
      } break;
      default:
      case CLOBBERS_NEITHER:
        break;
      }
    }
  }
}

void calculate_stack_offsets(CodegenContext *context) {
  foreach_ptr (IRFunction*, function, context->functions) {
    size_t offset = 0;
    list_foreach (IRBlock *, block, function->blocks) {
      list_foreach (IRInstruction *, instruction, block->instructions) {
        switch (instruction->kind) {
        case IR_ALLOCA:
          offset += instruction->alloca.size;
          instruction->alloca.offset = offset;
          break;
        default:
          break;
        }
      }
    }
    function->locals_total_size = offset;
  }
}

static size_t interfering_regs(IRInstruction *instruction) {
  ASSERT(instruction, "Can not get register interference of NULL instruction.");
  size_t mask = 0;
  switch(instruction->kind) {
  case IR_SHL:
  case IR_SHR:
  case IR_SAR:
    mask |= (1 << REG_RCX);
    break;
  case IR_DIV:
  case IR_MOD:
    mask |= (1 << REG_RAX);
    mask |= (1 << REG_RDX);
    break;
  case IR_CALL: // FIXME: This seems specific to calling convention...
    mask |= (1 << REG_RAX);
  default:
    break;
  }
  // Shift mask right because it doesn't include REG_NONE
  return mask >> 1;
}

// TODO: This should probably be used by every backend, so it should
// move "up" somewhere.
static void mangle_type_to(string_buffer *buf, Type *t) {
  ASSERT(t);
  switch (t->kind) {
    default: UNREACHABLE();

    case TYPE_STRUCT:
      if (t->structure.decl->struct_decl->name.size)
        format_to(buf, "%Z%S", t->structure.decl->struct_decl->name.size, t->structure.decl->struct_decl->name);
      else {
        static usz struct_count = 0;
        format_to(buf, "%Z%Z", number_width(struct_count), struct_count);
        ++struct_count;
      }
      break;

    case TYPE_PRIMITIVE:
      format_to(buf, "%Z%S", t->primitive.name.size, t->primitive.name);
      break;

    case TYPE_NAMED:
      if (!t->named->val.type) format_to(buf, "%Z%S", t->named->name.size, t->named->name);
      else mangle_type_to(buf, t->named->val.type);
      break;

    case TYPE_POINTER:
      format_to(buf, "P");
      mangle_type_to(buf, t->pointer.to);
      break;

    case TYPE_ARRAY:
      format_to(buf, "A%ZE", t->array.size);
      mangle_type_to(buf, t->array.of);
      break;

    case TYPE_FUNCTION:
      format_to(buf, "F");
      mangle_type_to(buf, t->function.return_type);
      foreach (Parameter, param, t->function.parameters) mangle_type_to(buf, param->type);
      format_to(buf, "E");
      break;
  }
}

void mangle_function_name(IRFunction *function) {
  if (function->is_extern) return;

  string_buffer buf = {0};
  format_to(&buf, "_XF%Z%S", function->name.size, function->name);
  mangle_type_to(&buf, function->type);
  free(function->name.data);
  function->name = (string){buf.data, buf.size};
}

void codegen_lower_x86_64(CodegenContext *context) {
  // IR fixup for this specific backend.
  lower(context);
}

bool parameter_is_in_register_x86_64(CodegenContext *context, IRFunction *function, usz parameter_index) {
  if (parameter_index >= function->type->function.parameters.size)
    ICE("Parameter index out of bounds");

  IRInstruction *parameter = function->parameters.data[parameter_index];

  switch (context->call_convention) {

  case CG_CALL_CONV_MSWIN: {
    if (parameter_index >= 4) return false;
    if (type_sizeof(parameter->type) > 8) return false;
  } return true;

  case CG_CALL_CONV_LINUX: {
    TODO("x86_64 backend does not yet support determining sysv ABI specifics for parameters, sorry");
  } return true;

  default:
    ICE("Unhandled calling convention: %d\n", context->call_convention);
  }

}

void codegen_emit_x86_64(CodegenContext *context) {
  /// Emit static variables.
  /// TODO: interning.
  bool have_data_section = false;
  foreach_ptr (IRStaticVariable*, var, context->static_vars) {
    /// Do not emit unused variables.
    if (optimise) {
        bool used = false;
        foreach_ptr (IRInstruction*, ref, var->references) {
            if (ref->users.size) {
                used = true;
                break;
            }
        }
        if (!used) continue;
    }

    /// Emit a data section directive if we haven't already.
    if (!have_data_section) {
      have_data_section = true;
      fprint(context->code, ".section .data\n");
    }

    // TODO: Do compile-time known static assignment (i.e. of string
    // literals) using assembler directives.

    if (var->init) {
      if (var->init->kind == IR_LIT_INTEGER) {
        fprint(context->code, "%S: .byte ", var->name);
        unsigned char *byte_repr = (unsigned char*)(&var->init->imm);

        // TODO: Endianness selection

        // `%u` and the `(unsigned)` cast is because variadic arguments
        // of integral types are always promoted to at least `int` or
        // `unsigned` in C.
        fprint(context->code, "%u", (unsigned) byte_repr[0]);
        for (usz i = 1; i < sizeof(var->init->imm); ++i)
          fprint(context->code, ",%u", (unsigned) byte_repr[i]);

        fprint(context->code, "\n");
      } else if (var->init->kind == IR_LIT_STRING) {
        {// MANUAL (required because multiline strings)
          fprint(context->code, "%S: .byte ", var->name);
          if (var->init->str.size)
            fprint(context->code, "%u", (unsigned) var->init->str.data[0]);
          for (usz i = 1; i < var->init->str.size; ++i)
            fprint(context->code, ",%u", (unsigned) var->init->str.data[i]);
          fprint(context->code, ",0\n");
        }
      }
      else {
        ir_femit_instruction(stdout, var->init);
        ICE("Unhandled literal IR type for static variable in x86_64 backend, sorry.");
      }
    } else {
      /// Allocate space for the variable.
      usz sz = type_sizeof(var->type);
      fprint(context->code, "%S: .space %zu\n", var->name, sz);
    }
  }

  /// Allocate registers to each temporary within the program.
  const MachineDescription desc = {
    .registers = general,
    .register_count = GENERAL_REGISTER_COUNT,
    .argument_registers = argument_registers,
    .argument_register_count = argument_register_count,
    .result_register = REG_RAX,
    .instruction_register_interference = interfering_regs
  };

  foreach_ptr (IRFunction*, f, context->functions)
    if (!f->is_extern) allocate_registers(f, &desc);

  if (debug_ir) ir_femit(stdout, context);

  // Assign block labels.
  usz block_cnt = 0;
  foreach_ptr (IRFunction*, function, context->functions) {
    list_foreach (IRBlock *, block, function->blocks) {
      if (optimise) {
        /// Determine whether this block is ever referenced anywhere.
        bool referenced = false;
        for (IRBlock *b = (function->blocks).first; b; b = b->next) {
          for (IRInstruction *i = (b->instructions).first; i; i = i->next) {
            switch (i->kind) {
              default: break;
              case IR_UNREACHABLE: goto next_block;
              case IR_BRANCH:
                if (i->destination_block == block) {
                  /// Direct branches to the next block are no-ops.
                  if (i->destination_block == block->next) goto next_block;
                  referenced = true;
                  goto done;
                }
                break;
              case IR_BRANCH_CONDITIONAL:
                if (i->cond_br.then == block) {
                  if (i->cond_br.then == i->parent_block->next) continue;
                  referenced = true;
                  goto done;
                }
                if (i->cond_br.else_ == block) {
                  if (i->cond_br.else_ == i->parent_block->next) continue;
                  referenced = true;
                  goto done;
                }
                break;
            }
          }
        next_block:;
        }

      done:
        if (!referenced) {
          block->name = string_dup(unreferenced_block_name);
          continue;
        }
      }

      block->name = format(".L%U", block_cnt++);
    }
  }

  /*ir_set_ids(context);
  ir_femit(stdout, context);*/

  calculate_stack_offsets(context);

  // FUNCTION NAME MANGLING
  foreach_ptr (IRFunction*, function, context->functions) {
    // Don't mangle external function(s).
    if (!function->is_extern)
      // Don't mangle `main` function.
      if (!string_eq(function->name, literal_span("main"))) mangle_function_name(function);
  }

  emit_entry(context);
  foreach_ptr (IRFunction*, function, context->functions) {
    if (!function->is_extern) emit_function(context, function);
  }
}
