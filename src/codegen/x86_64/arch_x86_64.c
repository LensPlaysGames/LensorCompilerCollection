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

  /// Using this for anything other than Reg <-> Reg is a VERY bad
  /// idea unless you know what you're doing.
  I_XCHG,

  I_COUNT
};

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
  STATIC_ASSERT(I_COUNT == 24, "ERROR: instruction_mnemonic() must exhaustively handle all instructions.");
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
  case I_XCHG: return "xchg";
  case I_LEA: return "lea";
  case I_SETCC: return "set";
  case I_TEST: return "test";
  case I_JCC: return "j";
  }

  switch (context->dialect) {
  default: ICE("instruction_mnemonic(): Unknown output format.");

  case CG_ASM_DIALECT_ATT:
    switch (instruction) {
    default: ICE("instruction_mnemonic(): Unknown instruction.");
    case I_CQO: return "cqto";
    }

  case CG_ASM_DIALECT_INTEL:
    switch (instruction) {
    default: ICE("instruction_mnemonic(): Unknown instruction.");
    case I_CQO: return "cqo";
    }
  }
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
      fprint(context->code, "    %s %s(%%%s), %%%s\n",
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

static void femit_reg_to_reg(CodegenContext *context, enum Instruction inst, RegisterDescriptor source_register, RegisterDescriptor destination_register) {
  const char *mnemonic = instruction_mnemonic(context, inst);
  const char *source = register_name(source_register);
  const char *destination = register_name(destination_register);

  // Always optimise away moves from a register to itself
  if (inst == I_MOV && source_register == destination_register) return;

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
      fprint(context->code, "    %s %%%s, %s(%%%s)\n",
          mnemonic, source, name, address);
      break;
    case CG_ASM_DIALECT_INTEL:
      fprint(context->code, "    %s [%s + %s], %s\n",
          mnemonic, address, name, source);
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
  STATIC_ASSERT(I_COUNT == 24, "femit() must exhaustively handle all x86_64 instructions.");

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
 RegisterDescriptor result
 ) {
  ASSERT(type < COMPARE_COUNT, "Invalid comparison type");

  // Zero out result register.

  // Perform the comparison.
  femit_reg_to_reg(cg_context, I_CMP, rhs, lhs);
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
      femit_reg_to_reg(cg_context, I_MOV, REG_RSP, REG_RBP);
      // femit_reg_to_reg(context, I_MOV, REG_RSP, REG_RBP);
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
      femit_imm_to_reg(cg_context, I_SUB, locals_offset, REG_RSP, r64);
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
      femit_reg_to_reg(cg_context, I_MOV, REG_RBP, REG_RSP);
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
  STATIC_ASSERT(IR_COUNT == 34, "Handle all IR instructions");
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
        femit_imm_to_reg(context, I_MOV, inst->imm, inst->result, r32);
      } else if (inst->imm <= UINT64_MAX) {
        femit_imm_to_reg(context, I_MOV, inst->imm, inst->result, r64);
      } else {
        ICE("Unsupported integer literal immediate on x86_64 (out of range)");
      }
    } else {
      if (type_sizeof(inst->type) <= 4) {
        femit_imm_to_reg(context, I_MOV, inst->imm, inst->result, r32);
      } else if (type_sizeof(inst->type) <= 8) {
        femit_imm_to_reg(context, I_MOV, inst->imm, inst->result, r64);
      } else {
        ICE("Unsupported immediate size on x86_64: %Z", type_sizeof(inst->type));
      }
    }
    break;
  case IR_NOT:
    femit_reg(context, I_NOT, inst->operand->result);
    femit_reg_to_reg(context, I_MOV, inst->operand->result, inst->result);
    break;
  case IR_COPY:
    femit_reg_to_reg(context, I_MOV, inst->operand->result, inst->result);
    break;
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

    size_t x = func_regs;
    while (x) {
      regs_pushed_count++;
      x &= x - 1;
    }
    // Align stack pointer before call, if necessary.
    if (regs_pushed_count & 0b1) {
      femit_imm_to_reg(context, I_SUB, 8, REG_RSP, r64);
    }
    for (Register i = REG_RAX + 1; i < sizeof(func_regs) * 8; ++i) {
      if (func_regs & (1 << i) && is_caller_saved(i)) {
        // TODO: Don't push registers that are used for arguments.
        femit_reg(context, I_PUSH, i);
      }
    }

    if (inst->call.is_indirect) femit_reg(context, I_CALL, inst->call.callee_instruction->result);
    else femit_name(context, I_CALL, inst->call.callee_function->name.data);
    // femit_name(context, I_CALL, inst->call.callee_function->name.data);

    // Restore caller saved registers used in called function.
    for (Register i = sizeof(func_regs) * 8 - 1; i > REG_RAX; --i) {
      if (func_regs & (1 << i) && is_caller_saved(i)) {
        femit_reg(context, I_POP, i);
      }
    }
    // Restore stack pointer from stack alignment, if necessary.
    if (regs_pushed_count & 0b1) {
      femit_imm_to_reg(context, I_ADD, 8, REG_RSP, r64);
    }
    femit_reg_to_reg(context, I_MOV, REG_RAX, inst->result);
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

    femit_reg_to_reg(context, I_TEST, branch->condition->result, branch->condition->result);

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
  case IR_LE: codegen_comparison(context, COMPARE_LE, inst->lhs->result, inst->rhs->result, inst->result); break;
  case IR_LT: codegen_comparison(context, COMPARE_LT, inst->lhs->result, inst->rhs->result, inst->result); break;
  case IR_GE: codegen_comparison(context, COMPARE_GE, inst->lhs->result, inst->rhs->result, inst->result); break;
  case IR_GT: codegen_comparison(context, COMPARE_GT, inst->lhs->result, inst->rhs->result, inst->result); break;
  case IR_EQ: codegen_comparison(context, COMPARE_EQ, inst->lhs->result, inst->rhs->result, inst->result); break;
  case IR_NE: codegen_comparison(context, COMPARE_NE, inst->lhs->result, inst->rhs->result, inst->result); break;
  case IR_ADD:
    femit_reg_to_reg(context, I_ADD, inst->lhs->result, inst->rhs->result);
    femit_reg_to_reg(context, I_MOV, inst->rhs->result, inst->result);
    break;
  case IR_SUB:
    femit_reg_to_reg(context, I_SUB, inst->rhs->result, inst->lhs->result);
    femit_reg_to_reg(context, I_MOV, inst->lhs->result, inst->result);
    break;
  case IR_MUL:
    femit_reg_to_reg(context, I_IMUL, inst->lhs->result, inst->rhs->result);
    femit_reg_to_reg(context, I_MOV, inst->rhs->result, inst->result);
    break;
  case IR_DIV:
    ASSERT(inst->rhs->result != REG_RAX,
           "Register allocation must not allocate RAX to divisor.");
    femit_reg_to_reg(context, I_MOV, inst->lhs->result, REG_RAX);
    femit(context, I_CQO);
    femit_reg(context, I_IDIV, inst->rhs->result);
    femit_reg_to_reg(context, I_MOV, REG_RAX, inst->result);
    break;
  case IR_MOD:
    ASSERT(inst->rhs->result != REG_RAX,
           "Register allocation must not allocate RAX to divisor.");
    femit_reg_to_reg(context, I_MOV, inst->rhs->result, REG_RAX);
    femit(context, I_CQO);
    femit_reg(context, I_IDIV, inst->rhs->result);
    femit_reg_to_reg(context, I_MOV, REG_RDX, inst->result);
    break;
  case IR_SHL:
    ASSERT(inst->lhs->result != REG_RCX,
           "Register allocation must not allocate RCX to result of lhs of shift.");
    femit_reg_to_reg(context, I_MOV, inst->rhs->result, REG_RCX);
    femit_reg(context, I_SHL, inst->lhs->result);
    femit_reg_to_reg(context, I_MOV, inst->lhs->result, inst->result);
    break;
  case IR_SHR:
    femit_reg_to_reg(context, I_MOV, inst->rhs->result, REG_RCX);
    femit_reg(context, I_SHR, inst->lhs->result);
    femit_reg_to_reg(context, I_MOV, inst->lhs->result, inst->result);
    break;
  case IR_SAR:
    femit_reg_to_reg(context, I_MOV, inst->rhs->result, REG_RCX);
    femit_reg(context, I_SAR, inst->lhs->result);
    femit_reg_to_reg(context, I_MOV, inst->lhs->result, inst->result);
    break;
  case IR_AND:
    femit_reg_to_reg(context, I_AND, inst->lhs->result, inst->rhs->result);
    femit_reg_to_reg(context, I_MOV, inst->rhs->result, inst->result);
    break;
  case IR_OR:
    femit_reg_to_reg(context, I_OR, inst->lhs->result, inst->rhs->result);
    femit_reg_to_reg(context, I_MOV, inst->rhs->result, inst->result);
    break;

  case IR_LOAD:
    // TODO: Handle size of type and stuff
    /// Load from a static variable.
    if (inst->operand->kind == IR_STATIC_REF) {
      enum RegSize size = -1;
      // TODO: Should this array to pointer decay happen here? Or higher up in codegen?
      if (inst->operand->type->kind == TYPE_ARRAY || inst->operand->type->pointer.to->kind == TYPE_ARRAY)
        size = regsize_from_bytes(type_sizeof(t_void_ptr));
      else size = regsize_from_bytes(type_sizeof(inst->operand->type));
      // TODO: Use `movzx`/`movzbl`
      if (size == r8 || size == r16) femit_reg_to_reg(context, I_XOR, inst->result, inst->result);
      // femit_reg_to_reg(context, I_XOR, inst->result, inst->result);
      if (inst->operand->type->kind == TYPE_ARRAY || inst->operand->type->pointer.to->kind == TYPE_ARRAY)
        femit_name_to_reg(context, I_LEA, REG_RIP, inst->operand->static_ref->name.data, inst->result, size);
      else
        femit_name_to_reg(context, I_MOV, REG_RIP, inst->operand->static_ref->name.data, inst->result, size);
    }

    /// Load from a local.
    else if (inst->operand->kind == IR_ALLOCA) {
      enum RegSize size = -1;
      // TODO: Should this array to pointer decay happen here? Or higher up in codegen?
      if (inst->operand->type->kind == TYPE_ARRAY || inst->operand->type->pointer.to->kind == TYPE_ARRAY)
        size = regsize_from_bytes(type_sizeof(t_void_ptr));
      else size = regsize_from_bytes(inst->operand->alloca.size);
      // TODO: Use `movzx`/`movzbl`
      if (size == r8 || size == r16) femit_reg_to_reg(context, I_XOR, inst->result, inst->result);
      if (inst->operand->type->kind == TYPE_ARRAY || inst->operand->type->pointer.to->kind == TYPE_ARRAY)
        femit_mem_to_reg(context, I_LEA, REG_RBP, -inst->operand->alloca.offset, inst->result, size);
      else
        femit_mem_to_reg(context, I_MOV, REG_RBP, -inst->operand->alloca.offset, inst->result, size);
    }

    /// Load from a pointer
    else {
      enum RegSize size = -1;
      // TODO: Should this array to pointer decay happen here? Or higher up in codegen?
      if (inst->operand->type->kind == TYPE_ARRAY) size = regsize_from_bytes(type_sizeof(t_void_ptr));
      // TODO: We are "supposed" to be loading sizeof pointed to type
      // here, but that causes segfaults when handling arrays.
      else size = regsize_from_bytes(type_sizeof(inst->operand->type));
      if (size == r8 || size == r16) femit_reg_to_reg(context, I_XOR, inst->result, inst->result);
      if (inst->operand->type->kind == TYPE_ARRAY)
        femit_mem_to_reg(context, I_LEA, inst->operand->result, 0, inst->result, size);
      else
        femit_mem_to_reg(context, I_MOV, inst->operand->result, 0, inst->result, size);
    }
    break;

  case IR_STORE:
    /// Store to a static variable.
    if (inst->store.addr->kind == IR_STATIC_REF) {
      enum RegSize size = regsize_from_bytes(type_sizeof(inst->store.addr->static_ref->type));
      femit_reg_to_name(context, I_MOV, inst->store.value->result, size, REG_RIP, inst->store.addr->static_ref->name.data);
    }

    /// Store to a local.
    else if (inst->store.addr->kind == IR_ALLOCA) {
      enum RegSize size = regsize_from_bytes(type_sizeof(inst->store.value->type));
      femit_reg_to_mem(context, I_MOV, inst->store.value->result, size, REG_RBP, -inst->store.addr->alloca.offset);
      break;
    }

    /// Store to a pointer.
    else {
      enum RegSize size = regsize_from_bytes(type_sizeof(inst->store.value->type));
      femit_reg_to_mem(context, I_MOV, inst->store.value->result, size, inst->store.addr->result, 0);
    }
    break;

  case IR_STATIC_REF:
    if (inst->result) femit_name_to_reg(context, I_LEA, REG_RIP, inst->static_ref->name.data, inst->result, r64);
    break;
  case IR_FUNC_REF:
    if (inst->result) femit_name_to_reg(context, I_LEA, REG_RIP, inst->function_ref->name.data, inst->result, r64);
    break;
  case IR_ALLOCA:
    femit_mem_to_reg(context, I_LEA, REG_RBP, -inst->alloca.offset, inst->result, r64);
    break;

  default:
    ir_femit_instruction(stderr, inst);
    TODO("Handle IRType %d\n", inst->kind);
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

typedef enum Clobbers {
  CLOBBERS_NEITHER,
  CLOBBERS_REFERENCE,
  CLOBBERS_LEFT,
  CLOBBERS_RIGHT,
  CLOBBERS_BOTH,
  CLOBBERS_OTHER,
} Clobbers;

Clobbers does_clobber(IRInstruction *instruction) {
  STATIC_ASSERT(IR_COUNT == 34, "Exhaustive handling of IR types.");
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

static void lower(CodegenContext *context) {
  ASSERT(argument_registers, "arch_x86_64 backend can not lower IR when argument registers have not been initialized.");
  FOREACH_INSTRUCTION (context) {
    switch (instruction->kind) {
      case IR_PARAMETER:
        if ((size_t)instruction->imm >= argument_register_count) {
          TODO("x86_64 backend doesn't yet support passing arguments on the stack, sorry.");
        }
        instruction->kind = IR_REGISTER;
        instruction->result = argument_registers[instruction->imm];
        break;
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
    default:
      ICE("Invalid call convention.");
  }

  // IR fixup for this specific backend.
  lower(context);
}

void codegen_emit_x86_64(CodegenContext *context) {
  /// Emit static variables.
  /// TODO: interning.
  bool have_data_section = false;
  foreach_ptr (IRStaticVariable*, var, context->static_vars) {
    /// Do not emit unused variables.
    if (optimise && var->reference->users.size == 0) continue;

    /// Emit a data section directive if we haven't already.
    if (!have_data_section) {
      have_data_section = true;
      fprint(context->code, ".section .data\n");
    }

    // TODO: Do compile-time known static assignment (i.e. of string
    // literals) using assembler directives.

    if (var->init) {
      if (var->init->kind == IR_LIT_INTEGER) {
        TODO("Handle static initialisation from IR integer literal");
        // TODO: This isn't ideal, I don't think? Because our idea of
        // `int` doesn't line up with binutils' idea of `int`.
        fprint(context->code, ".int %U\n", var->init->imm);
      } else if (var->init->kind == IR_LIT_STRING) {
        fprint(context->code, "%S: .asciz \"%S\"\n", var->name, var->init->str);
        /*{// MANUAL
          fprint(context->code, "%S: .byte ", var->name);
          if (var->init->str.size)
            fprint(context->code, "%U", var->init->str.data[0]);
          for (usz i = 1; i < var->init->str.size; ++i)
            fprint(context->code, ", %U", var->init->str.data[i]);
          fprint(context->code, ", 0\n");
        }*/
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
    allocate_registers(f, &desc);

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
