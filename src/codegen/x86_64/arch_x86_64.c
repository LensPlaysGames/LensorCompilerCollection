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

#define X86_64_GENERATE_MACHINE_CODE

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
  I_DIV,
  I_IDIV,
  I_XOR,
  I_CMP,
  I_TEST,
  I_CWD,
  I_CDQ,
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
  STATIC_ASSERT(I_COUNT == 29, "ERROR: instruction_mnemonic() must exhaustively handle all instructions.");
  // x86_64 instructions that aren't different across syntaxes can go here!
  switch (instruction) {
  default: break;
  case I_ADD: return "add";
  case I_SUB: return "sub";
    // case I_MUL: return "mul";
  case I_IMUL: return "imul";
  case I_DIV: return "div";
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
    case I_CWD: return "cwtd";
    case I_CDQ: return "cltd";
    case I_CQO: return "cqto";
    default: break;
    }
  } break;

  case CG_ASM_DIALECT_INTEL: {
    switch (instruction) {
    case I_CWD: return "cwd";
    case I_CDQ: return "cdq";
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

#ifdef X86_64_GENERATE_MACHINE_CODE

#include <codegen/generic_object.h>

// NOTE: +rw indicates the lower three bits of the opcode byte are used
// to indicate the 16-bit register operand.
// For registers R8 through R15, the REX.b bit also needs set.
static uint8_t rw_encoding(RegisterDescriptor reg) {
  switch (reg) {
  case REG_RAX:
  case REG_R8:
    return 0;
  case REG_RCX:
  case REG_R9:
    return 1;
  case REG_RDX:
  case REG_R10:
    return 2;
  case REG_RBX:
  case REG_R11:
    return 3;
  case REG_RSP:
  case REG_R12:
    return 4;
  case REG_RBP:
  case REG_R13:
    return 5;
  case REG_RSI:
  case REG_R14:
    return 6;
  case REG_RDI:
  case REG_R15:
    return 7;
  }
  UNREACHABLE();
}

// NOTE: +rw indicates the lower three bits of the opcode byte are used
// to indicate the 32 or 64-bit register operand.
// For registers R8 through R15, the REX.b bit also needs set.
// EAX: 0
// ECX: 1
// EDX: 2
// EBX: 3
// ESP: 4
// EBP: 5
// ESI: 6
// EDI: 7
// R8D: REX.B, 0
// R9D: REX.B, 1
// R10D: REX.B, 2
// R11D: REX.B, 3
// R12D: REX.B, 4
// R13D: REX.B, 5
// R14D: REX.B, 6
// R15D: REX.B, 7
static uint8_t rd_encoding(RegisterDescriptor reg) {
  return rw_encoding(reg);
}

// NOTE: +rw indicates the lower three bits of the opcode byte are used
// to indicate the 32 or 64-bit register operand.
// For registers R8 through R15, the REX.b bit also needs set.
// AL: 0
// CL: 1
// DL: 2
// BL: 3
// SPL: REX.b, 4
// BPL: REX.b, 5
// SIL: REX.b, 6
// DIL: REX.b, 7
// R8B: REX.b, 0
// R9B: REX.b, 1
// R10B: REX.b, 2
// R11B: REX.b, 3
// R12B: REX.b, 4
// R13B: REX.b, 5
// R14B: REX.b, 6
// R15B: REX.b, 7
static uint8_t rb_encoding(RegisterDescriptor reg) {
  return rw_encoding(reg);
}

// Don't use me directly!
static uint8_t rex_byte(bool w, bool r, bool x, bool b) {
  return (uint8_t)(0b01000000 | ((int)w << 3) | ((int)r << 2) | ((int)x << 1) | (int)b);
}
/// REX.W prefix is commonly used to promote a 32-bit operation to 64-bit.
static uint8_t rexw_byte() {
  return rex_byte(true, false, false, false);
}

uint8_t regbits(RegisterDescriptor reg) {
  switch (reg) {
  case REG_RAX: return 0b0000;
  case REG_RCX: return 0b0001;
  case REG_RDX: return 0b0010;
  case REG_RBX: return 0b0011;
  case REG_RSP: return 0b0100;
  case REG_RBP: return 0b0101;
  case REG_RSI: return 0b0110;
  case REG_RDI: return 0b0111;
  case REG_R8:  return 0b1000;
  case REG_R9:  return 0b1001;
  case REG_R10: return 0b1010;
  case REG_R11: return 0b1011;
  case REG_R12: return 0b1100;
  case REG_R13: return 0b1101;
  case REG_R14: return 0b1110;
  case REG_R15: return 0b1111;
  default: ICE("Unhandled register in regbits: %s\n", register_name(reg));
  }
}

/// Suitable for use in an if condition to test if the top bit is set
/// in the Intel encoding of registers.
#define REGBITS_TOP(regbits) (regbits & 0b1000)

bool regbits_top(RegisterDescriptor reg) {
  return REGBITS_TOP(regbits(reg));
}

static uint8_t modrm_byte(uint8_t mod, uint8_t reg, uint8_t rm) {
  // Ensure no bits above the amount expected are set.
  ASSERT((mod & (~0b11)) == 0);
  // Top bit of register stored in REX bit(s), but may still be present here.
  ASSERT((reg & (~0b1111)) == 0);
  ASSERT((rm & (~0b1111)) == 0);
  return (uint8_t)((mod << 6) | ((reg & 0b111) << 3) | rm);
}

static void mcode_imm_to_reg(CodegenContext *context, enum Instruction inst, int64_t immediate, RegisterDescriptor destination_register, enum RegSize size) {
  if ((inst == I_SUB || inst == I_ADD) && immediate == 0) return;

  switch (inst) {
  case I_MOV: {

    switch (size) {
    case r8: {
      // Move imm8 to r8
      // 0xb0+ rb ib

      // Encode a REX prefix if the ModRM register descriptor needs
      // the bit extension.
      uint8_t destination_regbits = regbits(destination_register);
      if (REGBITS_TOP(destination_regbits)) {
        uint8_t rex = rex_byte(false, false, false, REGBITS_TOP(destination_regbits));
        mcode_1(context->object, rex);
      }

      uint8_t op = 0xb0 + rb_encoding(destination_register);
      int8_t imm8 = (int8_t)immediate;
      mcode_2(context->object, op, (uint8_t)imm8);
    } break;
    case r16: {
      // Move imm16 to r16
      // 0xb8+ rw iw
      uint8_t op = 0xb8 + rw_encoding(destination_register);
      int16_t imm16 = (int16_t)immediate;
      mcode_1(context->object, op);
      mcode_n(context->object, &imm16, 2);
    } break;
    case r32: {
      // Move imm32 to r32
      // 0xb8+ rd id
      uint8_t op = 0xb8 + rd_encoding(destination_register);
      int32_t imm32 = (int32_t)immediate;
      mcode_1(context->object, op);
      mcode_n(context->object, &imm32, 4);
    } break;
    case r64: {
      // Move imm64 to r64
      // REX.W + 0xb8+ rd io
      uint8_t rex = rexw_byte();
      uint8_t op = 0xb8 + rd_encoding(destination_register);
      mcode_2(context->object, rex, op);
      mcode_n(context->object, &immediate, 8);
    } break;

    } // switch (size)

  } break; // case I_MOV

  case I_ADD:
  case I_SUB: {

    // Immediate add/sub both share the same opcodes, just with a different opcode extension in ModRM:reg.
    uint8_t add_extension = 0;
    uint8_t sub_extension = 5;
    uint8_t modrm = 0;
    uint8_t destination_regbits = regbits(destination_register);
    // Mod == 0b11  ->  register
    // Reg == Opcode Extension (5 for sub, 0 for add)
    // R/M == Destination
    if (inst == I_ADD)
      modrm = modrm_byte(0b11, add_extension, destination_regbits);
    else modrm = modrm_byte(0b11, sub_extension, destination_regbits);

    switch (size) {
    case r8: {
      // 0x80 /5 ib
      uint8_t op = 0x80;
      int8_t imm8 = (int8_t)immediate;

      // Encode a REX prefix if the ModRM register descriptor needs
      // the bit extension.
      if (REGBITS_TOP(destination_regbits)) {
        uint8_t rex = rex_byte(false, false, false, REGBITS_TOP(destination_regbits));
        mcode_1(context->object, rex);
      }
      mcode_3(context->object, op, modrm, (uint8_t)imm8);
    } break;
    case r16: {
      // 0x66 + 0x81 /5 iw
      uint8_t sixteen_bit_prefix = 0x66;
      mcode_1(context->object, sixteen_bit_prefix);
    } // FALLTHROUGH to case r32
    case r32: {
      // 0x81 /5 id
      uint8_t op = 0x81;

      // Encode a REX prefix if the ModRM register descriptor needs
      // the bit extension.
      if (REGBITS_TOP(destination_regbits)) {
        uint8_t rex = rex_byte(false, false, false, REGBITS_TOP(destination_regbits));
        mcode_1(context->object, rex);
      }

      mcode_2(context->object, op, modrm);
      if (size == r16) {
        int16_t imm16 = (int16_t)immediate;
        mcode_n(context->object, &imm16, 2);
      } else {
        int32_t imm32 = (int32_t)immediate;
        mcode_n(context->object, &imm32, 4);
      }
    } break;

    case r64: {
      // Subtract imm32 sign extended to 64-bits from r64
      // REX.W + 0x81 /5 id
      uint8_t op = 0x81;
      uint8_t rex = rex_byte(true, false, false, REGBITS_TOP(destination_regbits));
      int32_t imm32 = (int32_t)immediate;

      mcode_3(context->object, rex, op, modrm);
      mcode_n(context->object, &imm32, 4);
    } break;

    } // switch (size)

  } break; // case I_ADD/I_SUB

  default: ICE("ERROR: mcode_imm_to_reg(): Unsupported instruction %d (%s)", inst, instruction_mnemonic(context, inst));
  }
}

static void mcode_imm_to_mem(CodegenContext *context, enum Instruction inst, int64_t immediate, RegisterDescriptor address_register, int64_t offset) {
  switch (inst) {
  default: ICE("ERROR: mcode_imm_to_mem(): Unsupported instruction %d (%s)", inst, instruction_mnemonic(context, inst));
  }
}

static void mcode_mem_to_reg(CodegenContext *context, enum Instruction inst, RegisterDescriptor address_register, int64_t offset, RegisterDescriptor destination_register, enum RegSize size) {
  switch (inst) {

  case I_LEA: {

    switch (size) {
    case r8: ICE("x86_64 machine code backend: LEA does not have an 8-bit encoding.");
    case r16: {
      // 0x66 + 0x8d /r
      uint8_t sixteen_bit_prefix = 0x66;
      mcode_1(context->object, sixteen_bit_prefix);
    } // FALLTHROUGH to case r32
    case r32: {
      // 0x8d /r
      uint8_t op = 0x8d;

      // Encode a REX prefix if either of the ModRM register descriptors need
      // the bit extension.
      uint8_t address_regbits = regbits(address_register);
      uint8_t destination_regbits = regbits(destination_register);
      if (REGBITS_TOP(address_regbits) || REGBITS_TOP(destination_regbits)) {
        uint8_t rex = rex_byte(false, REGBITS_TOP(destination_regbits), false, REGBITS_TOP(address_regbits));
        mcode_1(context->object, rex);
      }

      // Mod == 0b10  ->  (R/M)+disp32
      // Reg == Destination
      // R/M == Address
      uint8_t modrm = modrm_byte(0b10, destination_regbits, address_regbits);

      mcode_2(context->object, op, modrm);
      int32_t disp32 = (int32_t)offset;
      mcode_n(context->object, &disp32, 4);

    } break;
    case r64: {
      // REX.W + 0x8d /r
      uint8_t op = 0x8d;

      uint8_t address_regbits = regbits(address_register);
      uint8_t destination_regbits = regbits(destination_register);
      uint8_t rex = rex_byte(true, REGBITS_TOP(destination_regbits), false, REGBITS_TOP(address_regbits));

      // Mod == 0b10  ->  (R/M)+disp32
      // Reg == Destination
      // R/M == Address
      uint8_t modrm = modrm_byte(0b10, destination_regbits, address_regbits);

      mcode_3(context->object, rex, op, modrm);
      int32_t disp32 = (int32_t)offset;
      mcode_n(context->object, &disp32, 4);

    } break;
    } // switch (size)

  } break; // case I_LEA

  case I_MOV: {

    uint8_t address_regbits = regbits(address_register);
    uint8_t destination_regbits = regbits(destination_register);

    // Each of these branches *must* assign modrm.
    if (offset == 0) {
      // Mod == 0b00  (register)
      // Reg == Destination
      // R/M == Address
      uint8_t modrm = modrm_byte(0b00, destination_regbits, address_regbits);

      switch (size) {
      case r8: {
        // 0x8a /r
        uint8_t op = 0x8a;
        if (REGBITS_TOP(address_regbits) || REGBITS_TOP(destination_regbits)) {
          uint8_t rex = rex_byte(false, REGBITS_TOP(address_regbits), false, REGBITS_TOP(destination_regbits));
          mcode_1(context->object, rex);
        }
        mcode_2(context->object, op, modrm);
      } break;

      case r16: {
        // 0x66 + 0x8b /r
        uint8_t sixteen_bit_prefix = 0x66;
        mcode_1(context->object, sixteen_bit_prefix);
      } // FALLTHROUGH to case r32
      case r32: {
        // 0x8b /r
        uint8_t op = 0x8b;
        if (REGBITS_TOP(address_regbits) || REGBITS_TOP(destination_regbits)) {
          uint8_t rex = rex_byte(false, REGBITS_TOP(address_regbits), false, REGBITS_TOP(destination_regbits));
          mcode_1(context->object, rex);
        }
        mcode_2(context->object, op, modrm);
      } break;

      case r64: {
        // REX.W + 0x8b /r
        uint8_t op = 0x8b;
        uint8_t rex = rex_byte(true, REGBITS_TOP(address_regbits), false, REGBITS_TOP(destination_regbits));
        mcode_3(context->object, rex, op, modrm);
      } break;

      } // switch (size)

    } else if (offset >= -128 && offset <= 127) {
      // Mod == 0b01  (register + disp8)
      // Reg == Destination
      // R/M == Address
      uint8_t modrm = modrm_byte(0b01, destination_regbits, address_regbits);
      int8_t disp8 = (int8_t)offset;

      switch (size) {
      case r8: {
        // 0x8a /r
        uint8_t op = 0x8a;
        if (REGBITS_TOP(address_regbits) || REGBITS_TOP(destination_regbits)) {
          uint8_t rex = rex_byte(false, REGBITS_TOP(address_regbits), false, REGBITS_TOP(destination_regbits));
          mcode_1(context->object, rex);
        }
        mcode_3(context->object, op, modrm, (uint8_t)disp8);
      } break;

      case r16: {
        // 0x66 + 0x8b /r
        uint8_t sixteen_bit_prefix = 0x66;
        mcode_1(context->object, sixteen_bit_prefix);
      } // FALLTHROUGH to case r32
      case r32: {
        // 0x8b /r
        uint8_t op = 0x8b;
        if (REGBITS_TOP(address_regbits) || REGBITS_TOP(destination_regbits)) {
          uint8_t rex = rex_byte(false, REGBITS_TOP(address_regbits), false, REGBITS_TOP(destination_regbits));
          mcode_1(context->object, rex);
        }
        mcode_3(context->object, op, modrm, (uint8_t)disp8);
      } break;

      case r64: {
        // REX.W + 0x8b /r
        uint8_t op = 0x8b;
        uint8_t rex = rex_byte(true, REGBITS_TOP(address_regbits), false, REGBITS_TOP(destination_regbits));
        mcode_4(context->object, rex, op, modrm, (uint8_t)disp8);
      } break;

      } // switch (size)

    } else {
      // Mod == 0b10  (register + disp32)
      // Reg == Destination
      // R/M == Address
      uint8_t modrm = modrm_byte(0b10, destination_regbits, address_regbits);
      int32_t disp32 = (int32_t)offset;

      switch (size) {
      case r8: {
        // 0x8a /r
        uint8_t op = 0x8a;
        if (REGBITS_TOP(address_regbits) || REGBITS_TOP(destination_regbits)) {
          uint8_t rex = rex_byte(false, REGBITS_TOP(address_regbits), false, REGBITS_TOP(destination_regbits));
          mcode_1(context->object, rex);
        }
        mcode_2(context->object, op, modrm);
        mcode_n(context->object, &disp32, 4);
      } break;
      case r16: {
        // 0x66 + 0x8b /r
        uint8_t sixteen_bit_prefix = 0x66;
        mcode_1(context->object, sixteen_bit_prefix);
      } // FALLTHROUGH to case r32
      case r32: {
        // 0x8b /r
        uint8_t op = 0x8b;
        if (REGBITS_TOP(address_regbits) || REGBITS_TOP(destination_regbits)) {
          uint8_t rex = rex_byte(false, REGBITS_TOP(address_regbits), false, REGBITS_TOP(destination_regbits));
          mcode_1(context->object, rex);
        }
        mcode_2(context->object, op, modrm);
        mcode_n(context->object, &disp32, 4);
      } break;
      case r64: {
        // REX.W + 0x8b /r
        uint8_t op = 0x8b;
        uint8_t rex = rex_byte(true, REGBITS_TOP(address_regbits), false, REGBITS_TOP(destination_regbits));
        mcode_3(context->object, rex, op, modrm);
        mcode_n(context->object, &disp32, 4);
      } break;

      } // switch (size)
    }

  } break; // case I_MOV

  default: ICE("ERROR: mcode_mem_to_reg(): Unsupported instruction %d (%s)", inst, instruction_mnemonic(context, inst));
  }
}

/// Write x86_64 machine code for instruction `inst` with `name` offset from `address_register` and store the result in register `destination_register` with size `size`.
static void mcode_name_to_reg(CodegenContext *context, enum Instruction inst, RegisterDescriptor address_register, const char *name, RegisterDescriptor destination_register, enum RegSize size) {
  switch (inst) {

  case I_LEA: {

    switch (size) {
    case r8: ICE("x86_64 machine code backend: LEA does not have an 8-bit encoding.");
    case r16: {
      // 0x66 + 0x8d /r
      uint8_t sixteen_bit_prefix = 0x66;
      mcode_1(context->object, sixteen_bit_prefix);
    } // FALLTHROUGH to case r32
    case r32: {
      // 0x8d /r
      uint8_t op = 0x8d;

      // RIP-Relative Addressing
      if (address_register == REG_RIP) {

        uint8_t destination_regbits = regbits(destination_register);
        if (REGBITS_TOP(destination_regbits)) {
          uint8_t rex = rex_byte(false, REGBITS_TOP(destination_regbits), false, false);
          mcode_1(context->object, rex);
        }

        // Mod == 0b00
        // R/M == 0b101 (none)
        uint8_t modrm = modrm_byte(0b00, destination_regbits, 0b101);

        mcode_2(context->object, op, modrm);

        // Make RIP-relative disp32 relocation
        RelocationEntry reloc = {0};
        Section *sec_code = code_section(context->object);
        ASSERT(sec_code, "NO CODE SECTION, WHAT HAVE YOU DONE?");
        reloc.sym.byte_offset = sec_code->data.bytes.size;
        reloc.sym.name = strdup(name);
        reloc.sym.section_name = strdup(sec_code->name);
        reloc.type = RELOC_DISP32_PCREL;
        vector_push(context->object->relocs, reloc);

        int32_t disp32 = 0;
        mcode_n(context->object, &disp32, 4);

        break;
      }

      // Encode a REX prefix if either of the ModRM register descriptors need
      // the bit extension.
      uint8_t address_regbits = regbits(address_register);
      uint8_t destination_regbits = regbits(destination_register);
      if (REGBITS_TOP(address_regbits) || REGBITS_TOP(destination_regbits)) {
        uint8_t rex = rex_byte(false, REGBITS_TOP(destination_regbits), false, REGBITS_TOP(address_regbits));
        mcode_1(context->object, rex);
      }

      // Mod == 0b10  ->  (R/M)+disp32
      // Reg == Destination
      // R/M == Address
      uint8_t modrm = modrm_byte(0b10, destination_regbits, address_regbits);

      mcode_2(context->object, op, modrm);

      // Make disp32 relocation to lea from symbol
      RelocationEntry reloc = {0};
      Section *sec_code = code_section(context->object);
      reloc.sym.byte_offset = sec_code->data.bytes.size;
      reloc.sym.name = strdup(name);
      reloc.sym.section_name = strdup(sec_code->name);
      reloc.type = RELOC_DISP32;
      vector_push(context->object->relocs, reloc);

      int32_t disp32 = 0;
      mcode_n(context->object, &disp32, 4);
    } break;
    case r64: {
      // REX.W + 0x8d /r
      uint8_t op = 0x8d;

      // RIP-Relative Addressing
      if (address_register == REG_RIP) {
        uint8_t destination_regbits = regbits(destination_register);
        uint8_t rex = rex_byte(true, REGBITS_TOP(destination_regbits), false, false);
        // Mod == 0b00
        // R/M == 0b101 (none)
        uint8_t modrm = modrm_byte(0b00, destination_regbits, 0b101);
        int32_t disp32 = 0;

        mcode_3(context->object, rex, op, modrm);

        // Make RIP-relative disp32 relocation
        RelocationEntry reloc = {0};
        Section *sec_code = code_section(context->object);
        ASSERT(sec_code, "NO CODE SECTION, WHAT HAVE YOU DONE?");
        reloc.sym.byte_offset = sec_code->data.bytes.size;
        reloc.sym.name = strdup(name);
        reloc.sym.section_name = strdup(sec_code->name);
        reloc.type = RELOC_DISP32_PCREL;
        vector_push(context->object->relocs, reloc);

        mcode_n(context->object, &disp32, 4);
        break;
      }

      uint8_t address_regbits = regbits(address_register);
      uint8_t destination_regbits = regbits(destination_register);
      uint8_t rex = rex_byte(true, REGBITS_TOP(destination_regbits), false, REGBITS_TOP(address_regbits));

      // Mod == 0b10  ->  (R/M)+disp32
      // Reg == Destination
      // R/M == Address
      uint8_t modrm = modrm_byte(0b10, destination_regbits, address_regbits);

      mcode_3(context->object, rex, op, modrm);

      // Make disp32 relocation to lea from symbol
      RelocationEntry reloc = {0};
      Section *sec_code = code_section(context->object);
      reloc.sym.byte_offset = sec_code->data.bytes.size;
      reloc.sym.name = strdup(name);
      reloc.sym.section_name = strdup(sec_code->name);
      reloc.type = RELOC_DISP32;
      vector_push(context->object->relocs, reloc);

      int32_t disp32 = 0;
      mcode_n(context->object, &disp32, 4);
    } break;

    } // switch (size)

  } break; // case I_LEA

  case I_MOV: {

    switch (size) {

    case r8: {
      // 0x8a /r
      uint8_t op = 0x8a;

      // RIP-Relative Addressing
      if (address_register == REG_RIP) {
        uint8_t destination_regbits = regbits(destination_register);
        if (REGBITS_TOP(destination_regbits)) {
          uint8_t rex = rex_byte(false, REGBITS_TOP(destination_regbits), false, false);
          mcode_1(context->object, rex);
        }

        // Mod == 0b00
        // R/M == 0b101 (none)
        uint8_t modrm = modrm_byte(0b00, destination_regbits, 0b101);

        mcode_2(context->object, op, modrm);

        // Make RIP-relative disp32 relocation
        RelocationEntry reloc = {0};
        Section *sec_code = code_section(context->object);
        ASSERT(sec_code, "NO CODE SECTION, WHAT HAVE YOU DONE?");
        reloc.sym.byte_offset = sec_code->data.bytes.size;
        reloc.sym.name = strdup(name);
        reloc.sym.section_name = strdup(sec_code->name);
        reloc.type = RELOC_DISP32_PCREL;
        vector_push(context->object->relocs, reloc);

        int32_t disp32 = 0;
        mcode_n(context->object, &disp32, 4);
        break;
      }

      // Encode a REX prefix if either of the ModRM register descriptors need
      // the bit extension.
      uint8_t address_regbits = regbits(address_register);
      uint8_t destination_regbits = regbits(destination_register);
      if (REGBITS_TOP(address_regbits) || REGBITS_TOP(destination_regbits)) {
        uint8_t rex = rex_byte(false, REGBITS_TOP(address_regbits), false, REGBITS_TOP(destination_regbits));
        mcode_1(context->object, rex);
      }

      // Mod == 0b10  ->  (R/M)+disp32
      // Reg == Destination
      // R/M == Address
      uint8_t modrm = modrm_byte(0b10, destination_regbits, address_regbits);

      mcode_2(context->object, op, modrm);

      // Make disp32 relocation to lea from symbol
      RelocationEntry reloc = {0};
      Section *sec_code = code_section(context->object);
      reloc.sym.byte_offset = sec_code->data.bytes.size;
      reloc.sym.name = strdup(name);
      reloc.sym.section_name = strdup(sec_code->name);
      reloc.type = RELOC_DISP32;
      vector_push(context->object->relocs, reloc);

      int32_t disp32 = 0;
      mcode_n(context->object, &disp32, 4);
    } break;
    case r16: {
      // 0x66 + 0x8b /r
      uint8_t sixteen_bit_prefix = 0x66;
      mcode_1(context->object, sixteen_bit_prefix);
    } // FALLTHROUGH to case r32
    case r32: {
      // 0x8b /r
      uint8_t op = 0x8b;

      // RIP-Relative Addressing
      if (address_register == REG_RIP) {
        uint8_t destination_regbits = regbits(destination_register);
        if (REGBITS_TOP(destination_regbits)) {
          uint8_t rex = rex_byte(false,  REGBITS_TOP(destination_regbits), false, false);
          mcode_1(context->object, rex);
        }

        // Mod == 0b00
        // R/M == 0b101 (none)
        uint8_t modrm = modrm_byte(0b00, destination_regbits, 0b101);

        mcode_2(context->object, op, modrm) ;

        // Make RIP-relative disp32 relocation
        RelocationEntry reloc = {0};
        Section *sec_code = code_section(context->object);
        ASSERT(sec_code, "NO CODE SECTION, WHAT HAVE YOU DONE?");
        reloc.sym.byte_offset = sec_code->data.bytes.size;
        reloc.sym.name = strdup(name);
        reloc.sym.section_name = strdup(sec_code->name);
        reloc.type = RELOC_DISP32_PCREL;
        vector_push(context->object->relocs, reloc);

        int32_t disp32 = 0;
        mcode_n(context->object, &disp32, 4);
        break;
      }

      // Encode a REX prefix if either of the ModRM register descriptors need
      // the bit extension.
      uint8_t address_regbits = regbits(address_register);
      uint8_t destination_regbits = regbits(destination_register);
      if (REGBITS_TOP(address_regbits) || REGBITS_TOP(destination_regbits)) {
        uint8_t rex = rex_byte(false, REGBITS_TOP(address_regbits), false, REGBITS_TOP(destination_regbits));
        mcode_1(context->object, rex);
      }

      // Mod == 0b10  ->  (R/M)+disp32
      // Reg == Destination
      // R/M == Address
      uint8_t modrm = modrm_byte(0b10, destination_regbits, address_regbits);

      mcode_2(context->object, op, modrm);

      // Make disp32 relocation to lea from symbol
      RelocationEntry reloc = {0};
      Section *sec_code = code_section(context->object);
      reloc.sym.byte_offset = sec_code->data.bytes.size;
      reloc.sym.name = strdup(name);
      reloc.sym.section_name = strdup(sec_code->name);
      reloc.type = RELOC_DISP32;
      vector_push(context->object->relocs, reloc);

      int32_t disp32 = 0;
      mcode_n(context->object, &disp32, 4);
    } break;
    case r64: {
      // REX.W + 0x8b /r
      uint8_t op = 0x8b;

      // RIP-Relative Addressing
      if (address_register == REG_RIP) {
        uint8_t destination_regbits = regbits(destination_register);
        uint8_t rex = rex_byte(true, REGBITS_TOP(destination_regbits), false, false);

        // Mod == 0b00
        // R/M == 0b101 (none)
        uint8_t modrm = modrm_byte(0b00, destination_regbits, 0b101);

        mcode_3(context->object, rex, op, modrm);

        // Make RIP-relative disp32 relocation
        RelocationEntry reloc = {0};
        Section *sec_code = code_section(context->object);
        ASSERT(sec_code, "NO CODE SECTION, WHAT HAVE YOU DONE?");
        reloc.sym.byte_offset = sec_code->data.bytes.size;
        reloc.sym.name = strdup(name);
        reloc.sym.section_name = strdup(sec_code->name);
        reloc.type = RELOC_DISP32_PCREL;
        vector_push(context->object->relocs, reloc);

        int32_t disp32 = 0;
        mcode_n(context->object, &disp32, 4);
        break;
      }

      // Encode a REX prefix if either of the ModRM register descriptors need
      // the bit extension.
      uint8_t address_regbits = regbits(address_register);
      uint8_t destination_regbits = regbits(destination_register);
      uint8_t rex = rex_byte(true, REGBITS_TOP(address_regbits), false, REGBITS_TOP(destination_regbits));

      // Mod == 0b10  ->  (R/M)+disp32
      // Reg == Destination
      // R/M == Address
      uint8_t modrm = modrm_byte(0b10, destination_regbits, address_regbits);

      mcode_3(context->object, rex, op, modrm);

      // Make disp32 relocation to lea from symbol
      RelocationEntry reloc = {0};
      Section *sec_code = code_section(context->object);
      reloc.sym.byte_offset = sec_code->data.bytes.size;
      reloc.sym.name = strdup(name);
      reloc.sym.section_name = strdup(sec_code->name);
      reloc.type = RELOC_DISP32;
      vector_push(context->object->relocs, reloc);

      int32_t disp32 = 0;
      mcode_n(context->object, &disp32, 4);
    } break;

    } // switch (size)

  } break; // case I_MOV

  default: ICE("ERROR: mcode_name_to_reg(): Unsupported instruction %d (%s)", inst, instruction_mnemonic(context, inst));
  }
}

static void mcode_reg_to_mem(CodegenContext *context, enum Instruction inst, RegisterDescriptor source_register, enum RegSize size, RegisterDescriptor address_register, int64_t offset) {
  switch (inst) {

  case I_MOV: {

    switch (size) {
    case r8: {
      // Move r8 into m8
      // 0x88 /r
      uint8_t op = 0x88;

      // Encode a REX prefix if either of the ModRM register
      // descriptors need the bit extension.
      uint8_t source_regbits = regbits(source_register);
      uint8_t address_regbits = regbits(address_register);
      if (REGBITS_TOP(source_regbits) || REGBITS_TOP(address_regbits)) {
        uint8_t rex = rex_byte(false, REGBITS_TOP(source_regbits), false, REGBITS_TOP(address_regbits));
        mcode_1(context->object, rex);
      }

      if (offset >= -128 && offset <= 127) {

        // To cut down on code size, we encode offsets that fit into
        // one byte using a Mod value of 0b01, allowing for only one byte
        // to be written for the displacement.

        // Mod == 0b01  ->  register + disp8
        // Reg == Source
        // R/M == Address
        uint8_t modrm = modrm_byte(0b01, source_regbits, address_regbits);
        int8_t displacement = (int8_t)offset;

        mcode_3(context->object, op, modrm, (uint8_t)displacement);

      } else if (offset) {

        // Mod == 0b10  ->  register + disp32
        // Reg == Source
        // R/M == Address
        uint8_t modrm = modrm_byte(0b10, source_regbits, address_regbits);
        int32_t displacement = (int32_t)offset;

        mcode_2(context->object, op, modrm);
        mcode_n(context->object, &displacement, 4);

      } else {

        // If offset is zero, we can omit the displacement byte(s).
        // Mod == 0b00  ->  (R/M)
        // Reg == Source
        // R/M == Address
        uint8_t modrm = modrm_byte(0b00, source_regbits, address_regbits);

        mcode_2(context->object, op, modrm);

      }

    } break;
    case r16: {
      // 0x66 + 0x89 /r
      uint8_t sixteen_bit_prefix = 0x66;
      mcode_1(context->object, sixteen_bit_prefix);
    } // FALLTHROUGH to case r32
    case r32: {
      // Move r32 into m32
      // 0x89 /r
      uint8_t op = 0x89;

      // Encode a REX prefix if either of the ModRM register descriptors need
      // the bit extension.
      uint8_t source_regbits = regbits(source_register);
      uint8_t address_regbits = regbits(address_register);
      if (REGBITS_TOP(source_regbits) || REGBITS_TOP(address_regbits)) {
        uint8_t rex = rex_byte(false, REGBITS_TOP(source_regbits), false, REGBITS_TOP(address_regbits));
        mcode_1(context->object, rex);
      }

      // Mod == 0b10  ->  register + disp32
      // Reg == Source
      // R/M == Address
      uint8_t modrm = modrm_byte(0b10, source_regbits, address_regbits);
      int32_t displacement = (int32_t)offset;

      mcode_2(context->object, op, modrm);
      mcode_n(context->object, &displacement, 4);

    } break;
    case r64: {
      // Move r64 into m64
      // REX.W + 0x89 /r
      uint8_t op = 0x89;

      // Encode a REX.W prefix to promote operation to 64-bits, also
      // including ModRM register bit extension(s).
      uint8_t source_regbits = regbits(source_register);
      uint8_t address_regbits = regbits(address_register);
      uint8_t rex = rex_byte(true, REGBITS_TOP(source_regbits), false, REGBITS_TOP(address_regbits));

      // Make output code smaller when possible by omitting zero displacements.
      if (offset == 0) {
        // Mod == 0b00  ->  R/M
        // Reg == Source
        // R/M == Address
        uint8_t modrm = modrm_byte(0b00, source_regbits, address_regbits);

        mcode_3(context->object, rex, op, modrm);
      } else {
        // Mod == 0b10  ->  R/M + disp32
        // Reg == Source
        // R/M == Address
        uint8_t modrm = modrm_byte(0b10, source_regbits, address_regbits);
        int32_t displacement = (int32_t)offset;

        mcode_3(context->object, rex, op, modrm);
        mcode_n(context->object, &displacement, 4);
      }
    } break;
    }

  } break;

  default: ICE("ERROR: mcode_reg_to_mem(): Unsupported instruction %d (%s)", inst, instruction_mnemonic(context, inst));
  }
}

static void mcode_reg_to_reg
(CodegenContext *context,
 enum Instruction inst,
 RegisterDescriptor source_register, enum RegSize source_size,
 RegisterDescriptor destination_register, enum RegSize destination_size
 )
{
  // Always optimise away moves from a register to itself
  if (inst == I_MOV && source_register == destination_register && source_size == destination_size) return;

  uint8_t source_regbits = regbits(source_register);
  uint8_t destination_regbits = regbits(destination_register);
  // Mod == 0b11  ->  Reg
  // Reg == Source
  // R/M == Destination
  uint8_t modrm = modrm_byte(0b11, source_regbits, destination_regbits);

  switch (inst) {

  case I_MOV: {
    ASSERT(source_size == destination_size, "x86_64 machine code backend requires reg-to-reg moves to be of equal size.");

    switch (source_size) {

    case r8: {
      // Move r8 to r8
      // 0x88 /r
      uint8_t op = 0x88;

      // Encode a REX prefix if either of the ModRM register
      // descriptors need the bit extension.
      if (REGBITS_TOP(source_regbits) || REGBITS_TOP(destination_regbits)) {
        uint8_t rex = rex_byte(false, REGBITS_TOP(source_regbits), false, REGBITS_TOP(destination_regbits));
        mcode_1(context->object, rex);
      }

      mcode_2(context->object, op, modrm);
    } break;

    case r16: {
      // 0x66 + 0x89 /r
      uint8_t sixteen_bit_prefix = 0x66;
      mcode_1(context->object, sixteen_bit_prefix);
    } // FALLTHROUGH to case r32
    case r32: {
      // 0x89 /r
      uint8_t op = 0x89;

      // Encode a REX prefix if either of the ModRM register descriptors need
      // the bit extension.
      if (REGBITS_TOP(source_regbits) || REGBITS_TOP(destination_regbits)) {
        uint8_t rex = rex_byte(false, REGBITS_TOP(source_regbits), false, REGBITS_TOP(destination_regbits));
        mcode_1(context->object, rex);
      }

      mcode_2(context->object, op, modrm);
    } break;

    case r64: {
      // REX.W + 0x89 /r
      uint8_t op = 0x89;
      uint8_t rex = rex_byte(true, REGBITS_TOP(source_regbits), false, REGBITS_TOP(destination_regbits));

      mcode_3(context->object, rex, op, modrm);
    } break;

    } // switch (size)

  } break; // case I_MOV

  case I_ADD: {

    ASSERT(source_size == destination_size, "x86_64 machine code backend requires reg-to-reg adds to be of equal size.");

    switch (source_size) {
    case r8: {
      // Add r8 to r8
      // 0x00 /r
      uint8_t op = 0x00;

      if (REGBITS_TOP(source_regbits) || REGBITS_TOP(destination_regbits)) {
        uint8_t rex = rex_byte(false, REGBITS_TOP(source_regbits), false, REGBITS_TOP(destination_regbits));
        mcode_1(context->object, rex);
      }

      mcode_2(context->object, op, modrm);
    } break;

    case r16: {
      // 0x66 + 0x01 /r
      uint8_t sixteen_bit_prefix = 0x66;
      mcode_1(context->object, sixteen_bit_prefix);
    } // FALLTHROUGH to case r32
    case r32: {
      // 0x01 /r
      uint8_t op = 0x01;

      if (REGBITS_TOP(source_regbits) || REGBITS_TOP(destination_regbits)) {
        uint8_t rex = rex_byte(false, REGBITS_TOP(source_regbits), false, REGBITS_TOP(destination_regbits));
        mcode_1(context->object, rex);
      }

      mcode_2(context->object, op, modrm);
    } break;

    case r64: {
      // REX.W + 0x01 /r
      uint8_t op = 0x01;
      uint8_t rex = rex_byte(true, REGBITS_TOP(source_regbits), false, REGBITS_TOP(destination_regbits));

      mcode_3(context->object, rex, op, modrm);
    } break;

    } // switch (size)

  } break; // case I_ADD

  case I_SUB: {

    ASSERT(source_size == destination_size, "x86_64 machine code backend requires reg-to-reg subs to be of equal size.");

    switch (source_size) {
    case r8: {
      // Subtract r8 from r8
      // 0x28 /r
      uint8_t op = 0x28;

      if (REGBITS_TOP(source_regbits) || REGBITS_TOP(destination_regbits)) {
        uint8_t rex = rex_byte(false, REGBITS_TOP(source_regbits), false, REGBITS_TOP(destination_regbits));
        mcode_1(context->object, rex);
      }

      mcode_2(context->object, op, modrm);
    } break;

    case r16: {
      // 0x66 + 0x29 /r
      uint8_t sixteen_bit_prefix = 0x66;
      mcode_1(context->object, sixteen_bit_prefix);
    } // FALLTHROUGH to case r32
    case r32: {
      // 0x29 /r
      uint8_t op = 0x29;

      if (REGBITS_TOP(source_regbits) || REGBITS_TOP(destination_regbits)) {
        uint8_t rex = rex_byte(false, REGBITS_TOP(source_regbits), false, REGBITS_TOP(destination_regbits));
        mcode_1(context->object, rex);
      }

      mcode_2(context->object, op, modrm);
    } break;

    case r64: {
      // REX.W + 0x29 /r
      uint8_t op = 0x29;
      uint8_t rex = rex_byte(true, REGBITS_TOP(source_regbits), false, REGBITS_TOP(destination_regbits));

      mcode_3(context->object, rex, op, modrm);
    } break;

    } // switch (size)

  } break; // case I_SUB

  case I_CMP: {

    ASSERT(source_size == destination_size, "x86_64 machine code backend requires reg-to-reg cmps to be of equal size.");

    switch (source_size) {
    case r8: {
      // 0x38 /r
      uint8_t op = 0x38;

      if (REGBITS_TOP(source_regbits) || REGBITS_TOP(destination_regbits)) {
        uint8_t rex = rex_byte(false, REGBITS_TOP(source_regbits), false, REGBITS_TOP(destination_regbits));
        mcode_1(context->object, rex);
      }

      mcode_2(context->object, op, modrm);
    } break;

    case r16: {
      // 0x66 + 0x39 /r
      uint8_t sixteen_bit_prefix = 0x66;
      mcode_1(context->object, sixteen_bit_prefix);
    } // FALLTHROUGH to case r32
    case r32: {
      // 0x39 /r
      uint8_t op = 0x39;

      if (REGBITS_TOP(source_regbits) || REGBITS_TOP(destination_regbits)) {
        uint8_t rex = rex_byte(false, REGBITS_TOP(source_regbits), false, REGBITS_TOP(destination_regbits));
        mcode_1(context->object, rex);
      }

      mcode_2(context->object, op, modrm);
    } break;

    case r64: {
      // REX.W + 0x39 /r
      uint8_t rex = rex_byte(true, REGBITS_TOP(source_regbits), false, REGBITS_TOP(destination_regbits));
      uint8_t op = 0x39;

      mcode_3(context->object, rex, op, modrm);
    } break;

    } // switch (size)

  } break; // case I_CMP

  case I_TEST: {
    ASSERT(source_size == destination_size, "x86_64 machine code backend requires reg-to-reg tests to be of equal size.");

    switch (source_size) {
    case r8: {
      // 0x84 /r
      uint8_t op = 0x84;

      if (REGBITS_TOP(source_regbits) || REGBITS_TOP(destination_regbits)) {
        uint8_t rex = rex_byte(false, REGBITS_TOP(source_regbits), false, REGBITS_TOP(destination_regbits));
        mcode_1(context->object, rex);
      }

      mcode_2(context->object, op, modrm);
    } break;

    case r16: {
      // 0x66 + 0x85 /r
      uint8_t sixteen_bit_prefix = 0x66;
      mcode_1(context->object, sixteen_bit_prefix);
    } // FALLTHROUGH to case r32
    case r32: {
      // 0x85 /r
      uint8_t op = 0x85;

      if (REGBITS_TOP(source_regbits) || REGBITS_TOP(destination_regbits)) {
        uint8_t rex = rex_byte(false, REGBITS_TOP(source_regbits), false, REGBITS_TOP(destination_regbits));
        mcode_1(context->object, rex);
      }

      mcode_2(context->object, op, modrm);
    } break;

    case r64: {
      // REX.W + 0x85 /r
      uint8_t op = 0x85;

      uint8_t rex = rex_byte(true, REGBITS_TOP(source_regbits), false, REGBITS_TOP(destination_regbits));
      mcode_3(context->object, rex, op, modrm);
    } break;

    } // switch (size)

  } break; // case I_TEST

  default: ICE("ERROR: mcode_reg_to_reg(): Unsupported instruction %d (%s)", inst, instruction_mnemonic(context, inst));

  }
}

static void mcode_indirect_branch(CodegenContext *context, enum Instruction inst, RegisterDescriptor address_register) {
  switch (inst) {
  default: ICE("ERROR: mcode_indirect_branch(): Unsupported instruction %d (%s)", inst, instruction_mnemonic(context, inst));
  }
}

static void mcode_reg_shift(CodegenContext *context, enum Instruction inst, RegisterDescriptor register_to_shift) {
  switch (inst) {
  default: ICE("ERROR: mcode_reg_shift(): Unsupported instruction %d (%s)", inst, instruction_mnemonic(context, inst));
  }
}

static void mcode_reg(CodegenContext *context, enum Instruction inst, RegisterDescriptor reg, enum RegSize size) {
  if (inst == I_JMP || inst == I_CALL) {
    mcode_indirect_branch(context, inst, reg);
    return;
  }
  if (inst == I_SAL || inst == I_SAR || inst == I_SHL || inst == I_SHR) {
    mcode_reg_shift(context, inst, reg);
    return;
  }

  // NOTE: +rb/+rw/+rd/+ro indicate the lower three bits of the opcode byte are used to indicate the register operand.
  // In 64-bit mode, indicates the four bit field of REX.b and opcode[2:0] field encodes the register operand.

  switch (inst) {
  case I_PUSH: {
    switch (size) {
    case r8:
    case r32:
      ICE("ERROR: x86_64 doesn't support pushing %Z-byte registers to the stack.", regbytes_from_size(size));

    case r16: {
      // 0x66 + 0x50+rw
      uint8_t op = 0x50 + rw_encoding(reg);
      mcode_2(context->object, 0x66, op);
    } break;
    case r64: {
      // 0x50+rd
      uint8_t source_regbits = regbits(reg);
      if (REGBITS_TOP(source_regbits)) {
        uint8_t rex = rex_byte(false, false,false, REGBITS_TOP(source_regbits));
        mcode_1(context->object, rex);
      }
      uint8_t op = 0x50 + rd_encoding(reg);
      mcode_1(context->object, op);
    } break;
    }
  } break;

  case I_POP: {
    switch (size) {
    case r8:
    case r32:
      ICE("ERROR: x86_64 doesn't support pushing %Z-byte registers to the stack.", regbytes_from_size(size));

    case r16: {
      // 0x66 + 0x58+rw
      uint8_t op = 0x58 + rw_encoding(reg);
      mcode_2(context->object, 0x66, op);
    } break;
    case r64: {
      // 0x58+rd
      uint8_t source_regbits = regbits(reg);
      if (REGBITS_TOP(source_regbits)) {
        uint8_t rex = rex_byte(false, false,false, REGBITS_TOP(source_regbits));
        mcode_1(context->object, rex);
      }
      uint8_t op = 0x58 + rd_encoding(reg);
      mcode_1(context->object, op);
    } break;
    }
  } break;

  default:
    ICE("ERROR: mcode_reg(): Unsupported instruction %d (%s)", inst, instruction_mnemonic(context, inst));
  }
}

static void mcode_reg_to_name(CodegenContext *context, enum Instruction inst, RegisterDescriptor source_register, enum RegSize size, RegisterDescriptor address_register, const char *name) {
  // TODO: Generate a relocation entry that will end up in the object file...
  switch (inst) {
  default: ICE("ERROR: mcode_reg_to_name(): Unsupported instruction %d (%s)", inst, instruction_mnemonic(context, inst));
  }
}

static void mcode_reg_to_offset_name(CodegenContext *context, enum Instruction inst, RegisterDescriptor source_register, enum RegSize size, RegisterDescriptor address_register, const char *name, usz offset) {
  // TODO: Generate a relocation entry that will end up in the object file...
  switch (inst) {
  default: ICE("ERROR: mcode_reg_to_offset_name(): Unsupported instruction %d (%s)", inst, instruction_mnemonic(context, inst));
  }
}

static void mcode_mem(CodegenContext *context, enum Instruction inst, int64_t offset, RegisterDescriptor address_register) {
  switch (inst) {
  default: ICE("ERROR: mcode_mem(): Unsupported instruction %d (%s)", inst, instruction_mnemonic(context, inst));
  }
}

static void mcode_imm(CodegenContext *context, enum Instruction inst, int64_t immediate) {
  switch (inst) {
  case I_PUSH: {
    // TODO: What size immediate to push?
    // PUSH imm8:  0x6a ib
    // PUSH imm16: 0x66 + 0x68 iw
    // PUSH imm32: 0x68 id
    uint8_t op = 0x68;
    int32_t immediate_value = (int32_t)immediate;
    mcode_1(context->object, op);
    mcode_n(context->object, &immediate_value, 4);
  } break;

  default:
    ICE("ERROR: mcode_imm(): Unsupported instruction %d (%s)", inst, instruction_mnemonic(context, inst));
  }
}

static void mcode_name(CodegenContext *context, enum Instruction inst, const char *name) {
  // TODO: Generate a relocation entry that will end up in the object file...
  switch (inst) {

  case I_CALL: {
    uint8_t op = 0xe8;
    int32_t disp32 = 0;
    mcode_1(context->object, op);

    // Make disp32 relocation to lea from symbol
    RelocationEntry reloc = {0};
    Section *sec_code = code_section(context->object);
    // Current offset in machine code byte buffer
    reloc.sym.byte_offset = sec_code->data.bytes.size;
    reloc.sym.name = strdup(name);
    reloc.sym.section_name = strdup(sec_code->name);
    reloc.type = RELOC_DISP32_PCREL;
    vector_push(context->object->relocs, reloc);

    mcode_n(context->object, &disp32, 4);
  } break; // case I_CALL

  case I_JMP: {
    uint8_t op = 0xe9;
    int32_t disp32 = 0;
    mcode_1(context->object, op);

    // Make disp32 relocation to lea from symbol
    RelocationEntry reloc = {0};
    Section *sec_code = code_section(context->object);
    reloc.sym.byte_offset = sec_code->data.bytes.size;
    reloc.sym.name = strdup(name);
    reloc.sym.section_name = strdup(sec_code->name);
    reloc.type = RELOC_DISP32_PCREL;
    vector_push(context->object->relocs, reloc);

    mcode_n(context->object, &disp32, 4);
  } break; // case I_JMP

  default: ICE("ERROR: mcode_name(): Unsupported instruction %d (%s)", inst, instruction_mnemonic(context, inst));
  }
}

static void mcode_none(CodegenContext *context, enum Instruction inst) {
  switch (inst) {
  case I_RET: { // 0xc3
    uint8_t op = 0xc3;
    mcode_1(context->object, op);
  } break;
  case I_CWD: { // 0x66 + 0x99
    uint8_t sixteen_bit_prefix = 0x66;
    mcode_1(context->object, sixteen_bit_prefix);
  } // FALLTHROUGH to case I_CDQ
  case I_CDQ: { // 0x99
    uint8_t op = 0x99;
    mcode_1(context->object, op);
  } break;
  case I_CQO: { // REX.W + 0x99
    uint8_t op = 0x99;
    uint8_t rexw = rexw_byte();
    mcode_2(context->object, rexw, op);
  } break;

  // FIXME: This shouldn't be here once `setcc` gets it's own emission function.
  case I_SETCC: break; // NOTE: Handled in `femit()`

  // FIXME: This shouldn't be here once `jcc` gets it's own emission function.
  case I_JCC: break; // NOTE: Handled in `femit()`

  default:
    ICE("ERROR: mcode_none(): Unsupported instruction %d (%s)", inst, instruction_mnemonic(context, inst));
  }
}

#endif // X86_64_GENERATE_MACHINE_CODE

static void femit_imm_to_reg(CodegenContext *context, enum Instruction inst, int64_t immediate, RegisterDescriptor destination_register, enum RegSize size) {
#ifdef X86_64_GENERATE_MACHINE_CODE
  mcode_imm_to_reg(context, inst, immediate, destination_register, size);
#endif // X86_64_GENERATE_MACHINE_CODE

  if ((inst == I_SUB || inst == I_ADD) && immediate == 0) return;

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
#ifdef X86_64_GENERATE_MACHINE_CODE
  mcode_imm_to_mem(context, inst, immediate, address_register, offset);
#endif // X86_64_GENERATE_MACHINE_CODE

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
#ifdef X86_64_GENERATE_MACHINE_CODE
  mcode_mem_to_reg(context, inst, address_register, offset, destination_register, size);
#endif // X86_64_GENERATE_MACHINE_CODE

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
#ifdef X86_64_GENERATE_MACHINE_CODE
  mcode_name_to_reg(context, inst, address_register, name, destination_register, size);
#endif // X86_64_GENERATE_MACHINE_CODE

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
#ifdef X86_64_GENERATE_MACHINE_CODE
  mcode_reg_to_mem(context, inst, source_register, size, address_register, offset);
#endif // X86_64_GENERATE_MACHINE_CODE

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
#ifdef X86_64_GENERATE_MACHINE_CODE
  mcode_reg_to_reg(context, inst, source_register, source_size, destination_register, destination_size);
#endif // X86_64_GENERATE_MACHINE_CODE

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
#ifdef X86_64_GENERATE_MACHINE_CODE
  mcode_reg_to_name(context, inst, source_register, size, address_register, name);
#endif // X86_64_GENERATE_MACHINE_CODE

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
#ifdef X86_64_GENERATE_MACHINE_CODE
  mcode_reg_to_offset_name(context, inst, source_register, size, address_register, name, offset);
#endif // X86_64_GENERATE_MACHINE_CODE

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
#ifdef X86_64_GENERATE_MACHINE_CODE
  mcode_mem(context, inst, offset, address_register);
#endif // X86_64_GENERATE_MACHINE_CODE

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

static void femit_reg(CodegenContext *context, enum Instruction inst, RegisterDescriptor reg, enum RegSize size) {
#ifdef X86_64_GENERATE_MACHINE_CODE
  mcode_reg(context, inst, reg, size);
#endif // x86_64_GENERATE_MACHINE_CODE

  if (inst == I_JMP || inst == I_CALL) {
    femit_indirect_branch(context, inst, reg);
    return;
  }
  if (inst == I_SAL || inst == I_SAR || inst == I_SHL || inst == I_SHR) {
    femit_reg_shift(context, inst, reg);
    return;
  }

  const char *mnemonic = instruction_mnemonic(context, inst);
  const char *source = regname(reg, size);
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
#ifdef X86_64_GENERATE_MACHINE_CODE
  mcode_imm(context, inst, immediate);
#endif // x86_64_GENERATE_MACHINE_CODE

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

#ifdef X86_64_GENERATE_MACHINE_CODE
  mcode_name(context, inst, name);
#endif // x86_64_GENERATE_MACHINE_CODE

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
#ifdef X86_64_GENERATE_MACHINE_CODE
  mcode_none(context, instruction);
#endif // X86_64_GENERATE_MACHINE_CODE

  va_list args;
  va_start(args, instruction);

  ASSERT(context);
  STATIC_ASSERT(I_COUNT == 29, "femit() must exhaustively handle all x86_64 instructions.");

  // TODO: Extract setcc and jcc to their own functions, get rid of varargs
  switch (instruction) {
    case I_SETCC: {
      enum ComparisonType comparison_type = va_arg(args, enum ComparisonType);
      RegisterDescriptor value_register = va_arg(args, RegisterDescriptor);

      const char *mnemonic = instruction_mnemonic(context, instruction);
      const char *value = register_name_8(value_register);

#ifdef X86_64_GENERATE_MACHINE_CODE
      uint8_t op = 0;
      switch (comparison_type) {
      case COMPARE_EQ: op = 0x94; break;
      case COMPARE_NE: op = 0x95; break;
      case COMPARE_GT: op = 0x9f; break;
      case COMPARE_LT: op = 0x9c; break;
      case COMPARE_GE: op = 0x9d; break;
      case COMPARE_LE: op = 0x9e; break;
      default: ICE("Invalid comparison type");
      }

      uint8_t destination_regbits = regbits(value_register);
      if (REGBITS_TOP(destination_regbits)) {
        uint8_t rex = rex_byte(false, false, false, REGBITS_TOP(destination_regbits));
        mcode_1(context->object, rex);
      }

      // Mod == 0b11  ->  register
      // R/M == Destination
      uint8_t modrm = modrm_byte(0b11, 0, destination_regbits);

      uint8_t op_escape = 0x0f;
      mcode_3(context->object, op_escape, op, modrm);
#endif // x86_64_GENERATE_MACHINE_CODE

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

#ifdef X86_64_GENERATE_MACHINE_CODE
      uint8_t op = 0;
      switch (type) {
      case JUMP_TYPE_E:  op = 0x84; break;
      case JUMP_TYPE_NE: op = 0x85; break;
      case JUMP_TYPE_G:  op = 0x8f; break;
      case JUMP_TYPE_L:  op = 0x8c; break;
      case JUMP_TYPE_GE: op = 0x8d; break;
      case JUMP_TYPE_LE: op = 0x8e; break;
      default: ICE("Unhandled jump type: %d", (int)type);
      }

      uint8_t op_escape = 0x0f;
      int32_t disp32 = 0;

      mcode_2(context->object, op_escape, op);

      // Make disp32 relocation to lea from symbol
      RelocationEntry reloc = {0};
      Section *sec_code = code_section(context->object);
      reloc.sym.byte_offset = sec_code->data.bytes.size;
      reloc.sym.name = strdup(label);
      reloc.sym.section_name = strdup(sec_code->name);
      reloc.type = RELOC_DISP32_PCREL;
      vector_push(context->object->relocs, reloc);

      mcode_n(context->object, &disp32, 4);
#endif // x86_64_GENERATE_MACHINE_CODE

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
    case I_CWD:
    case I_CDQ:
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
 IRType ir_type,
 RegisterDescriptor lhs,
 RegisterDescriptor rhs,
 RegisterDescriptor result,
 enum RegSize size)
{
  enum ComparisonType type = COMPARE_COUNT;
  switch (ir_type) {
    case IR_EQ: type = COMPARE_EQ; break;
    case IR_NE: type = COMPARE_NE; break;
    case IR_LT: type = COMPARE_LT; break;
    case IR_GT: type = COMPARE_GT; break;
    case IR_LE: type = COMPARE_LE; break;
    case IR_GE: type = COMPARE_GE; break;
    default: ICE("Unsupported IR instruction in codegen_comparison: %d", ir_type);
  }

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
      femit_reg(cg_context, I_PUSH, REG_RBP, r64);
      femit_reg_to_reg(cg_context, I_MOV, REG_RSP, r64, REG_RBP, r64);
      if (!optimise || f->locals_total_size)
        femit_imm_to_reg(cg_context, I_SUB, (i64) f->locals_total_size, REG_RSP, r64);
    } break;

    case FRAME_MINIMAL: {
      femit_reg(cg_context, I_PUSH, REG_RBP, r64);
    } break;
  }
}

/// Emit the function epilogue.
static void codegen_epilogue(CodegenContext *cg_context, IRFunction *f) {
  enum StackFrameKind frame_kind = stack_frame_kind(cg_context, f);
  switch (frame_kind) {
    case FRAME_NONE: break;

    case FRAME_FULL: {
      femit_reg_to_reg(cg_context, I_MOV, REG_RBP, r64, REG_RSP, r64);
      femit_reg(cg_context, I_POP, REG_RBP, r64);
    } break;

    case FRAME_MINIMAL: {
      femit_reg(cg_context, I_POP, REG_RBP, r64);
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
  if (lhs_size == r8 || lhs_size == r16) femit_reg_to_reg(context, I_XOR, REG_RAX, r64, REG_RAX, r64);
  femit_reg_to_reg(context, I_MOV, inst->lhs->result, lhs_size, REG_RAX, lhs_size);

  if (type_is_signed(inst->type)) {
    // For 8-byte signed types, we need to extend RAX into the 16-byte
    // RDX:RAX. For this, we use CQO (convert quad-word to octal-word).
    if (lhs_size == r64)
      femit(context, I_CQO);
      // For 4-byte signed types, we need to extend EAX into the 8-byte
      // EDX:EAX. For this, we use CDQ (convert double-word to quad-word).
    else if (lhs_size == r32)
      femit(context, I_CDQ);
      // For 2-byte signed types, we need to extend AX into the 4-byte
      // DX:AX. For this, we use CWD (convert word to double-word).
    else if (lhs_size == r16)
      femit(context, I_CWD);
    else ICE("Unhandled register size for signed division");

    femit_reg(context, I_IDIV, inst->rhs->result, lhs_size);
  }
  else {
    // For unsigned types, we need to make sure RDX is zero; if it
    // was set, it would cause a signed division, and we don't want
    // that.
    femit_reg_to_reg(context, I_XOR, REG_RDX, r64, REG_RDX, r64);
    femit_reg(context, I_DIV, inst->rhs->result, lhs_size);
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
    femit_reg(context, I_NOT, inst->operand->result, r64);
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
      if (inst->call.is_indirect) femit_reg(context, I_JMP, inst->call.callee_instruction->result, r64);
      else femit_name(context, I_JMP, inst->call.callee_function->name.data);
      if (inst->parent_block) inst->parent_block->done = true;
      break;
    }

    size_t func_regs = inst->parent_block->function->registers_in_use;
    size_t regs_pushed_count = 0;

    // Save return register.
    // FIXME: Use calling convention for return register.
    if (func_regs & REG_RAX) {
      femit_reg(context, I_PUSH, REG_RAX, r64);
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
        femit_reg(context, I_PUSH, i, r64);

    usz bytes_pushed = 0;
    // Shadow stack
    if (context->call_convention == CG_CALL_CONV_MSWIN) {
      femit_imm_to_reg(context, I_SUB, 32, REG_RSP, r64);
      bytes_pushed += 32;
    }

    // Push argument addresses, if need be.
    foreach_ptr (IRInstruction *, arg, inst->call.arguments) {
      // If argument is passed on stack due to ABI.
      if (arg->kind == IR_ALLOCA) {
        femit_reg(context, I_PUSH, REG_RBP, r64);
        bytes_pushed += 8;
        femit_imm_to_mem(context, I_SUB, (i64)arg->alloca.offset, REG_RSP, 0);
      }
    }

    if (inst->call.is_indirect) femit_reg(context, I_CALL, inst->call.callee_instruction->result, r64);
    else femit_name(context, I_CALL, inst->call.callee_function->name.data);

    // Restore stack
    if (bytes_pushed)
      femit_imm_to_reg(context, I_ADD, bytes_pushed, REG_RSP, r64);

    // Restore caller saved registers used in called function.
    for (Register i = sizeof(func_regs) * 8 - 1; i > REG_RAX; --i)
      if (func_regs & (1 << i) && is_caller_saved(i))
        femit_reg(context, I_POP, i, r64);

    // Restore stack pointer from stack alignment, if necessary.
    if (regs_pushed_count & 0b1)
      femit_imm_to_reg(context, I_ADD, 8, REG_RSP, r64);

    femit_reg_to_reg(context, I_MOV, REG_RAX, r64, inst->result, r64);

    // Restore return register.
    // FIXME: Use calling convention for return register.
    if (func_regs & REG_RAX)
      femit_reg(context, I_POP, REG_RAX, r64);

  } break;

  case IR_RETURN:
    // Restore callee-saved registers used in the function.
    for (Register i = sizeof(inst->parent_block->function->registers_in_use) * 8 - 1; i > 0; --i) {
      if (inst->parent_block->function->registers_in_use & ((size_t)1 << i) && is_callee_saved(i)) {
        femit_reg(context, I_POP, i, r64);
      }
    }
    codegen_epilogue(context, inst->parent_block->function);
    femit(context, I_RET);
    if (optimise && inst->parent_block) inst->parent_block->done = true;
    break;

  case IR_BRANCH:
    /// Only emit a jump if the target isn’t the next block.
    if (!optimise || (inst->parent_block && inst->destination_block != inst->parent_block->next && !inst->parent_block->done)) {
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
  case IR_EQ: // FALLTHROUGH to case IR_GE
  case IR_NE: // FALLTHROUGH to case IR_GE
  case IR_LT: // FALLTHROUGH to case IR_GE
  case IR_GT: // FALLTHROUGH to case IR_GE
  case IR_LE: // FALLTHROUGH to case IR_GE
  case IR_GE: {
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    codegen_comparison(context, inst->kind, inst->lhs->result, inst->rhs->result, inst->result, size);
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
    divmod(context, inst);
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    femit_reg_to_reg(context, I_MOV, REG_RAX, size, inst->result, size);
  } break;
  case IR_MOD: {
    divmod(context, inst);
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    femit_reg_to_reg(context, I_MOV, REG_RDX, size, inst->result, size);
  } break;
  case IR_SHL: {
    ASSERT(inst->lhs->result != REG_RCX,
           "Register allocation must not allocate RCX to result of lhs of shift.");
    enum RegSize rhs_size = regsize_from_bytes(type_sizeof(inst->rhs->type));
    femit_reg_to_reg(context, I_MOV, inst->rhs->result, rhs_size, REG_RCX, r64);
    femit_reg(context, I_SHL, inst->lhs->result, r64);
    enum RegSize lhs_size = regsize_from_bytes(type_sizeof(inst->lhs->type));
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    femit_reg_to_reg(context, I_MOV, inst->lhs->result, lhs_size, inst->result, size);
  } break;
  case IR_SHR: {
    enum RegSize rhs_size = regsize_from_bytes(type_sizeof(inst->rhs->type));
    femit_reg_to_reg(context, I_MOV, inst->rhs->result, rhs_size, REG_RCX, r64);
    femit_reg(context, I_SHR, inst->lhs->result, r64);
    enum RegSize lhs_size = regsize_from_bytes(type_sizeof(inst->lhs->type));
    enum RegSize size = regsize_from_bytes(type_sizeof(inst->type));
    femit_reg_to_reg(context, I_MOV, inst->lhs->result, lhs_size, inst->result, size);
  } break;
  case IR_SAR: {
    enum RegSize rhs_size = regsize_from_bytes(type_sizeof(inst->rhs->type));
    femit_reg_to_reg(context, I_MOV, inst->rhs->result, rhs_size, REG_RCX, r64);
    femit_reg(context, I_SAR, inst->lhs->result, r64);
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
    /// Load from a static variable.
    if (inst->operand->kind == IR_STATIC_REF) {
      size = regsize_from_bytes(type_sizeof(inst->operand->type->pointer.to));
      if (size == r8 || size == r16) femit_reg_to_reg(context, I_XOR, inst->result, r32, inst->result, r32);
      femit_name_to_reg(context, I_MOV, REG_RIP, inst->operand->static_ref->name.data, inst->result, size);
    }

    /// Load from a local.
    else if (inst->operand->kind == IR_ALLOCA) {
      size = regsize_from_bytes(inst->operand->alloca.size);
      if (size == r8 || size == r16) femit_reg_to_reg(context, I_XOR, inst->result, r32, inst->result, r32);
      femit_mem_to_reg(context, I_MOV, REG_RBP, - (i64)inst->operand->alloca.offset, inst->result, size);
    }

    /// Load from a pointer
    else {
      size = regsize_from_bytes(type_sizeof(inst->operand->type));
      if (size == r8 || size == r16) femit_reg_to_reg(context, I_XOR, inst->result, r32, inst->result, r32);
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
#ifdef X86_64_GENERATE_MACHINE_CODE
  GObjSymbol sym = {0};
  sym.type = GOBJ_SYMTYPE_STATIC;
  sym.name = strdup(block->name.data);
  sym.section_name = strdup(code_section(context->object)->name);
  sym.byte_offset = code_section(context->object)->data.bytes.size;
  vector_push(context->object->symbols, sym);
#endif // x86_64_GENERATE_MACHINE_CODE

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
      femit_reg(context, I_PUSH, i, r64);
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

    insert_instruction_before(alloca, instruction);

    emit_memcpy(context, alloca, instruction->operand, type_sizeof(instruction->type), instruction);

    // Replace `load` with `alloca`.
    ir_replace_uses(instruction, alloca);
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

static IRInstruction *alloca_copy_of(CodegenContext *context, IRInstruction *copy, IRInstruction *insert_before_this) {
  INSTRUCTION(alloca, IR_ALLOCA);
  alloca->alloca.size = type_sizeof(copy->type);
  alloca->type = ast_make_type_pointer(context->ast, copy->type->source_location, copy->type);
  insert_instruction_before(alloca, insert_before_this);

  INSTRUCTION(store, IR_STORE);

  store->store.addr = alloca;
  mark_used(alloca, store);

  store->store.value = copy;
  mark_used(copy, store);

  insert_instruction_before(store, insert_before_this);

  return alloca;
}

typedef enum SysVArgumentClass {
  SYSV_REGCLASS_INVALID,
  SYSV_REGCLASS_INTEGER,
  SYSV_REGCLASS_SSE,
  SYSV_REGCLASS_SSEUP,
  SYSV_REGCLASS_x87,
  SYSV_REGCLASS_x87UP,
  SYSV_REGCLASS_COMPLEX_x87,
  SYSV_REGCLASS_NO_CLASS,
  SYSV_REGCLASS_MEMORY,
} SysVArgumentClass;

SysVArgumentClass sysv_classify_argument(Type *given_type) {
  Type *type = type_canonical(given_type);
  // TODO: Use type_is_integer instead of t_integer comparisons, etc.
  if (type_is_pointer(type) ||
      type_is_reference(type) ||
      type == t_integer ||
      type == t_byte) {
    return SYSV_REGCLASS_INTEGER;
  }
  // FIXME: Probably more efficient to have the size check be the
  // outermost check, and then divy it up based on more type specifics.
  if (type_is_array(type) || type_is_struct(type)) {
    usz size = type_sizeof(type);
    // If the size of an object is larger than four eightbytes, or it
    // contains unaligned fields, it has class MEMORY.
    // TODO: Check for unaligned fields.
    // Technically, the only things that get lowered into multiple
    // registers are two eightbytes or smaller; so while the "rule"
    // above *does* say four eightbytes, it actually is only two.
    if (size > 16) return SYSV_REGCLASS_MEMORY;
    // If the size of the aggregate exceeds a single eightbyte,
    // each is classified separately. Each eightbyte gets
    // initialized to class NO_CLASS.
    else if (size > 8) {

      // At this point we have a 9-16 byte aggregate type.

      if (type_is_array(type)) {
        // Classify base type of array.
        SysVArgumentClass base_class = sysv_classify_argument(type->array.of);
        if (type->array.size == 1) return base_class;
        usz base_size = type_sizeof(type->array.of);
        // If an aggregate exceeds two eightbytes, the whole argument is passed in memory.
        if (type->array.size * base_size > 16) return SYSV_REGCLASS_MEMORY;
        // Otherwise, the aggregate is less than or equal to two
        // eightbytes, and can be passed in one or two registers.
        return SYSV_REGCLASS_INTEGER;
      }

      if (type_is_struct(type))
        return SYSV_REGCLASS_INTEGER;
    }
    // Anything 1, 2, 4, or 8 bytes can go in a register.
    else return SYSV_REGCLASS_INTEGER;
  }
  return SYSV_REGCLASS_INVALID;
}

/// @return How many registers an argument takes up.
usz sysv_argument_register_count_x86_64(CodegenContext *context, Type *function, usz parameter_index) {
  ASSERT(context->call_convention == CG_CALL_CONV_LINUX, "Don't call sysv_* things unless you are using the SYSV ABI!!");
  ASSERT(function->kind == TYPE_FUNCTION);

  if (parameter_index >= function->function.parameters.size)
    ICE("Parameter index out of bounds");

  Parameter *parameter = function->function.parameters.data + parameter_index;

  SysVArgumentClass class = SYSV_REGCLASS_INVALID;
  class = sysv_classify_argument(parameter->type);
  ASSERT(class != SYSV_REGCLASS_INVALID, "Could not classify argument according to SYSV ABI, sorry");

  if (class == SYSV_REGCLASS_INTEGER) {
    if (type_sizeof(parameter->type) > 8) return 2;
    return 1;
  }
  return 0;
}

usz sysv_argument_register_index_x86_64(CodegenContext *context, Type *function, usz parameter_index) {
  ASSERT(context->call_convention == CG_CALL_CONV_LINUX, "Don't call sysv_* things unless you are using the SYSV ABI!!");
  ASSERT(function->kind == TYPE_FUNCTION);

  if (parameter_index >= function->function.parameters.size)
    ICE("Parameter index out of bounds");

  usz argument_register_offset = 0;
  for (usz i = 0; i < parameter_index; ++i) {
    argument_register_offset += sysv_argument_register_count_x86_64(context, function, i);
  }
  return argument_register_offset;
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
      size_t argcount = instruction->call.arguments.size;

      switch (context->call_convention) {
      case CG_CALL_CONV_LINUX: {
        Type *function_type = NULL;
        if (instruction->call.is_indirect) {
          if (type_is_pointer(instruction->call.callee_instruction->type))
            function_type = instruction->call.callee_instruction->type->pointer.to;
          else function_type = instruction->call.callee_instruction->type;
        } else function_type = instruction->call.callee_function->type;
        ASSERT(function_type->kind == TYPE_FUNCTION, "Expected callee of IR_CALL to be a function, but got %T\n", function_type);

        Vector(usz) sixteen_bytes_that_need_split = {0};
        foreach_index (i, function_type->function.parameters) {
          Parameter *parameter = function_type->function.parameters.data + i;
          SysVArgumentClass class = sysv_classify_argument(parameter->type);
          if (class == SYSV_REGCLASS_INTEGER && type_sizeof(parameter->type) > 8)
            vector_push(sixteen_bytes_that_need_split, i);
        }

        foreach_rev (usz, i, sixteen_bytes_that_need_split) {
          IRInstruction *argument = instruction->call.arguments.data[*i];

          // Load first eightbyte of the parameter.
          INSTRUCTION(first_eightbyte_addr, IR_COPY);
          first_eightbyte_addr->operand = argument;
          first_eightbyte_addr->type = ast_make_type_pointer(context->ast, t_integer->source_location, t_integer);
          insert_instruction_before(first_eightbyte_addr, instruction);

          INSTRUCTION(load1, IR_LOAD);
          load1->operand = first_eightbyte_addr;
          load1->type = t_integer;
          insert_instruction_before(load1, instruction);

          // Load second eightbyte of the parameter.
          // FIXME: Second eightbyte may not be fully eight bytes.
          ASSERT(type_sizeof(argument->type->pointer.to) == 16,
                 "SysV ABI requires alignment of a multiple of 16 for aggregate types from (8 to 16]: %T",
                 argument->type->pointer.to);
          INSTRUCTION(offset, IR_IMMEDIATE);
          offset->type = t_integer;
          offset->imm = 8;
          insert_instruction_before(offset, instruction);

          INSTRUCTION(second_eightbyte_addr, IR_ADD);
          second_eightbyte_addr->type = first_eightbyte_addr->type;
          second_eightbyte_addr->lhs = first_eightbyte_addr;
          mark_used(first_eightbyte_addr, second_eightbyte_addr);
          second_eightbyte_addr->rhs = offset;
          mark_used(offset, second_eightbyte_addr);
          insert_instruction_before(second_eightbyte_addr, instruction);

          INSTRUCTION(load2, IR_LOAD);
          load2->operand = second_eightbyte_addr;
          load2->type = t_integer;
          insert_instruction_before(load2, instruction);

          // Remove argument from call, and replace with two new arguments.
          ir_remove_use(argument, instruction);
          vector_remove_index(instruction->call.arguments, *i);
          vector_insert_after(instruction->call.arguments, load1, *i);
          mark_used(load1, instruction);
          vector_insert_after(instruction->call.arguments, load2, *i);
          mark_used(load2, instruction);

        }
      } break;
      case CG_CALL_CONV_MSWIN: {
        usz idx = 0;
        foreach_ptr (IRInstruction *, argument, instruction->call.arguments) {
          if (idx >= argument_register_count) break;
          Type *type = type_canonical(argument->type);
          if ((type->kind == TYPE_STRUCT || type->kind == TYPE_ARRAY) && type_sizeof(type) > 8) {
            alloca_copy_of(context, argument, instruction);
          }
          ++idx;
        }

        // FIXME: We don't need to allocate a stack copy *here* of the
        // passed argument if it itself fits in a register.
        if (argcount >= argument_register_count) {
          usz i = instruction->call.arguments.size - 1;
          foreach_ptr_rev (IRInstruction *, argument, instruction->call.arguments) {
            if (i < argument_register_count) break;
            instruction->call.arguments.data[i] = alloca_copy_of(context, argument, instruction);
            --i;
          }
        }
      } break;
      }
    } break;

    case IR_PARAMETER: {
      switch (context->call_convention) {

      case CG_CALL_CONV_LINUX: {
        if (parameter_is_in_register_x86_64(context, instruction->parent_block->function, instruction->imm)) {
          // Classify argument into register class.
          // NOTE: This has probably already been done, and we could
          // cache it and use that computed value, if we have somewhere
          // to store it.
          SysVArgumentClass class = SYSV_REGCLASS_INVALID;
          class = sysv_classify_argument(instruction->type);
          ASSERT(class != SYSV_REGCLASS_INVALID, "Could not classify argument according to SYSV ABI, sorry");
          switch(class) {
          case SYSV_REGCLASS_INTEGER: {
            usz size = type_sizeof(instruction->type);
            if (size > 8) {
              ASSERT(size <= 16, "Can only pass things that are two-eightbytes or less in general purpose registers.");

              INSTRUCTION(eightbyte1, IR_REGISTER);
              usz argument_register_index = sysv_argument_register_index_x86_64(context, instruction->parent_block->function->type, instruction->imm);
              ASSERT(argument_register_index + 1 < argument_register_count);
              eightbyte1->result = argument_registers[argument_register_index];
              eightbyte1->type = t_integer;
              insert_instruction_before(eightbyte1, instruction);

              INSTRUCTION(eightbyte2, IR_REGISTER);
              eightbyte2->result = argument_registers[argument_register_index + 1];
              eightbyte2->type = t_integer;
              insert_instruction_before(eightbyte2, instruction);

              INSTRUCTION(alloca, IR_ALLOCA);
              alloca->alloca.size = 16;
              alloca->type = ast_make_type_pointer(context->ast, instruction->type->source_location, instruction->type);
              insert_instruction_before(alloca, instruction);

              // Store first eight bytes from parameter register into
              // newly allocated local variable.
              INSTRUCTION(store1, IR_STORE);
              store1->store.addr = alloca;
              mark_used(alloca, store1);
              store1->store.value = eightbyte1;
              mark_used(eightbyte1, store1);
              insert_instruction_before(store1, instruction);

              // Increment address
              INSTRUCTION(offset, IR_IMMEDIATE);
              offset->type = t_integer;
              offset->imm = 8;
              insert_instruction_before(offset, instruction);

              INSTRUCTION(address, IR_ADD);
              address->lhs = alloca;
              mark_used(alloca, address);
              address->rhs = offset;
              mark_used(offset, address);
              address->type = ast_make_type_pointer(context->ast, t_integer->source_location, t_integer);
              insert_instruction_before(address, instruction);

              // Store second eightbyte.
              INSTRUCTION(store2, IR_STORE);
              store2->kind = IR_STORE;
              store2->store.addr = address;
              mark_used(address, store2);
              store2->store.value = eightbyte2;
              mark_used(eightbyte2, store2);
              insert_instruction_before(store2, instruction);

              instruction->kind = IR_LOAD;
              instruction->operand = alloca;

              lower_load(context, instruction);
            } else {
              instruction->kind = IR_REGISTER;
              instruction->result = argument_registers[instruction->imm];
            }
          } break;
          case SYSV_REGCLASS_MEMORY: {
            INSTRUCTION(rbp, IR_REGISTER);
            rbp->result = REG_RBP;
            rbp->type = t_integer;
            insert_instruction_before(rbp, instruction);

            usz parameter_index = instruction->imm;

            INSTRUCTION(offset, IR_IMMEDIATE);
            offset->type = t_integer;

            // FIXME: Tail calls, leaf functions, etc. may alter the size of the stack frame here.
            // Skip pushed RBP and return addess.
            offset->imm += 16;

            usz i = instruction->parent_block->function->type->function.parameters.size - 1;
            foreach_rev (Parameter, param, instruction->parent_block->function->type->function.parameters) {
              if (i <= parameter_index) break;
              offset->imm += type_sizeof(param->type);
              --i;
            }
            insert_instruction_before(offset, instruction);

            INSTRUCTION(address, IR_ADD);
            address->lhs = rbp;
            mark_used(rbp, address);
            address->rhs = offset;
            mark_used(offset, address);
            address->type = ast_make_type_pointer(context->ast, instruction->type->source_location, instruction->type);
            insert_instruction_before(address, instruction);

            instruction->kind = IR_LOAD;
            instruction->operand = address;
            mark_used(address, instruction);

            lower_load(context, instruction);

          } break;
          default:
            TODO("Handle lowering of SYSV Register Classification: %d\n", class);
          }
        }
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
          if (instruction->imm >= argument_register_count && !(type_sizeof(type) > 8))
            TODO("Handle argument on stack that is not lowered to a pointer...");

          // FIXME: Not all arguments passed on stack are lowered to pointers! Only aggregates.

          // Lower type to a pointer, because that's how the calls have
          // been altered as well.
          INSTRUCTION(rbp, IR_REGISTER);
          rbp->result = REG_RBP;
          rbp->type = t_integer;
          insert_instruction_before(rbp, instruction);

          usz parameter_index = instruction->imm;

          INSTRUCTION(offset, IR_IMMEDIATE);
          offset->type = t_integer;

          // FIXME: Tail calls, leaf functions, etc. may alter the size of the stack frame here.
          // Skip pushed RBP and return addess.
          offset->imm += 16;

          usz i = instruction->parent_block->function->type->function.parameters.size - 1;
          foreach_rev (Parameter, param, instruction->parent_block->function->type->function.parameters) {
            if (i <= parameter_index) break;
            offset->imm += type_sizeof(param->type);
            --i;
          }
          insert_instruction_before(offset, instruction);

          INSTRUCTION(address, IR_ADD);
          address->lhs = rbp;
          mark_used(rbp, address);
          address->rhs = offset;
          mark_used(offset, address);
          address->type = ast_make_type_pointer(context->ast, instruction->type->source_location, instruction->type);
          insert_instruction_before(address, instruction);

          instruction->kind = IR_LOAD;
          instruction->operand = address;
          mark_used(address, instruction);
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

  // FIXME: It'd be really great if we /didn't/ have to loop over every
  // single user of every single instruction here, but I don't see another
  // way of doing this, really.
  // Divisor of div/mod are not allowed to go in RAX/RDX; that's where
  // the dividend must go.
  foreach_ptr (IRInstruction*, inst, instruction->users) {
    if ((inst->kind == IR_DIV || inst->kind == IR_MOD) && inst->rhs == instruction) {
      mask |= (1 << REG_RAX);
      mask |= (1 << REG_RDX);
    }
  }

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
    default: TODO("Handle type kind %d in type mangling!", (int)t->kind);

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

    case TYPE_REFERENCE:
      format_to(buf, "R");
      mangle_type_to(buf, t->reference.to);
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

void codegen_lower_x86_64(CodegenContext *context) { lower(context); }

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
    SysVArgumentClass class = SYSV_REGCLASS_INVALID;
    class = sysv_classify_argument(parameter->type);
    ASSERT(class != SYSV_REGCLASS_INVALID, "Could not classify argument according to SYSV ABI, sorry");
    if (class == SYSV_REGCLASS_INTEGER) return true;
    if (class == SYSV_REGCLASS_MEMORY) return false;
    TODO("Handle SYSV Register Classification: %d\n", class);
  }

  default:
    ICE("Unhandled calling convention: %d\n", context->call_convention);
  }

}

void codegen_emit_x86_64(CodegenContext *context) {
#ifdef X86_64_GENERATE_MACHINE_CODE
  GenericObjectFile object = {0};
  context->object = &object;
  {
    Section sec_code = {0};
    sec_code.name = ".text";
    sec_code.attributes |= SEC_ATTR_EXECUTABLE;
    vector_push(object.sections, sec_code);
    Section sec_data = {0};
    sec_data.name = ".data";
    sec_data.attributes |= SEC_ATTR_WRITABLE;
    vector_push(object.sections, sec_data);
    Section sec_bss = {0};
    sec_bss.name = ".bss";
    sec_bss.attributes |= SEC_ATTR_SPAN_FILL | SEC_ATTR_WRITABLE;
    vector_push(object.sections, sec_bss);
  }
  Section *sec_initdata = get_section_by_name(object.sections, ".data");
  Section *sec_uninitdata = get_section_by_name(object.sections, ".bss");
#endif // x86_64_GENERATE_MACHINE_CODE

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

    // Do compile-time known static assignment.

    if (var->init) {
      if (var->init->kind == IR_LIT_INTEGER) {
        fprint(context->code, "%S: .byte ", var->name);
        uint8_t *byte_repr = (uint8_t*)(&var->init->imm);

        // TODO: Endianness selection

        // `%u` and the `(unsigned)` cast is because variadic arguments
        // of integral types are always promoted to at least `int` or
        // `unsigned` in C.
        fprint(context->code, "%u", (unsigned) byte_repr[0]);
        for (usz i = 1; i < type_sizeof(var->type); ++i)
          fprint(context->code, ",%u", (unsigned) byte_repr[i]);

        fprint(context->code, "\n");


#ifdef X86_64_GENERATE_MACHINE_CODE
        // Create symbol for var->name at current offset within the .data section
        GObjSymbol sym = {0};
        sym.type = GOBJ_SYMTYPE_STATIC;
        sym.name = strdup(var->name.data);
        sym.section_name = strdup(".data");
        sym.byte_offset = sec_initdata->data.bytes.size;
        vector_push(object.symbols, sym);
        // Write initialised bytes to .data section
        sec_write_n(sec_initdata, byte_repr, type_sizeof(var->type));
#endif // x86_64_GENERATE_MACHINE_CODE

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

#ifdef X86_64_GENERATE_MACHINE_CODE
      // Create symbol for var->name at current offset within the .bss section
      GObjSymbol sym = {0};
      sym.type = GOBJ_SYMTYPE_STATIC;
      sym.name = strdup(var->name.data);
      sym.section_name = strdup(".bss");
      sym.byte_offset = sec_initdata->data.bytes.size;
      vector_push(object.symbols, sym);
      // Write uninitialised bytes to .data section
      sec_uninitdata->data.fill.amount += sz;
#endif
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
    if (!function->attr_nomangle) mangle_function_name(function);
  }

  emit_entry(context);
  foreach_ptr (IRFunction*, function, context->functions) {
#ifdef X86_64_GENERATE_MACHINE_CODE
    GObjSymbol sym = {0};
    sym.type = function->is_extern ? GOBJ_SYMTYPE_EXTERNAL : GOBJ_SYMTYPE_FUNCTION;
    sym.name = strdup(function->name.data);
    sym.section_name = strdup(code_section(&object)->name);
    sym.byte_offset = code_section(&object)->data.bytes.size;
    vector_push(object.symbols, sym);
#endif // x86_64_GENERATE_MACHINE_CODE

    if (!function->is_extern) emit_function(context, function);
  }

#ifdef X86_64_GENERATE_MACHINE_CODE
  // Resolve local label (".Lxxxx") relocations.
  Vector(size_t) relocations_to_remove = {0};
  foreach_index (idx, object.relocs) {
    RelocationEntry *reloc = object.relocs.data + idx;
    GObjSymbol *sym = &reloc->sym;
    if (strlen(sym->name) > 2 && memcmp(sym->name, ".L", 2) == 0) {
      // We have to go sym->byte_offset bytes into the ByteBuffer of
      // the code section and then fill in the next bytes depending on the
      // relocation type.
      GObjSymbol *label_sym = NULL;
      foreach (GObjSymbol, s, object.symbols) {
        if (strcmp(s->name, sym->name) == 0) {
          label_sym = s;
          break;
        }
      }
      if (!label_sym) ICE("Could not find local label referenced by relocation: \"%s\"", sym->name);

      // TODO: Handle endianess
      // NOTE: 4 == sizeof relocation displacement
      int32_t disp32 = (int32_t)label_sym->byte_offset - (4 + (int32_t)sym->byte_offset);
      uint8_t *src_it = (uint8_t*)&disp32;
      uint8_t *dst_it = code_section(&object)->data.bytes.data + sym->byte_offset;
      for (int i = 0; i < 4; ++i)
        *dst_it++ = *src_it++;

      vector_push(relocations_to_remove, idx);
    }
  }
  foreach_rev (size_t, idx, relocations_to_remove) {
    vector_remove_index(object.relocs, *idx);
  }

  // Remove all local label symbols (".Lxxxx")
  Vector(size_t) symbols_to_remove = {0};
  foreach_index (idx, object.symbols) {
    GObjSymbol *sym = object.symbols.data + idx;
    if (strlen(sym->name) > 2 && memcmp(sym->name, ".L", 2) == 0)
      vector_push(symbols_to_remove, idx);
  }
  foreach_rev (size_t, idx, symbols_to_remove) {
    vector_remove_index(object.symbols, *idx);
  }

  generic_object_as_coff_x86_64(&object, "out.obj");
  generic_object_as_elf_x86_64(&object, "out.o");
#endif // x86_64_GENERATE_MACHINE_CODE
}
