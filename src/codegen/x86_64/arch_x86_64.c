#include <codegen/x86_64/arch_x86_64.h>

#include <ast.h>
#include <codegen.h>
#include <codegen/x86_64/arch_x86_64_common.h>
#include <codegen/x86_64/arch_x86_64_isel.h>
#include <codegen/codegen_forward.h>
#include <codegen/intermediate_representation.h>
#include <codegen/instruction_selection.h>
#include <codegen/machine_ir.h>
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

Register *caller_saved_registers = NULL;
size_t caller_saved_register_count = 0;

Register *argument_registers = NULL;
size_t argument_register_count = 0;

span unreferenced_block_name = literal_span_raw("");

// Maximum size of parameter that can go in a register vs on the stack.
// TODO: Has to do with calling convention?
static const usz max_register_size = 8;

static const char *setcc_suffixes_x86_64[COMPARE_COUNT] = {
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

/// A SIB byte is needed in the following cases:
///   a. If a memory operand has two pointer or index registers,
///   b. If a memory operand has a scaled index register,
///   c. If a memory operand has the stack pointer (ESP or RSP) as base,
///   d. If a memory operand in 64-bit mode uses a 32-bit sign-extended
///      direct memory address rather than a RIP-relative address.
/// A SIB byte cannot be used in 16-bit addressing mode.
static uint8_t sib_byte(uint8_t scale_factor, uint8_t index, uint8_t base) {
  // Ensure no bits above the amount expected are set.
  ASSERT((scale_factor & (~0b11)) == 0);
  ASSERT((index & (~0b1111)) == 0);
  ASSERT((base & (~0b1111)) == 0);
  return (uint8_t)((scale_factor << 6) | ((index & 0b111) << 3) | base);
}

static void mcode_imm_to_reg(CodegenContext *context, enum Instruction inst, int64_t immediate, RegisterDescriptor destination_register, enum RegSize size) {
  if ((inst == I_SUB || inst == I_ADD) && immediate == 0) return;

  switch (inst) {
  case I_MOV: {

    if (size == r64 && immediate > INT32_MIN && immediate < INT32_MAX)
      size = r32;

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
      // 0x66 + 0xb8+ rw iw
      // Encode a REX prefix if the ModRM register descriptor needs
      // the bit extension.
      uint8_t destination_regbits = regbits(destination_register);
      if (REGBITS_TOP(destination_regbits)) {
        uint8_t rex = rex_byte(false, false, false, REGBITS_TOP(destination_regbits));
        mcode_1(context->object, rex);
      }
      uint8_t op = 0xb8 + rw_encoding(destination_register);
      int16_t imm16 = (int16_t)immediate;
      mcode_2(context->object, 0x66, op);
      mcode_n(context->object, &imm16, 2);
    } break;
    case r32: {
      // Move imm32 to r32
      // 0xb8+ rd id
      // Encode a REX prefix if the ModRM register descriptor needs
      // the bit extension.
      uint8_t destination_regbits = regbits(destination_register);
      if (REGBITS_TOP(destination_regbits)) {
        uint8_t rex = rex_byte(false, false, false, REGBITS_TOP(destination_regbits));
        mcode_1(context->object, rex);
      }
      uint8_t op = 0xb8 + rd_encoding(destination_register);
      int32_t imm32 = (int32_t)immediate;
      mcode_1(context->object, op);
      mcode_n(context->object, &imm32, 4);
    } break;
    case r64: {
      // Move imm64 to r64
      // REX.W + 0xb8+ rd io
      uint8_t rex = rex_byte(true, false, false, REGBITS_TOP(regbits(destination_register)));
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
    } FALLTHROUGH;
    case r32: {
      if (immediate >= INT8_MIN && immediate <= INT8_MAX) {
        // 0x83 /5 ib
        uint8_t op = 0x83;

        // Encode a REX prefix if the ModRM register descriptor needs
        // the bit extension.
        if (REGBITS_TOP(destination_regbits)) {
          uint8_t rex = rex_byte(false, false, false, REGBITS_TOP(destination_regbits));
          mcode_1(context->object, rex);
        }

        int8_t imm8 = (int8_t)immediate;
        mcode_3(context->object, op, modrm, (uint8_t)imm8);

      } else {
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
      }
    } break;

    case r64: {
      if (immediate >= INT8_MIN && immediate <= INT8_MAX) {
        // Subtract imm8 sign extended to 64-bits from r64
        // REX.W + 0x83 /5 ib
        uint8_t op = 0x83;
        uint8_t rex = rex_byte(true, false, false, REGBITS_TOP(destination_regbits));
        int8_t imm8 = (int8_t)immediate;
        mcode_4(context->object, rex, op, modrm, (uint8_t)imm8);
      } else {
        // Subtract imm32 sign extended to 64-bits from r64
        // REX.W + 0x81 /5 id
        uint8_t op = 0x81;
        uint8_t rex = rex_byte(true, false, false, REGBITS_TOP(destination_regbits));
        int32_t imm32 = (int32_t)immediate;

        mcode_3(context->object, rex, op, modrm);
        mcode_n(context->object, &imm32, 4);
      }
    } break;

    } // switch (size)

  } break; // case I_ADD/I_SUB

  default: ICE("ERROR: mcode_imm_to_reg(): Unsupported instruction %d (%s)", inst, instruction_mnemonic(context, inst));
  }
}

static void mcode_imm_to_mem(CodegenContext *context, enum Instruction inst, int64_t immediate, RegisterDescriptor address_register, int64_t offset) {
  switch (inst) {

  case I_SUB: {
    // REX.W 0x81 /5 id

    if (offset == 0) {
      uint8_t address_regbits = regbits(address_register);
      uint8_t rex = rex_byte(true, false, false, REGBITS_TOP(address_regbits));
      uint8_t modrm = modrm_byte(0b00, 5, address_regbits);
      int32_t imm32 = (int32_t)immediate;

      mcode_3(context->object, rex, 0x81, modrm);
      if (address_register == REG_RSP) {
        /// Scaling Factor == 0b00  ->  1
        /// Index == 0b100  ->  None
        /// Base == RSP bits (0b100)
        mcode_1(context->object, sib_byte(0b00, 0b100, address_regbits));
      }
      mcode_n(context->object, &imm32, 4);
      break;
    }

    uint8_t address_regbits = regbits(address_register);
    uint8_t rex = rex_byte(true, false, false, REGBITS_TOP(address_regbits));
    uint8_t modrm = modrm_byte(0b10, 5, address_regbits);
    int32_t imm32 = (int32_t)immediate;
    int32_t disp32 = (int32_t)offset;

    mcode_3(context->object, rex, 0x81, modrm);
    if (address_register == REG_RSP) {
      /// Scaling Factor == 0b00  ->  1
      /// Index == 0b100  ->  None
      /// Base == RSP bits (0b100)
      mcode_1(context->object, sib_byte(0b00, 0b100, address_regbits));
    }
    mcode_n(context->object, &disp32, 4);
    mcode_n(context->object, &imm32, 4);

  } break; // case I_SUB

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
    } FALLTHROUGH;
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
      } FALLTHROUGH;
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
      } FALLTHROUGH;
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
      } FALLTHROUGH;
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
    } FALLTHROUGH;
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
    } FALLTHROUGH;
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
    } FALLTHROUGH;
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
    } FALLTHROUGH;
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
    } FALLTHROUGH;
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
    } FALLTHROUGH;
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
    } FALLTHROUGH;
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
    } FALLTHROUGH;
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
  switch (inst) {

  case I_MOV: {

    switch (size) {
    case r8: {
      // 0x88 /r
      if (address_register == REG_RIP) {
        uint8_t source_regbits = regbits(source_register);
        // RIP-relative ModRM byte
        // Mod == 0b00
        // Reg == Source Register
        // R/M == 0b101
        uint8_t modrm = modrm_byte(0b00, source_regbits, 0b101);
        if (REGBITS_TOP(source_regbits)) {
          uint8_t rex = rex_byte(false, REGBITS_TOP(source_regbits), false, false);
          mcode_1(context->object, rex);
        }
        mcode_2(context->object, 0x88, modrm);

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
      uint8_t source_regbits = regbits(source_register);
      uint8_t address_regbits = regbits(address_register);
      // Mod == 0b10  ->  (register + disp32)
      // Reg == Source
      // R/M == Address
      uint8_t modrm = modrm_byte(0b10, source_regbits, address_regbits);
      if (REGBITS_TOP(source_regbits) || REGBITS_TOP(address_regbits)) {
        uint8_t rex = rex_byte(false, REGBITS_TOP(source_regbits), false, REGBITS_TOP(address_regbits));
        mcode_1(context->object, rex);
      }
      mcode_2(context->object, 0x88, modrm);

      // Generate disp32 relocation!
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
      // 0x66 + 0x89 /r
      mcode_1(context->object, 0x66);
    } FALLTHROUGH;
    case r32: {
      // 0x89 /r
      if (address_register == REG_RIP) {
        uint8_t source_regbits = regbits(source_register);
        // RIP-relative ModRM byte
        // Mod == 0b00
        // Reg == Source Register
        // R/M == 0b101
        uint8_t modrm = modrm_byte(0b00, source_regbits, 0b101);
        if (REGBITS_TOP(source_regbits)) {
          uint8_t rex = rex_byte(false, REGBITS_TOP(source_regbits), false, false);
          mcode_1(context->object, rex);
        }

        mcode_2(context->object, 0x89, modrm);

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
      uint8_t source_regbits = regbits(source_register);
      uint8_t address_regbits = regbits(address_register);
      // Mod == 0b10  ->  (register + disp32)
      // Reg == Source
      // R/M == Address
      uint8_t modrm = modrm_byte(0b10, source_regbits, address_regbits);
      if (REGBITS_TOP(source_regbits) || REGBITS_TOP(address_regbits)) {
        uint8_t rex = rex_byte(false, REGBITS_TOP(source_regbits), false, REGBITS_TOP(address_regbits));
        mcode_1(context->object, rex);
      }
      mcode_2(context->object, 0x89, modrm);

      // Generate disp32 relocation!
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
      // REX.W + 0x89 /r
      if (address_register == REG_RIP) {
        uint8_t source_regbits = regbits(source_register);
        // RIP-relative ModRM byte
        // Mod == 0b00
        // Reg == Source Register
        // R/M == 0b101
        uint8_t modrm = modrm_byte(0b00, source_regbits, 0b101);
        uint8_t rex = rex_byte(true, REGBITS_TOP(source_regbits), false, false);
        mcode_3(context->object, rex, 0x89, modrm);

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
      uint8_t source_regbits = regbits(source_register);
      uint8_t address_regbits = regbits(address_register);
      uint8_t rex = rex_byte(true, REGBITS_TOP(source_regbits), false, REGBITS_TOP(address_regbits));
      // Mod == 0b10  ->  (register + disp32)
      // Reg == Source
      // R/M == Address
      uint8_t modrm = modrm_byte(0b10, source_regbits, address_regbits);
      mcode_3(context->object, rex, 0x89, modrm);

      // Generate disp32 relocation!
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
  } FALLTHROUGH;
  case I_CDQ: { // 0x99
    uint8_t op = 0x99;
    mcode_1(context->object, op);
  } break;
  case I_CQO: { // REX.W + 0x99
    uint8_t op = 0x99;
    uint8_t rexw = rexw_byte();
    mcode_2(context->object, rexw, op);
  } break;

  default:
    ICE("ERROR: mcode_none(): Unsupported instruction %d (%s)", inst, instruction_mnemonic(context, inst));
  }
}

static void mcode_setcc(CodegenContext *context, enum ComparisonType comparison_type, RegisterDescriptor value_register) {
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
}

static void mcode_jcc(CodegenContext *context, IndirectJumpType type, const char *label) {
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
}

#endif // X86_64_GENERATE_MACHINE_CODE

static void femit_imm_to_reg(CodegenContext *context, enum Instruction inst, int64_t immediate, RegisterDescriptor destination_register, enum RegSize size) {
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

static void femit_mem_to_reg(CodegenContext *context, enum Instruction inst, RegisterDescriptor address_register, int64_t offset, RegisterDescriptor destination_register, RegSize size) {
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
  default: ICE("ERROR: femit_reg_shift(): Unsupported dialect %d for shift instruction", context->dialect);
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
  default: ICE("ERROR: femit_name(): Unsupported dialect %d for CALL/JMP instruction", context->dialect);
  }
}

static void femit_setcc(CodegenContext *context, enum ComparisonType comparison_type, RegisterDescriptor value_register) {
  const char *mnemonic = instruction_mnemonic(context, I_SETCC);
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
  default: ICE("ERROR: femit_setcc(): Unsupported dialect %d", context->dialect);
  }

}



static void femit_jcc(CodegenContext *context, IndirectJumpType type, const char *label) {
      const char *mnemonic = instruction_mnemonic(context, I_JCC);

      switch (context->dialect) {
        case CG_ASM_DIALECT_ATT:
        case CG_ASM_DIALECT_INTEL:
          fprint(context->code, "    %s%s %s\n",
              mnemonic, jump_type_names_x86_64[type], label);
          break;
        default: ICE("ERROR: femit_direct_branch(): Unsupported dialect %d", context->dialect);
      }
}

static void femit_none(CodegenContext *context, enum Instruction instruction) {
  switch (instruction) {
    case I_RET:
    case I_CWD:
    case I_CDQ:
    case I_CQO: {
      const char *mnemonic = instruction_mnemonic(context, instruction);
      fprint(context->code, "    %s\n", mnemonic);
    } break;

    default:
      ICE("Unhandled instruction in femit_none(): %d (%s)\n"
          "  Consider using femit_x() or femit_x_to_x()",
          instruction, instruction_mnemonic(context, instruction));
  }
}

/// Creates a context for the CG_FMT_x86_64_MSWIN architecture.
CodegenContext *codegen_context_x86_64_mswin_create() {
  caller_saved_register_count = MSWIN_CALLER_SAVED_REGISTER_COUNT;
  caller_saved_registers = mswin_caller_saved_registers;
  argument_register_count = MSWIN_ARGUMENT_REGISTER_COUNT;
  argument_registers = mswin_argument_registers;

  CodegenContext *cg_ctx = calloc(1,sizeof(CodegenContext));
  cg_ctx->format = CG_FMT_x86_64_GAS;
  cg_ctx->call_convention = CG_CALL_CONV_MSWIN;
  cg_ctx->dialect = CG_ASM_DIALECT_ATT;
  return cg_ctx;
}

/// Creates a context for the x86_64/CG_CALL_CONV_LINUX.
CodegenContext *codegen_context_x86_64_linux_create() {
  caller_saved_register_count = LINUX_CALLER_SAVED_REGISTER_COUNT;
  caller_saved_registers = linux_caller_saved_registers;
  argument_register_count = LINUX_ARGUMENT_REGISTER_COUNT;
  argument_registers = linux_argument_registers;

  CodegenContext *cg_ctx = calloc(1,sizeof(CodegenContext));
  cg_ctx->format = CG_FMT_x86_64_GAS;
  cg_ctx->call_convention = CG_CALL_CONV_LINUX;
  cg_ctx->dialect = CG_ASM_DIALECT_ATT;
  return cg_ctx;
}

/// Free a context created by codegen_context_x86_64_mswin_create.
void codegen_context_x86_64_mswin_free(CodegenContext *ctx) {
  (void)ctx;
}

void codegen_context_x86_64_linux_free(CodegenContext *ctx) {
  (void)ctx;
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
    default:
      break;

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
      default: ICE("Unhandled calling convention in x86_64 lowering of calls");
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
          vector_insert_after(instruction->call.arguments, load1, instruction->call.arguments.data + *i);
          mark_used(load1, instruction);
          vector_insert_after(instruction->call.arguments, load2, instruction->call.arguments.data + *i);
          mark_used(load2, instruction);

        }
      } break;
      case CG_CALL_CONV_MSWIN: {
        usz idx = 0;

        // Lower aggregates in possible-register arguments by allocating a copy of them on the stack.
        foreach_ptr (IRInstruction *, argument, instruction->call.arguments) {
          if (idx >= argument_register_count) break;
          Type *type = type_canonical(argument->type);
          if ((type->kind == TYPE_STRUCT || type->kind == TYPE_ARRAY) && type_sizeof(type) > 8) {
            instruction->call.arguments.data[idx] = alloca_copy_of(context, argument, instruction);
          }
          ++idx;
        }

        // Lower all arguments not able to go in a register by allocating a copy of them on the stack.
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
        if (instruction->imm >= argument_register_count) {

          // Calculate offset to caller-allocated stack memory for large parameters.

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
          if (type_sizeof(type) > 8)
            address->type = ast_make_type_pointer(context->ast, instruction->type->source_location, instruction->type);
          else address->type = instruction->type;
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
    }
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
      mask |= ((usz)1 << REG_RAX);
      mask |= ((usz)1 << REG_RDX);
    }
  }

  switch(instruction->kind) {
  case IR_SHL:
  case IR_SHR:
  case IR_SAR:
    mask |= ((usz)1 << REG_RCX);
    break;
  case IR_DIV:
  case IR_MOD:
    mask |= ((usz)1 << REG_RAX);
    mask |= ((usz)1 << REG_RDX);
    break;
  case IR_CALL: // FIXME: This seems specific to calling convention...
    mask |= ((usz)1 << REG_RAX);
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
    Section sec_rodata = {0};
    sec_rodata.name = ".rodata";
    vector_push(object.sections, sec_rodata);
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
  Section *sec_rodata = get_section_by_name(object.sections, ".rodata");
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

#ifdef X86_64_GENERATE_MACHINE_CODE
        // Create symbol for var->name at current offset within the .rodata section
        GObjSymbol sym = {0};
        sym.type = GOBJ_SYMTYPE_STATIC;
        sym.name = strdup(var->name.data);
        sym.section_name = strdup(".rodata");
        sym.byte_offset = sec_initdata->data.bytes.size;
        vector_push(object.symbols, sym);
        // Write string bytes to .rodata section
        sec_write_n(sec_rodata, var->init->str.data, var->init->str.size);
        sec_write_1(sec_rodata, 0);
#endif // x86_64_GENERATE_MACHINE_CODE

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

//  foreach_ptr (IRFunction*, f, context->functions)
//    if (!f->is_extern) allocate_registers(f, &desc);

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

  // FUNCTION NAME MANGLING
  foreach_ptr (IRFunction*, function, context->functions) {
    // Don't mangle external function(s).
    if (!function->attr_nomangle) mangle_function_name(function);
  }

  MIRFunctionVector machine_instructions_from_ir = mir_from_ir(context);

  foreach_ptr (MIRFunction*, f, machine_instructions_from_ir) {
    print_mir_function(f);
  }

  print("================ ISel ================\n");

  // TODO: Either embed x86_64 isel or somehow make this path knowable (i.e. via install).
  const char isel_filepath[] = "src/codegen/x86_64/arch_x86_64.isel";
  ISelPatterns patterns =  isel_parse_file(isel_filepath);

  //isel_print_patterns(&patterns, mir_x86_64_opcode_mnemonic);

  isel_do_selection(machine_instructions_from_ir, patterns);

  // TODO: Calculate defining use of each vreg, or smth.

  foreach_ptr (MIRFunction*, f, machine_instructions_from_ir) {
    print_mir_function_with_mnemonic(f, mir_x86_64_opcode_mnemonic);
  }

  isel_patterns_delete(&patterns);

  // RA -- Register Allocation
  foreach_ptr (MIRFunction*, f, machine_instructions_from_ir) {
    allocate_registers(f, &desc);
  }

  // EMIT ASSEMBLY CODE
  { // Emit entry
    fprint(context->code,
           "%s"
         ".section .text\n",
           context->dialect == CG_ASM_DIALECT_INTEL ? ".intel_syntax noprefix\n" : "");

    fprint(context->code, "\n");
    foreach_ptr (MIRFunction*, function, machine_instructions_from_ir) {
      if (!function->origin->attr_global) continue;
      fprint(context->code, ".global %S\n", function->name);
    }
  }
  foreach_ptr (MIRFunction*, function, machine_instructions_from_ir) {
    // Generate function entry label if function has definition.
    if (!function->origin->is_extern)
      fprint(context->code, "\n%s:\n", function->name.data);

    // Calculate stack offsets
    isz frame_offset = 0;
    foreach (MIRFrameObject, fo, function->frame_objects) {
      frame_offset -= fo->size;
      fo->offset = frame_offset;
    }

    foreach_ptr (MIRBlock*, block, function->blocks) {
      /// Emit block symbol if it is used.
      if (block->name.size)
        fprint(context->code, "%s:\n", block->name.data);

      foreach_ptr (MIRInstruction*, instruction, block->instructions) {
        switch (instruction->opcode) {

        case MX64_LEA: {
          MIROperand *address = mir_get_op(instruction, 0);
          MIROperand *local = mir_get_op(instruction, 1);
          MIROperand *destination = mir_get_op(instruction, 2);
          ASSERT(address->kind == MIR_OP_REGISTER && address->value.reg.value == REG_RBP,
                 "LEA expected first operand to be address register");
          ASSERT(local->kind == MIR_OP_LOCAL_REF && local->value.local_ref != (usz)-1,
                 "LEA expected second operand to be frame object reference");
          ASSERT(destination->kind == MIR_OP_REGISTER,
                 "LEA requires third operand to be a destination register");
          femit_mem_to_reg(context, I_LEA, address->value.reg.value, mir_get_frame_object(function, local->value.local_ref)->offset, destination->value.reg.value, destination->value.reg.size);
        } break; // case MX64_LEA

        case MX64_CALL: {
          MIROperand *dst = mir_get_op(instruction, 0);

          switch (dst->kind) {

          case MIR_OP_NAME: {
            print("|> call %s\n", dst->value.name);
            femit_name(context, I_CALL, dst->value.name);
          } break;

          case MIR_OP_BLOCK: {
            print("|> call %s\n", dst->value.block->name.data);
            femit_name(context, I_CALL, dst->value.block->name.data);
          } break;

          case MIR_OP_FUNCTION: {
            print("|> call %s\n", dst->value.function->name.data);
            femit_name(context, I_CALL, dst->value.function->name.data);
          } break;

          default: ICE("Unhandled operand kind in CALL: %d (%s)", dst->kind, mir_operand_kind_string(dst->kind));

          } // switch (dst->kind)

        } break; // case MX64_CALL

        case MX64_MOV: {
          if (mir_operand_kinds_match(instruction, 2, MIR_OP_IMMEDIATE, MIR_OP_REGISTER)) {
            // imm to reg | imm, dst
            MIROperand *imm = mir_get_op(instruction, 0);
            MIROperand *reg = mir_get_op(instruction, 1);
            if (!reg->value.reg.size) {
              putchar('\n');
              print_mir_instruction_with_mnemonic(instruction, mir_x86_64_opcode_mnemonic);
              print("%35WARNING%m: Zero sized register, assuming 64-bit...\n");
              putchar('\n');
              reg->value.reg.size = r64;
            }
            femit_imm_to_reg(context, I_MOV, imm->value.imm, reg->value.reg.value, reg->value.reg.size);
          } else if (mir_operand_kinds_match(instruction, 2, MIR_OP_IMMEDIATE, MIR_OP_LOCAL_REF)) {
            // imm to mem (local) | imm, local
            MIROperand *imm = mir_get_op(instruction, 0);
            MIROperand *local = mir_get_op(instruction, 1);
            ASSERT(local->value.local_ref < function->frame_objects.size,
                   "MX64_MOV(imm, local): local index %d is greater than amount of frame objects in function: %Z",
                   (int)local->value.local_ref, function->frame_objects.size);
            femit_imm_to_mem(context, I_MOV, imm->value.imm, REG_RBP, function->frame_objects.data[local->value.local_ref].offset);
          } else if (mir_operand_kinds_match(instruction, 2, MIR_OP_REGISTER, MIR_OP_REGISTER)) {
            // reg to reg | src, dst
            MIROperand *src = mir_get_op(instruction, 0);
            MIROperand *dst = mir_get_op(instruction, 1);
            if (!src->value.reg.size) {
              putchar('\n');
              print_mir_instruction_with_mnemonic(instruction, mir_x86_64_opcode_mnemonic);
              print("%35WARNING%m: Source is a zero sized register, assuming 64-bit...\n");
              putchar('\n');
              src->value.reg.size = r64;
            }
            if (!dst->value.reg.size) {
              putchar('\n');
              print_mir_instruction_with_mnemonic(instruction, mir_x86_64_opcode_mnemonic);
              print("%35WARNING%m: Source is a zero sized register, assuming 64-bit...\n");
              putchar('\n');
              dst->value.reg.size = r64;
            }
            femit_reg_to_reg(context, I_MOV,
                             src->value.reg.value, src->value.reg.size,
                             dst->value.reg.value, dst->value.reg.size);
          } else if (mir_operand_kinds_match(instruction, 2, MIR_OP_REGISTER, MIR_OP_LOCAL_REF)) {
            // reg to mem (local) | src, local
            MIROperand *reg = mir_get_op(instruction, 0);
            MIROperand *local = mir_get_op(instruction, 1);

            ASSERT(function->frame_objects.size,
                   "Cannot reference local at index %Z when there are no frame objects in this function",
                   local->value.local_ref);
            ASSERT(local->value.local_ref < function->frame_objects.size,
                   "Local reference index %Z is larger than maximum possible local index %Z",
                   local->value.local_ref, function->frame_objects.size - 1);

            femit_reg_to_mem(context, I_MOV, reg->value.reg.value, reg->value.reg.size,
                             REG_RBP, function->frame_objects.data[local->value.local_ref].offset);
          } else if (mir_operand_kinds_match(instruction, 2, MIR_OP_LOCAL_REF, MIR_OP_REGISTER)) {
            // mem (local) to reg | local, src
            MIROperand *local = mir_get_op(instruction, 0);
            MIROperand *reg = mir_get_op(instruction, 1);

            ASSERT(function->frame_objects.size,
                   "Cannot reference local at index %Z when there are no frame objects in this function",
                   local->value.local_ref);
            ASSERT(local->value.local_ref < function->frame_objects.size,
                   "Local reference index %Z is larger than maximum possible local index %Z",
                   local->value.local_ref, function->frame_objects.size - 1);

            femit_mem_to_reg(context, I_MOV,
                             REG_RBP, function->frame_objects.data[local->value.local_ref].offset,
                             reg->value.reg.value, reg->value.reg.size);
          } else if (mir_operand_kinds_match(instruction, 3, MIR_OP_IMMEDIATE, MIR_OP_REGISTER, MIR_OP_IMMEDIATE)) {
            // imm to mem | imm, addr, offset
            MIROperand *imm = mir_get_op(instruction, 0);
            MIROperand *reg_address = mir_get_op(instruction, 1);
            MIROperand *offset = mir_get_op(instruction, 2);
            femit_imm_to_mem(context, I_MOV, imm->value.imm, reg_address->value.reg.value, offset->value.imm);
          } else if (mir_operand_kinds_match(instruction, 3, MIR_OP_REGISTER, MIR_OP_REGISTER, MIR_OP_IMMEDIATE)) {
            // reg to mem | src, addr, offset
            MIROperand *reg_source = mir_get_op(instruction, 0);
            MIROperand *reg_address = mir_get_op(instruction, 1);
            MIROperand *offset = mir_get_op(instruction, 2);
            femit_reg_to_mem(context, I_MOV, reg_source->value.reg.value, reg_source->value.reg.size, reg_address->value.reg.value, offset->value.imm);
          } else if (mir_operand_kinds_match(instruction, 3, MIR_OP_REGISTER, MIR_OP_IMMEDIATE, MIR_OP_REGISTER)) {
            // mem to reg | addr, offset, dst
            MIROperand *reg_address = mir_get_op(instruction, 0);
            MIROperand *offset = mir_get_op(instruction, 1);
            MIROperand *reg_dst = mir_get_op(instruction, 2);
            femit_mem_to_reg(context, I_MOV, reg_address->value.reg.value, offset->value.imm, reg_dst->value.reg.value, reg_dst->value.reg.size);
          } else {
            print("\n\nUNHANDLED INSTRUCTION:\n");
            print_mir_instruction_with_mnemonic(instruction, mir_x86_64_opcode_mnemonic);
            ICE("[x86_64/CodeEmission]: Unhandled instruction, sorry");
          }

        } break; // case MX64_MOV

        case MX64_ADD: {
          if (mir_operand_kinds_match(instruction, 2, MIR_OP_IMMEDIATE, MIR_OP_REGISTER)) {
            // imm to reg | imm, dst
            MIROperand *imm = mir_get_op(instruction, 0);
            MIROperand *reg = mir_get_op(instruction, 1);
            if (!reg->value.reg.size) {
              putchar('\n');
              print_mir_instruction_with_mnemonic(instruction, mir_x86_64_opcode_mnemonic);
              print("%35WARNING%m: Zero sized register, assuming 64-bit...\n");
              putchar('\n');
              reg->value.reg.size = r64;
            }
            femit_imm_to_reg(context, I_ADD, imm->value.imm, reg->value.reg.value, reg->value.reg.size);
          } else if (mir_operand_kinds_match(instruction, 2, MIR_OP_REGISTER, MIR_OP_REGISTER)) {
            // reg to reg | src, dst
            MIROperand *src = mir_get_op(instruction, 0);
            MIROperand *dst = mir_get_op(instruction, 1);
            femit_reg_to_reg(context, I_ADD, src->value.reg.value, src->value.reg.size, dst->value.reg.value, dst->value.reg.size);
          } else {
            print("\n\nUNHANDLED INSTRUCTION:\n");
            print_mir_instruction_with_mnemonic(instruction, mir_x86_64_opcode_mnemonic);
            ICE("[x86_64/CodeEmission]: Unhandled instruction, sorry");
          }
        } break; // case MX64_ADD

        case MX64_RET:
          femit_none(context, I_RET);
          break;

        default: {
          print("Unhandled opcode: %d (%s)\n", instruction->opcode, mir_x86_64_opcode_mnemonic(instruction->opcode));
        } break;
        }
      }
    }
  }

  // EMIT MACHINE CODE (GENERAL OBJECT FILE)
  foreach_ptr (MIRFunction*, function, machine_instructions_from_ir) {
    { // Function symbol
      GObjSymbol sym = {0};
      sym.type = function->origin->is_extern ? GOBJ_SYMTYPE_EXTERNAL : GOBJ_SYMTYPE_FUNCTION;
      sym.name = strdup(function->name.data);
      sym.section_name = strdup(code_section(&object)->name);
      sym.byte_offset = code_section(&object)->data.bytes.size;
      vector_push(object.symbols, sym);
    }
    foreach_ptr (MIRBlock*, block, function->blocks) {
      { // Block label symbol
        GObjSymbol sym = {0};
        sym.type = GOBJ_SYMTYPE_STATIC;
        sym.name = strdup(block->name.data);
        sym.section_name = strdup(code_section(context->object)->name);
        sym.byte_offset = code_section(context->object)->data.bytes.size;
        vector_push(context->object->symbols, sym);
      }
      foreach_ptr (MIRInstruction*, instruction, block->instructions) {
        switch (instruction->opcode) {

        case MX64_ADD: {
          if (mir_operand_kinds_match(instruction, 2, MIR_OP_IMMEDIATE, MIR_OP_REGISTER)) {
            // imm to reg | imm, dst
            MIROperand *imm = mir_get_op(instruction, 0);
            MIROperand *reg = mir_get_op(instruction, 1);
            mcode_imm_to_reg(context, I_ADD, imm->value.imm, reg->value.reg.value, reg->value.reg.size);
          } else if (mir_operand_kinds_match(instruction, 2, MIR_OP_REGISTER, MIR_OP_REGISTER)) {
            // reg to reg | src, dst
            MIROperand *src = mir_get_op(instruction, 0);
            MIROperand *dst = mir_get_op(instruction, 1);
            mcode_reg_to_reg(context, I_ADD, src->value.reg.value, src->value.reg.size, dst->value.reg.value, dst->value.reg.size);
          } else {
            print("\n\nUNHANDLED INSTRUCTION:\n");
            print_mir_instruction_with_mnemonic(instruction, mir_x86_64_opcode_mnemonic);
            ICE("[x86_64/CodeEmission]: Unhandled instruction, sorry");
          }
        } break; // case MX64_ADD

        case MX64_MOV: {
          if (mir_operand_kinds_match(instruction, 2, MIR_OP_IMMEDIATE, MIR_OP_REGISTER)) {
            // imm to reg | imm, dst
            MIROperand *imm = mir_get_op(instruction, 0);
            MIROperand *reg = mir_get_op(instruction, 1);
            mcode_imm_to_reg(context, I_MOV, imm->value.imm, reg->value.reg.value, reg->value.reg.size);
          } else if (mir_operand_kinds_match(instruction, 2, MIR_OP_IMMEDIATE, MIR_OP_LOCAL_REF)) {
            // imm to mem (local) | imm, local
            MIROperand *imm = mir_get_op(instruction, 0);
            MIROperand *local = mir_get_op(instruction, 1);
            ASSERT(local->value.local_ref < function->frame_objects.size,
                   "MX64_MOV(imm, local): local index %d is greater than amount of frame objects in function: %Z",
                   (int)local->value.local_ref, function->frame_objects.size);
            mcode_imm_to_mem(context, I_MOV, imm->value.imm, REG_RBP, function->frame_objects.data[local->value.local_ref].offset);
          } else if (mir_operand_kinds_match(instruction, 2, MIR_OP_REGISTER, MIR_OP_REGISTER)) {
            // reg to reg | src, dst
            MIROperand *src = mir_get_op(instruction, 0);
            MIROperand *dst = mir_get_op(instruction, 1);
            mcode_reg_to_reg(context, I_MOV, src->value.reg.value, src->value.reg.size, dst->value.reg.value, dst->value.reg.size);
          } else if (mir_operand_kinds_match(instruction, 2, MIR_OP_REGISTER, MIR_OP_LOCAL_REF)) {
            // reg to mem (local) | src, local
            MIROperand *reg = mir_get_op(instruction, 0);
            MIROperand *local = mir_get_op(instruction, 1);

            ASSERT(function->frame_objects.size,
                   "Cannot reference local at index %Z when there are no frame objects in this function",
                   local->value.local_ref);
            ASSERT(local->value.local_ref < function->frame_objects.size,
                   "Local reference index %Z is larger than maximum possible local index %Z",
                   local->value.local_ref, function->frame_objects.size - 1);

            mcode_reg_to_mem(context, I_MOV, reg->value.reg.value, reg->value.reg.size,
                             REG_RBP, function->frame_objects.data[local->value.local_ref].offset);
          } else if (mir_operand_kinds_match(instruction, 3, MIR_OP_IMMEDIATE, MIR_OP_REGISTER, MIR_OP_IMMEDIATE)) {
            // imm to mem | imm, addr, offset
            MIROperand *imm = mir_get_op(instruction, 0);
            MIROperand *reg_address = mir_get_op(instruction, 1);
            MIROperand *offset = mir_get_op(instruction, 2);
            mcode_imm_to_mem(context, I_MOV, imm->value.imm, reg_address->value.reg.value, offset->value.imm);
          } else if (mir_operand_kinds_match(instruction, 3, MIR_OP_REGISTER, MIR_OP_REGISTER, MIR_OP_IMMEDIATE)) {
            // reg to mem | src, addr, offset
            MIROperand *reg_source = mir_get_op(instruction, 0);
            MIROperand *reg_address = mir_get_op(instruction, 1);
            MIROperand *offset = mir_get_op(instruction, 2);
            mcode_reg_to_mem(context, I_MOV, reg_source->value.reg.value, reg_source->value.reg.size, reg_address->value.reg.value, offset->value.imm);
          } else if (mir_operand_kinds_match(instruction, 3, MIR_OP_REGISTER, MIR_OP_IMMEDIATE, MIR_OP_REGISTER)) {
            // mem to reg | addr, offset, dst
            MIROperand *reg_address = mir_get_op(instruction, 0);
            MIROperand *offset = mir_get_op(instruction, 1);
            MIROperand *reg_dst = mir_get_op(instruction, 2);
            mcode_mem_to_reg(context, I_MOV, reg_address->value.reg.value, offset->value.imm, reg_dst->value.reg.value, reg_dst->value.reg.size);
          } else if (mir_operand_kinds_match(instruction, 2, MIR_OP_LOCAL_REF, MIR_OP_REGISTER)) {
            // mem (local) to reg | local, src
            MIROperand *local = mir_get_op(instruction, 0);
            MIROperand *reg = mir_get_op(instruction, 1);

            ASSERT(function->frame_objects.size,
                   "Cannot reference local at index %Z when there are no frame objects in this function",
                   local->value.local_ref);
            ASSERT(local->value.local_ref < function->frame_objects.size,
                   "Local reference index %Z is larger than maximum possible local index %Z",
                   local->value.local_ref, function->frame_objects.size - 1);

            mcode_mem_to_reg(context, I_MOV,
                             REG_RBP, function->frame_objects.data[local->value.local_ref].offset,
                             reg->value.reg.value, reg->value.reg.size);
          } else {
            print("\n\nUNHANDLED INSTRUCTION:\n");
            print_mir_instruction_with_mnemonic(instruction, mir_x86_64_opcode_mnemonic);
            ICE("[x86_64/CodeEmission]: Unhandled instruction, sorry");
          }

        } break; // case MX64_MOV

        case MX64_CALL: {
          MIROperand *dst = mir_get_op(instruction, 0);

          switch (dst->kind) {

          case MIR_OP_NAME: {
            mcode_name(context, I_CALL, dst->value.name);
          } break;
          case MIR_OP_BLOCK: {
            mcode_name(context, I_CALL, dst->value.block->name.data);
          } break;
          case MIR_OP_FUNCTION: {
            mcode_name(context, I_CALL, dst->value.function->name.data);
          } break;

          default: ICE("Unhandled operand kind in CALL: %d (%s)", dst->kind, mir_operand_kind_string(dst->kind));

          } // switch (dst->kind)

        } break;

        case MX64_RET: {
          mcode_none(context, I_RET);
        } break;

        default: {
          print("Unhandled opcode (mcode): %d (%s)\n", instruction->opcode, mir_x86_64_opcode_mnemonic(instruction->opcode));
        } break;
        }
      }
    }
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

  generic_object_delete(&object);

#endif // x86_64_GENERATE_MACHINE_CODE
}
