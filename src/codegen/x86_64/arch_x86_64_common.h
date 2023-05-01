#ifndef ARCH_X86_64_COMMON_H
#define ARCH_X86_64_COMMON_H

#include <codegen/x86_64/arch_x86_64.h>

#define DEFINE_REGISTER_ENUM(name, ...) REG_##name,
enum Registers_x86_64 {
  REG_NONE,
  FOR_ALL_X86_64_REGISTERS(DEFINE_REGISTER_ENUM)
  REG_COUNT
};
#undef DEFINE_REGISTER_ENUM

#define GENERAL_REGISTER_COUNT 14

extern Register general[GENERAL_REGISTER_COUNT];

/// RDI, RSI, RDX, RCX, R8, R9
#define LINUX_ARGUMENT_REGISTER_COUNT 6
extern Register linux_argument_registers[LINUX_ARGUMENT_REGISTER_COUNT];

/// RCX, RDX, R8, R9
#define MSWIN_ARGUMENT_REGISTER_COUNT 4
extern Register mswin_argument_registers[MSWIN_ARGUMENT_REGISTER_COUNT];

/// RAX, RCX, RDX, R8, R9, R10, R11, RSI, RDI
#define LINUX_CALLER_SAVED_REGISTER_COUNT 9
extern Register linux_caller_saved_registers[LINUX_CALLER_SAVED_REGISTER_COUNT];

/// RAX, RCX, RDX, R8, R9, R10, R11
#define MSWIN_CALLER_SAVED_REGISTER_COUNT 7
extern Register mswin_caller_saved_registers[MSWIN_CALLER_SAVED_REGISTER_COUNT];

/// Types of conditional jump instructions (Jcc).
/// Do NOT reorder these.
typedef enum IndirectJumpType {
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
} IndirectJumpType;

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

extern const char *jump_type_names_x86_64[JUMP_TYPE_COUNT];

typedef enum RegSize {
  r64,
  r32,
  r16,
  r8,
} RegSize;

/// Return the corresponding RegSize enum value to the given amount of
/// bytes (smallest fit). ICE if can not contain.
RegSize regsize_from_bytes(size_t bytes);

/// Return the corresponding byte size of a valid RegSize enum value.
/// ICE if enum value invalid.
size_t regbytes_from_size(enum RegSize r);

const char *regname(RegisterDescriptor reg, enum RegSize size);
const char *regname_from_bytes(RegisterDescriptor reg, size_t bytes);

/// Lookup functions for register names.
const char *register_name(RegisterDescriptor descriptor);
const char *register_name_32(RegisterDescriptor descriptor);
const char *register_name_16(RegisterDescriptor descriptor);
const char *register_name_8(RegisterDescriptor descriptor);

IndirectJumpType negate_jump(IndirectJumpType j);
IndirectJumpType comparison_to_jump_type(enum ComparisonType comparison);

#endif /* ARCH_X86_64_COMMON_H */
