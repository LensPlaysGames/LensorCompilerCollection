#include <codegen/x86_64/arch_x86_64_common.h>

#include <codegen/x86_64/arch_x86_64.h>
#include <error.h>

#define REGISTER_NAME_64(ident, name, ...) name,
#define REGISTER_NAME_32(ident, name, name_32, ...) name_32,
#define REGISTER_NAME_16(ident, name, name_32, name_16, ...) name_16,
#define REGISTER_NAME_8(ident, name, name_32, name_16, name_8, ...) name_8,

/// Lookup tables for register names.
#define DEFINE_REGISTER_NAME_LOOKUP_FUNCTION(name, bits)                \
  const char *name(RegisterDescriptor descriptor) {                     \
    static const char* register_names[] =                               \
      { FOR_ALL_X86_64_REGISTERS(REGISTER_NAME_##bits) };               \
    if (descriptor <= 0 || descriptor > REG_COUNT) {                    \
      ICE("ERROR::" #name "(): Could not find register with descriptor of %d\n", descriptor); \
    }                                                                   \
    return register_names[descriptor - 1];                              \
  }


/// Define register_name and friends.
DEFINE_REGISTER_NAME_LOOKUP_FUNCTION(register_name, 64)
DEFINE_REGISTER_NAME_LOOKUP_FUNCTION(register_name_32, 32)
DEFINE_REGISTER_NAME_LOOKUP_FUNCTION(register_name_16, 16)
DEFINE_REGISTER_NAME_LOOKUP_FUNCTION(register_name_8, 8)
#undef REGISTER_NAME_64
#undef REGISTER_NAME_32
#undef REGISTER_NAME_16
#undef REGISTER_NAME_8
#undef DEFINE_REGISTER_NAME_LOOKUP_FUNCTION

Register general[GENERAL_REGISTER_COUNT] = {
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

Register linux_argument_registers[LINUX_ARGUMENT_REGISTER_COUNT] = {
  REG_RDI, REG_RSI, REG_RDX, REG_RCX, REG_R8, REG_R9
};

Register mswin_argument_registers[MSWIN_ARGUMENT_REGISTER_COUNT] = {
  REG_RCX, REG_RDX, REG_R8, REG_R9
};

Register mswin_caller_saved_registers[MSWIN_CALLER_SAVED_REGISTER_COUNT] = {
  REG_RAX, REG_RCX, REG_RDX, REG_R8, REG_R9, REG_R10, REG_R11
};

Register linux_caller_saved_registers[LINUX_CALLER_SAVED_REGISTER_COUNT] = {
  REG_RAX, REG_RCX, REG_RDX, REG_R8, REG_R9, REG_R10, REG_R11, REG_RSI, REG_RDI
};

/// Do NOT reorder these. See enum IndirectJumpType.
const char *jump_type_names_x86_64[JUMP_TYPE_COUNT] = {
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

RegSize regsize_from_bytes(size_t bytes) {
  switch (bytes) {
  case 1: return r8;
  case 2: return r16;
  case 4: return r32;
  case 8: return r64;
  default: ICE("Byte size can not be converted into register size on x86_64: %U", bytes);
  }
}

size_t regbytes_from_size(RegSize r) {
  switch (r) {
  case r8: return 1;
  case r16: return 2;
  case r32: return 4;
  case r64: return 8;
  default: ICE("Register size can not be converted into byte count on x86_64: %d", r);
  }
}

const char *regname(RegisterDescriptor reg, RegSize size) {
  switch (size) {
  case r64: return register_name(reg);
  case r32: return register_name_32(reg);
  case r16: return register_name_16(reg);
  case r8:  return register_name_8(reg);
  default: ICE("Register size can not be converted into name on x86_64: %d", size);
  }
}
const char *regname_from_bytes(RegisterDescriptor reg, size_t bytes) {
  return regname(reg, regsize_from_bytes(bytes));
}

IndirectJumpType negate_jump(IndirectJumpType j) {
  switch (j) {
    case JUMP_TYPE_E: return JUMP_TYPE_NE;
    case JUMP_TYPE_NE: return JUMP_TYPE_E;
    case JUMP_TYPE_L: return JUMP_TYPE_GE;
    case JUMP_TYPE_LE: return JUMP_TYPE_G;
    case JUMP_TYPE_G: return JUMP_TYPE_LE;
    case JUMP_TYPE_GE: return JUMP_TYPE_L;
    default: ICE("Unknown jump type.");
  }
}

IndirectJumpType comparison_to_jump_type(enum ComparisonType comparison) {
  switch (comparison) {
    case COMPARE_EQ: return JUMP_TYPE_E;
    case COMPARE_NE: return JUMP_TYPE_NE;
    case COMPARE_LT: return JUMP_TYPE_L;
    case COMPARE_LE: return JUMP_TYPE_LE;
    case COMPARE_GT: return JUMP_TYPE_G;
    case COMPARE_GE: return JUMP_TYPE_GE;
    default: ICE("Unknown comparison type.");
  }
}

// NOTE
// Normally I don't like putting includes later on in a file, but most
// of the above is freestanding and I'd like to keep it separate.

#include <codegen/machine_ir.h>
#include <codegen/intermediate_representation.h>
#include <opt.h>

StackFrameKind stack_frame_kind(MIRFunction *f) {
  ASSERT(f->origin, "Cannot get stack frame kind of MIRFunction with no IRFunction origin set");
  /// Always emit a frame if we’re not optimising.
  if (!optimise) return FRAME_FULL;

  /// Emit a frame if we have local variables.
  if (f->origin->locals_total_size) return FRAME_FULL;

  /// We need *some* sort of prologue if we don’t use the stack but
  /// still call other functions.
  if (!f->origin->attr_leaf) return FRAME_MINIMAL;

  /// Otherwise, no frame is required.
  return FRAME_NONE;
}
