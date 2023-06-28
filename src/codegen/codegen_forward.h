#ifndef CODEGEN_FORWARD_H
#define CODEGEN_FORWARD_H

#include <vector.h>

typedef struct IRInstruction IRInstruction;
typedef struct IRBlock IRBlock;
typedef struct IRFunction IRFunction;
typedef struct IR IR;

typedef usz RegisterDescriptor;
typedef struct RegisterPool RegisterPool;

typedef unsigned Register;
typedef struct CodegenContext CodegenContext;

// TODO: Reorganise codegen specification
// CURRENTLY: Format + Assembly Dialect
// PROPOSED: Architecture + Target [ + Target-specific Value ] or smth like this

typedef enum CodegenArchitecture {
  ARCH_NONE,
  ARCH_X86_64,
  ARCH_COUNT,
} CodegenArchitecture;
#ifndef ARCH_DEFAULT
#  define ARCH_DEFAULT ARCH_X86_64
#endif

typedef enum CodegenTarget {
  TARGET_NONE,
  TARGET_GNU_ASM_ATT,
  TARGET_GNU_ASM_INTEL,
  TARGET_LLVM,
  TARGET_COFF_OBJECT,
  TARGET_ELF_OBJECT,
  TARGET_COUNT,
} CodegenTarget;
#define TARGET_DEFAULT TARGET_GNU_ASM_ATT
#ifndef TARGET_DEFAULT
#  ifdef _WIN32
#    define TARGET_DEFAULT TARGET_COFF_OBJECT
#  else
#    define TARGET_DEFAULT TARGET_ELF_OBJECT
#  endif
#endif

typedef enum CodegenCallingConvention {
  CG_CALL_CONV_MSWIN,
  CG_CALL_CONV_SYSV,
  CG_CALL_CONV_COUNT,

#ifndef _WIN32
  CG_CALL_CONV_DEFAULT = CG_CALL_CONV_SYSV,
#else
  CG_CALL_CONV_DEFAULT = CG_CALL_CONV_MSWIN,
#endif
} CodegenCallingConvention;

typedef enum CodegenLanguage {
  LANG_FUN,
  LANG_IR,
  LANG_COUNT,

  LANG_DEFAULT = LANG_FUN,
} CodegenLanguage;

// Types of comparison to be implemented by codegen backend.
enum ComparisonType {
  COMPARE_EQ,
  COMPARE_NE,
  COMPARE_LT,
  COMPARE_LE,
  COMPARE_GT,
  COMPARE_GE,

  COMPARE_COUNT,
};

/// All instructions that take two arguments.
#define ALL_BINARY_INSTRUCTION_TYPES(F) \
  F(ADD, add)                           \
  F(SUB, sub)                           \
  F(MUL, mul)                           \
  F(DIV, div)                           \
  F(MOD, mod)                           \
                                        \
  F(SHL, shl)                           \
  F(SAR, sar)                           \
  F(SHR, shr)                           \
  F(AND, and)                           \
  F(OR, or)                             \
                                        \
  F(LT, lt)                             \
  F(LE, le)                             \
  F(GT, gt)                             \
  F(GE, ge)                             \
  F(EQ, eq)                             \
  F(NE, ne)

/// Some of these are also used in the parser, and until C implements
/// inheriting from enums (i.e. never), this is the best we can do.
#define ALL_IR_INSTRUCTION_TYPES(F)                              \
  F(IMMEDIATE)                                                   \
  F(CALL)                                                        \
  F(LOAD)                                                        \
                                                                 \
  F(RETURN)                                                      \
  F(BRANCH)                                                      \
  F(BRANCH_CONDITIONAL)                                          \
  F(UNREACHABLE)                                                 \
                                                                 \
  F(PHI)                                                         \
  F(COPY)                                                        \
                                                                 \
  ALL_BINARY_INSTRUCTION_TYPES(F)                                \
                                                                 \
  F(STATIC_REF)                                                  \
  F(FUNC_REF)                                                    \
                                                                 \
  F(ZERO_EXTEND)                                                 \
  F(SIGN_EXTEND)                                                 \
  F(TRUNCATE)                                                    \
                                                                 \
  /** Reinterpret bits as new type **/                           \
  F(BITCAST)                                                     \
                                                                 \
  /** Store data at an address. **/                              \
  F(STORE)                                                       \
                                                                 \
  F(NOT)                                                         \
                                                                 \
  F(PARAMETER)                                                   \
                                                                 \
  /**                                                            \
   * A lot of backends have these instructions, but the IR isn't \
   * generated with them in it.                                  \
   */                                                            \
  F(REGISTER)                                                    \
  F(ALLOCA)                                                      \
  /**                                                            \
   * Literal types (not generated, but used for data transfer    \
   * between frontend and backend)                               \
   */                                                            \
  F(LIT_INTEGER)                                                 \
  F(LIT_STRING)

typedef struct IRStaticVariable IRStaticVariable;
typedef struct IRStackAllocation IRStackAllocation;

typedef struct MIROperandOpRef {
    unsigned int pattern_instruction_index;
    unsigned int operand_index;
} MIROperandOpRef;
typedef unsigned int MIROperandInstRef;

typedef struct MIRFunction MIRFunction;
typedef struct MIRBlock MIRBlock;
typedef struct MIRInstruction MIRInstruction;
typedef Vector(MIRFunction*) MIRFunctionVector;
typedef Vector(MIRBlock*) MIRBlockVector;
typedef Vector(MIRInstruction*) MIRInstructionVector;

typedef struct VReg {
  usz value;
  usz size;
} VReg;

#endif // CODEGEN_FORWARD_H
