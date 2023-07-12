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

typedef Vector(IRBlock *) IRBlockVector;

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

/// All instructions that take two arguments that aren’t comparisons.
#define ALL_BINARY_INSTRUCTION_TYPES_EXCEPT_COMPARISONS(F) \
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


#define ALL_BINARY_COMPARISON_TYPES(F)  \
  F(LT, lt)                             \
  F(LE, le)                             \
  F(GT, gt)                             \
  F(GE, ge)                             \
  F(EQ, eq)                             \
  F(NE, ne)

/// All instructions that take two arguments.
#define ALL_BINARY_INSTRUCTION_TYPES(F) \
    ALL_BINARY_INSTRUCTION_TYPES_EXCEPT_COMPARISONS(F) \
    ALL_BINARY_COMPARISON_TYPES(F)

/// Some of these are also used in the parser, and until C implements
/// inheriting from enums (i.e. never), this is the best we can do.
#define ALL_IR_INSTRUCTION_TYPES(F)                              \
  F(IMMEDIATE)                                                   \
  F(CALL)                                                        \
  F(INTRINSIC)                                                   \
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
  F(LIT_STRING)                                                  \
                                                                 \
  /** Invalid value. This is used to facilitate deletion.**/     \
  F(POISON)

/// Function attributes shared by the frontend and backend.
#define SHARED_FUNCTION_ATTRIBUTES(F)                                    \
  F(CONST, const)       /** Function does not read or write memory. **/  \
  F(FLATTEN, flatten)   /** Inline all callees, if possible. **/         \
  F(INLINE, inline)     /** Always inline, not a hint like in C. **/     \
  F(NOINLINE, noinline) /** Never inline this function. **/              \
  F(NOMANGLE, nomangle) /** Do not mangle the name of this function. **/ \
  F(NORETURN, noreturn) /** This function does not return. **/           \
  F(PURE, pure)         /** This function has no side effects. **/

/// Function attributes that are only used by the frontend.
#define FRONTEND_FUNCTION_ATTRIBUTES(F)                              \
  F(DISCARDABLE, discardable) /** Function result may be unused. **/ \
  F(USED, used)               /** This function is used; do not deleted it **/

/// Function attributes that are only used by the backend.
#define IR_FUNCTION_ATTRIBUTES(F) \
  F(LEAF, leaf) /** Function does not call other functions. **/

/// Linkage of a (global) symbol.
typedef enum SymbolLinkage {
  /// Local variable.
  ///
  /// This is just a dummy value that is used for local variables
  /// only. In particular, a top-level declaration that is marked
  /// as local is treated as a variable local to the top-level
  /// function.
  LINKAGE_LOCALVAR,

  /// Not exported. Will be deleted if unused.
  ///
  /// This is used for variables and functions that are defined in
  /// and local to this module. A variable or function marked with
  /// this attribute will be *deleted* if it is not used anywhere
  /// and will not be accessible to outside code.
  LINKAGE_INTERNAL,

  /// Like internal, but will not be deleted.
  ///
  /// This is for variables and functions that are not really exported
  /// and behave just like internal variables and functions, except that
  /// their name will be included in the object file’s symbol table.
  LINKAGE_USED,

  /// Exported. May be used by other modules.
  ///
  /// This is used for variables and functions that are defined in
  /// this module and exported. Variables and functions marked with
  /// this attribute will not be deleted even if they are not
  /// referenced anywhere.
  LINKAGE_EXPORTED,

  /// Imported from another module or from C.
  ///
  /// This is used for variables and functions imported from outside
  /// code, whether via importing an Intercept module or simply declaring
  /// an external symbol. This linkage type means that the object is
  /// not defined in this module and that it will be made accessible at
  /// link time only. However, this module will not export the symbol.
  LINKAGE_IMPORTED,

  /// Imported *and* exported.
  ///
  /// This sort of combines exported and imported in that it means that
  /// the symbol is exported from this module, which will make it accessible
  /// to other *Intercept modules* that import this module, but unlike
  /// regular exports, this module does not have a definition of the symbol.
  LINKAGE_REEXPORTED
} SymbolLinkage;

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
