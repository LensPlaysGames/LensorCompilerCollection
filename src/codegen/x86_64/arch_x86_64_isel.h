#ifndef ARCH_X86_64_ISEL_H
#define ARCH_X86_64_ISEL_H

#include <codegen/codegen_forward.h>
#include <codegen/register_allocation.h>
#include <codegen/machine_ir.h>
#include <codegen/x86_64/arch_x86_64_common.h>
#include <error.h>

/// Given Intercept IR, return a vector of machine instructions for x86_64 that represents the program.
MIRInstructionVector select_instructions(CodegenContext *context);

/// Given freshly-lowered MIR, return a vector of machine instructions for x86_64 that represents the program.
MIRFunctionVector select_instructions2(const MachineDescription*, MIRFunctionVector input);

STATIC_ASSERT(I_COUNT == 29, "Exhaustive handling of x86_64 instructions in x86_64 MIR");
typedef enum MIROpcodex86_64 {
  /// Arithmetic instructions.
  MX64_ADD = MIR_ARCH_START,
  MX64_SUB,
  // I_MUL,
  MX64_IMUL,
  MX64_DIV,
  MX64_IDIV,
  MX64_XOR,
  MX64_CMP,
  MX64_TEST,
  MX64_CWD,
  MX64_CDQ,
  MX64_CQO,
  MX64_SETCC,
  MX64_SAL,
  MX64_SHL = MX64_SAL,
  MX64_SAR,
  MX64_SHR,
  MX64_AND,
  MX64_OR,
  MX64_NOT,

  /// Stack instructions.
  MX64_PUSH,
  MX64_POP,

  /// Control flow.
  MX64_CALL,
  MX64_JMP,
  MX64_RET,
  MX64_JCC,

  /// Memory stuff.
  MX64_MOV,
  MX64_LEA,

  MX64_MOVSX,
  MX64_MOVZX,

  MX64_XCHG,

  MX64_END,
  MX64_COUNT = MX64_END - MIR_ARCH_START
} MIROpcodex86_64;

const char *mir_x86_64_opcode_mnemonic(uint32_t opcode);

#endif /* ARCH_X86_64_ISEL_H */
