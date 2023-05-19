#ifndef ARCH_X86_64_ISEL_H
#define ARCH_X86_64_ISEL_H

#include <codegen/codegen_forward.h>
#include <codegen/register_allocation.h>
#include <codegen/machine_ir.h>
#include <codegen/x86_64/arch_x86_64_common.h>
#include <error.h>
#include <utils.h>

#define ALL_MX64_INSTRUCTIONS(X)                \
  /* Arithmetic instructions. */                \
  X(ADD)                                        \
  X(SUB)                                        \
  /* X(MUL) */                                  \
  X(IMUL)                                       \
  X(DIV)                                        \
  X(IDIV)                                       \
  X(XOR)                                        \
  X(CMP)                                        \
  X(TEST)                                       \
  X(CWD)                                        \
  X(CDQ)                                        \
  X(CQO)                                        \
  X(SETCC)                                      \
  X(SAL)                                        \
  X(SAR)                                        \
  X(SHR)                                        \
  X(AND)                                        \
  X(OR)                                         \
  X(NOT)                                        \
  /* Stack instructions */                      \
  X(PUSH)                                       \
  X(POP)                                        \
  /* Control flow */                            \
  X(CALL)                                       \
  X(JMP)                                        \
  X(RET)                                        \
  X(JCC)                                        \
  /* Memory stuff */                            \
  X(MOV)                                        \
  X(LEA)                                        \
  X(MOVSX)                                      \
  X(MOVZX)                                      \
  /* Atomics */                                 \
  X(XCHG)


#define DEFINE_MX64_OPCODE(opcode) CAT(MX64_, opcode),
typedef enum MIROpcodex86_64 {
  MX64_START = MIR_ARCH_START,
  ALL_MX64_INSTRUCTIONS(DEFINE_MX64_OPCODE)
  MX64_END,

  MX64_SHL = MX64_SAL,

  MX64_COUNT = MX64_END - MIR_ARCH_START - 1
} MIROpcodex86_64;
#undef DEFINE_MX64_OPCODE

const char *mir_x86_64_opcode_mnemonic(uint32_t opcode);

#endif /* ARCH_X86_64_ISEL_H */
