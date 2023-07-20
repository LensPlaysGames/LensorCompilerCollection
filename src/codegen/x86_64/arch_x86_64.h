#ifndef ARCH_X86_64_H
#define ARCH_X86_64_H

#include <stdbool.h>
#include <stddef.h>
#include <codegen/codegen_forward.h>

/// This is used for defining lookup tables etc. and
/// ensures that the registers are always in the correct
/// order
#define FOR_ALL_X86_64_REGISTERS(F)     \
  F(RAX, "rax", "eax", "ax", "al")      \
  F(RCX, "rcx", "ecx", "cx", "cl")      \
  F(RDX, "rdx", "edx", "dx", "dl")      \
  F(R8,  "r8", "r8d", "r8w", "r8b")     \
  F(R9,  "r9", "r9d", "r9w", "r9b")     \
  F(R10, "r10", "r10d", "r10w", "r10b") \
  F(R11, "r11", "r11d", "r11w", "r11b") \
  F(R12, "r12", "r12d", "r12w", "r12b") \
  F(RBX, "rbx", "ebx", "bx", "bl")      \
  F(R13, "r13", "r13d", "r13w", "r13b") \
  F(R14, "r14", "r14d", "r14w", "r14b") \
  F(R15, "r15", "r15d", "r15w", "r15b") \
  F(RSI, "rsi", "esi", "si", "sil")     \
  F(RDI, "rdi", "edi", "di", "dil")     \
  F(RBP, "rbp", "ebp", "bp", "bpl")     \
  F(RSP, "rsp", "esp", "sp", "spl")     \
  F(RIP, "rip", "eip", "ip", "ipl")

/// Context allocation/deallocation
CodegenContext *codegen_context_x86_64_mswin_create();
CodegenContext *codegen_context_x86_64_linux_create();
void codegen_context_x86_64_mswin_free(CodegenContext *ctx);
void codegen_context_x86_64_linux_free(CodegenContext *ctx);

bool parameter_is_in_register_x86_64(CodegenContext *context, IRFunction *function, size_t parameter_index);

void codegen_lower_x86_64(CodegenContext *context);
void codegen_lower_early_x86_64(CodegenContext *context);
void codegen_emit_x86_64(CodegenContext *context);

/// Values only valid once codegen_context_x86_64_*_create() has been called and returned.
extern Register *caller_saved_registers;
extern size_t caller_saved_register_count;
extern Register *argument_registers;
extern size_t argument_register_count;

#endif // ARCH_X86_64_H
