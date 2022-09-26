#ifndef ARCH_X86_64_H
#define ARCH_X86_64_H

#include <codegen/codegen_forward.h>

#define MIN_VIRTUAL_REGISTER_X86_64 (1024)

/// Context allocation/deallocation
CodegenContext *codegen_context_x86_64_create(CodegenContext *parent);
void codegen_context_x86_64_free(CodegenContext *ctx);

/// Emit the program.
void codegen_emit_x86_64(CodegenContext *context);

#endif // ARCH_X86_64_H
