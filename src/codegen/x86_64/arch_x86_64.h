#ifndef ARCH_X86_64_H
#define ARCH_X86_64_H

#include <codegen/codegen_forward.h>

/// Context allocation/deallocation
CodegenContext *codegen_context_x86_64_mswin_create(CodegenContext *parent);
void codegen_context_x86_64_mswin_free(CodegenContext *ctx);

#endif // ARCH_X86_64_H
