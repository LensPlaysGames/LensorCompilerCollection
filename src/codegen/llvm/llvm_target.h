#ifndef INTERCEPT_LLVM_LLVM_TARGET_H
#define INTERCEPT_LLVM_LLVM_TARGET_H

#include <codegen/codegen_forward.h>

NODISCARD CodegenContext* codegen_context_llvm_create();
void codegen_context_llvm_free(CodegenContext* ctx);

/// Emit LLVM IR.
void codegen_emit_llvm(CodegenContext* ctx);

#endif // INTERCEPT_LLVM_LLVM_TARGET_H
