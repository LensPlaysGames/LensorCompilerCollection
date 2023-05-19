#ifndef ARCH_X86_64_TGT_ASSEMBLY_H
#define ARCH_X86_64_TGT_ASSEMBLY_H

#include <codegen/codegen_forward.h>

/// Emits into `context->code` file.
void emit_x86_64_assembly(CodegenContext *context, MIRFunctionVector machine_instructions);

#endif /* ARCH_X86_64_TGT_ASSEMBLY_H */
