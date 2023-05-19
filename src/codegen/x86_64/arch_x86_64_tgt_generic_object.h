#ifndef ARCH_X86_64_TGT_GENERIC_OBJECT_H
#define ARCH_X86_64_TGT_GENERIC_OBJECT_H

#include <codegen/codegen_forward.h>

/// Emits into `context->object` GenericObjectFile. JUST emits the code section, and some symbols for functions.
void emit_x86_64_generic_object(CodegenContext *context, MIRFunctionVector machine_instructions);

#endif /* ARCH_X86_64_TGT_GENERIC_OBJECT_H */
