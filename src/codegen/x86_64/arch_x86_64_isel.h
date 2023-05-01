#ifndef ARCH_X86_64_ISEL_H
#define ARCH_X86_64_ISEL_H

#include <codegen/codegen_forward.h>
#include <codegen/machine_ir.h>

/// Given Intercept IR, return a vector of machine instructions for x86_64 that represents the program.
MIRVector select_instructions(CodegenContext *context);

#endif /* ARCH_X86_64_ISEL_H */
