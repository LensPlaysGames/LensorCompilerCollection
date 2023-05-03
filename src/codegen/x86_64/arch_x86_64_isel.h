#ifndef ARCH_X86_64_ISEL_H
#define ARCH_X86_64_ISEL_H

#include <codegen/codegen_forward.h>
#include <codegen/machine_ir.h>

/// Given Intercept IR, return a vector of machine instructions for x86_64 that represents the program.
MIRInstructionVector select_instructions(CodegenContext *context);

/// Given freshly-lowered MIR, return a vector of machine instructions for x86_64 that represents the program.
MIRFunctionVector select_instructions2(MIRFunctionVector input);

#endif /* ARCH_X86_64_ISEL_H */
