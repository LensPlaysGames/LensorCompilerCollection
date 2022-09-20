#ifndef X86_64_RA_H
#define X86_64_RA_H
#include <codegen/codegen_forward.h>
#include <stddef.h>

void allocate_registers(CodegenContext *context, Function *f, size_t num_regs);

#endif // X86_64_RA_H
