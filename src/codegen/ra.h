#ifndef X86_64_RA_H
#define X86_64_RA_H
#include <codegen/codegen_forward.h>
#include <stddef.h>

/// Allocate registers for a function.
///
/// This is the entry point for the register allocator; its purpose is to assign
/// to each IR instruction in the function a physical register between 1 and the
/// number of registers (inclusive).
///
/// If the result register of an instruction is already set to a physical
/// register, then the register allocator will respect that and try its best
/// to work with that restriction.
///
/// \param context The codegen context holding architecture-specific data.
/// \param function The function to allocate registers for.
/// \param num_registers The maximum number of registers to allocate.
/// \param platform_interfere_p A predicate that when passed a value and a
///     physical register should return 1 if the computation of that values
///     indirectly interferes with (clobbers) that register, and 0 otherwise.
///     For example, if the value passed to it is a call instruction, the
///     function should return 1 for each caller-saved register.
void allocate_registers
(CodegenContext *context,
 Function *f,
 size_t num_regs,
 regmask_t platform_interfering_regs(const Value *value));

#endif // X86_64_RA_H
