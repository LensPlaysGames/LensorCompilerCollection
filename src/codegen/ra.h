#ifndef X86_64_RA_H
#define X86_64_RA_H
#include <codegen/codegen_forward.h>
#include <stddef.h>

/// Allocate registers for a function.
///
/// This assigns to IR instruction in `f` a physical register between 1 and
/// `num_regs` (inclusive).
///
/// If the result register of an instruction is already set to a value between
/// 1 and `num_regs` (inclusive), then the register allocator will respect that
/// and try its best to work with that restriction.
///
/// `platform_interfere_p` is a callback that when passed a Value* and a register
/// number should return 1 or 0 depending on whether that register is clobbered by
/// the instruction that computes the value or not, respectively.
void allocate_registers
(CodegenContext *context,
 Function *f,
 size_t num_regs,
 char platform_interfere_p(const Value *value, unsigned reg));

#endif // X86_64_RA_H
