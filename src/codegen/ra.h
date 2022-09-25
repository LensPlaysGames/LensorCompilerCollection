#ifndef X86_64_RA_H
#define X86_64_RA_H
#include <codegen/codegen_forward.h>
#include <stddef.h>

typedef unsigned Register;

/// Argument for the register allocator.
typedef struct RAInfo {
  CodegenContext *context;

  /// The function to allocate registers for.
  Function *function;

  /// The number of registers available for allocation.
  size_t num_regs;

  /// An array of registers in which function arguments are passed, in order.
  const Register *arg_regs;

  /// The number of registers in `arg_regs`.
  size_t num_arg_regs;

  /// A function that will be called for each value in the function and should
  /// return a mask indicating which registers cannot be allocated to that value.
  /// For instance, a call instruction should return a mask consisting of all
  /// caller-saved (volatile) registers.
  regmask_t (*platform_interfering_regs)(const CodegenContext *context, const Value *value);
} RAInfo;

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
/// \param info A RAInfo struct containing options for the register allocator.
///     See the documentation for the struct for more information.
void allocate_registers(RAInfo *info);

#endif // X86_64_RA_H
