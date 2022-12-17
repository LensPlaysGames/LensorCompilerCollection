#ifndef REGISTER_ALLOCATION_H
#define REGISTER_ALLOCATION_H

#include <codegen.h>

typedef struct MachineDescription {
  size_t register_count;
  Register *registers;

  size_t argument_register_count;
  Register *argument_registers;

  // In which register every function result ends up.
  Register result_register;

  size_t (*instruction_register_interference)(IRInstruction *instruction);
} MachineDescription;

/// Peform register allocation for a function.
void allocate_registers(IRFunction *f, const MachineDescription *desc);

#endif /* REGISTER_ALLOCATION_H */
