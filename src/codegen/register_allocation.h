#ifndef REGISTER_ALLOCATION_H
#define REGISTER_ALLOCATION_H

#include <codegen.h>

typedef struct RegisterAllocationInfo {
  CodegenContext *context;

  size_t register_count;
  Register *registers;

  size_t argument_register_count;
  Register *argument_registers;

  // In which register every function result ends up.
  Register result_register;

  int64_t (*instruction_register_interference)(IRInstruction *instruction);
} RegisterAllocationInfo;

RegisterAllocationInfo *ra_allocate_info
(CodegenContext *context,
 Register result_register,
 size_t general_registers_count,
 Register *general_registers,
 size_t argument_registers_count,
 Register *argument_registers,
 int64_t (*instruction_register_interference)(IRInstruction *instruction)
 );

void ra(RegisterAllocationInfo *info);

#endif /* REGISTER_ALLOCATION_H */
