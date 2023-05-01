#ifndef MACHINE_IR_H
#define MACHINE_IR_H

#include <codegen/codegen_forward.h>
#include <stdint.h>
#include <vector.h>

typedef struct MIRValue_x86_64 {
  uint8_t instruction_form;
  uint16_t instruction;
  uint8_t reg_src_sz;
  uint8_t reg_src;
  uint8_t reg_dst_sz;
  uint8_t reg_dst;
  uint8_t reg_addr;
  int64_t immediate;
  int64_t offset;
  const char *name;
  IRBlock *ir_block;
  IRFunction *ir_function;
} MIRValue_x86_64;

// NOTE: There is no way of keeping track of which ISA a particular
// instruction is for; that's up to each backend that uses it!
typedef struct MIRInstruction {
  union {
    MIRValue_x86_64 x64;
  };

  // Maybe keep track of originating IR instruction?
  IRInstruction *origin;
} MIRInstruction;
typedef Vector(MIRInstruction) MIRVector;

#endif /* MACHINE_IR_H */
