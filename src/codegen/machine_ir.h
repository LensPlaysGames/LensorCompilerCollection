#ifndef MACHINE_IR_H
#define MACHINE_IR_H

#include <codegen/machine_ir_forward.h>
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

typedef struct MIROperandRegister {
  uint32_t value;
  uint16_t size;
} MIROperandRegister;

typedef int64_t MIROperandImmediate;
typedef IRBlock* MIROperandBlock;
typedef IRFunction* MIROperandFunction;
typedef MIRInstruction* MIROperandReference;

typedef struct MIROperand {
  MIROperandKind kind;
  union {
    MIROperandReference ref;
    MIROperandRegister reg;
    MIROperandImmediate imm;
    MIROperandBlock block;
    MIROperandFunction function;
  } value;
} MIROperand;

// NOTE: There is no way of keeping track of which ISA a particular
// instruction is for; that's up to each backend that uses it!
typedef struct MIRInstruction {
  size_t id;

  /// An MIR opcode is some integer that has the 1:1 IR types from 0->N
  /// and then after that each ISA starts it's own opcode enum for
  /// architecture-specific instructions.
  /// The idea is that each ISA gets fed an MIR full of regular IR
  /// instructions and it's job is to translate them into (or build a
  /// new MIR with) the arch-specific opcodes.
  uint32_t opcode;

  MIROperand operands[4];

  // Architecture-specific values.
  union {
    MIRValue_x86_64 x64;
  };

  // Keep track of originating IR instruction.
  IRInstruction *origin;

} MIRInstruction;

/// dwisott
/// Caller is responsible for calling vector_delete() on returned vector.
MIRVector mir_from_ir(CodegenContext *context);

/// If opcode is within common opcode range (less than or equal to
/// MIR_COUNT), return a pointer to a NULL-terminated string containing
/// a human-readable name of the opcode.
const char *mir_common_opcode_mnemonic(uint32_t opcode);

void print_mir_instruction(MIRInstruction *m_inst);

#endif /* MACHINE_IR_H */
