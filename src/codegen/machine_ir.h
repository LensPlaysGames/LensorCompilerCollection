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

typedef struct MIROperand {
  MIROperandKind kind;
  union {
    MIROperandRegister reg;
    MIROperandImmediate imm;
    MIROperandName name;
    MIROperandBlock block;
    MIROperandFunction function;
  } value;
} MIROperand;

#define MIR_OPERAND_SSO_THRESHOLD 3

// NOTE: There is no way of keeping track of which ISA a particular
// instruction is for; that's up to each backend that uses it!
typedef struct MIRInstruction {
  size_t id;

  MIRRegister reg;

  /// An MIR opcode is some integer that has the 1:1 IR types from 0->N
  /// and then after that each ISA starts it's own opcode enum for
  /// architecture-specific instructions.
  /// The idea is that each ISA gets fed an MIR full of regular IR
  /// instructions and it's job is to translate them into (or build a
  /// new MIR with) the arch-specific opcodes.
  uint32_t opcode;

  uint8_t operand_count;
  union {
    MIROperand arr[MIR_OPERAND_SSO_THRESHOLD];
    Vector(MIROperand) vec;
  } operands;

  // Architecture-specific values.
  union {
    MIRValue_x86_64 x64;
  };

  // Keep track of originating IR instruction.
  IRInstruction *origin;

  // If an architecture lowers a generically-lowered MIR instruction,
  // store the arch-lowered MIR instruction in the generic instruction
  // so that we may properly update references going forward.
  struct MIRInstruction *lowered;

} MIRInstruction;

/// dwisott
MIRInstruction *mir_makenew(uint32_t opcode);
/// Copy the entire instruction
MIRInstruction *mir_makecopy(MIRInstruction *original, uint32_t opcode);

/// Clear the given instructions operands.
void mir_op_clear(MIRInstruction *);

/// Caller is responsible for calling vector_delete() on returned vector.
MIRVector mir_from_ir(CodegenContext *context);

/// If opcode is within common opcode range (less than or equal to
/// MIR_COUNT), return a pointer to a NULL-terminated string containing
/// a human-readable name of the opcode.
const char *mir_common_opcode_mnemonic(uint32_t opcode);

void print_mir_instruction(MIRInstruction *m_inst);

MIROperand mir_op_function(IRFunction *f);
MIROperand mir_op_block(IRBlock *block);
MIROperand mir_op_reference(MIRInstruction *inst);
MIROperand mir_op_reference_ir(IRInstruction *inst);
MIROperand mir_op_immediate(int64_t imm);
MIROperand mir_op_name(const char *name);
MIROperand mir_op_register(RegisterDescriptor reg, uint16_t size);

void mir_add_op(MIRInstruction *inst, MIROperand op);
/// Return a pointer to operand at index within instruction.
MIROperand *mir_get_op(MIRInstruction *inst, size_t index);

void mir_push(MIRVector *mir, MIRInstruction *mi);
MIRInstruction *mir_find_by_vreg(MIRVector mir, size_t reg);

#endif /* MACHINE_IR_H */
