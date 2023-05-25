#ifndef MACHINE_IR_H
#define MACHINE_IR_H

#include <codegen/codegen_forward.h>
#include <inttypes.h>
#include <stddef.h>
#include <stdint.h>
#include <vector.h>
#include <utils.h>


#define DEFINE_MIR_INSTRUCTION_TYPE(type, ...) CAT(MIR_, type),
typedef enum MIROpcodeCommon {
  ALL_IR_INSTRUCTION_TYPES(DEFINE_MIR_INSTRUCTION_TYPE)
  MIR_COUNT,

  MIR_ARCH_START = 0x420
} MIROpcodeCommon;
#undef DEFINE_MIR_INSTRUCTION_TYPE

typedef enum MIROperandKind {
  MIR_OP_NONE,
  MIR_OP_REGISTER,
  MIR_OP_IMMEDIATE,
  MIR_OP_BLOCK,
  MIR_OP_FUNCTION,
  MIR_OP_NAME,
  MIR_OP_STATIC_REF,
  MIR_OP_LOCAL_REF,
  MIR_OP_COUNT,
  // Operand kinds used exclusively during instruction selection.
  MIR_OP_OP_REF,
  MIR_OP_INST_REF,
  MIR_OP_ANY,
} MIROperandKind;

typedef struct MIROperandRegister {
  usz value;
  usz size;

  bool defining_use;
} MIROperandRegister;
typedef Vector(MIROperandRegister) MIROperandRegisters;

typedef usz MIROperandLocal; // index into function FrameObject vector
typedef const char* MIROperandName;
typedef int64_t MIROperandImmediate;
typedef MIRBlock* MIROperandBlock;
typedef MIRFunction* MIROperandFunction;
/// Pointer to IR_STATIC_REF IR instruction
typedef IRInstruction* MIROperandStatic;

typedef unsigned int MIRRegister;

typedef struct MIROperand {
  MIROperandKind kind;
  union MIROperandValue {
    MIROperandRegister reg;
    MIROperandImmediate imm;
    MIROperandName name;
    MIROperandBlock block;
    MIROperandFunction function;
    MIROperandStatic static_ref;
    MIROperandLocal local_ref;

    /// Used *only* by instruction selection.
    MIROperandOpRef op_ref;
    MIROperandInstRef inst_ref;
  } value;
} MIROperand;

#define MIR_OPERAND_SSO_THRESHOLD 3

// NOTE: There is no way of keeping track of which ISA a particular
// instruction is for; that's up to each backend that uses it!
typedef struct MIRInstruction {
  size_t id;

  // TODO: Expand upon the meaning of "register" here, and the register namespace.
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

  MIROperandRegisters clobbers;

  MIRBlock *block;

  // Keep track of originating IR instruction.
  IRInstruction *origin;

  // If an architecture lowers a generically-lowered MIR instruction,
  // store the arch-lowered MIR instruction in the generic instruction
  // so that we may properly update references going forward.
  struct MIRInstruction *lowered;

} MIRInstruction;

/// Like `foreach (MIROperand, (name), inst->operands)`/`for (auto* name : inst->operands)`
#define FOREACH_MIR_OPERAND(inst, name)                                 \
  MIROperand *CAT(name, base) = (inst->operand_count <= MIR_OPERAND_SSO_THRESHOLD) ? (inst)->operands.arr : (inst)->operands.vec.data; \
  for (MIROperand *(name) = CAT(name, base);                            \
       (name) < CAT(name, base) + inst->operand_count;                  \
       ++(name))

typedef struct MIRBlock {
  string name;
  MIRInstructionVector instructions;

  MIRFunction *function;
  IRBlock *origin;
  MIRBlock* lowered;

  /// Control flow
  MIRBlockVector predecessors;
  MIRBlockVector successors;
  bool is_entry : 1;
  bool is_exit : 1;

} MIRBlock;

typedef struct MIRFrameObject {
  usz size;
  /// ISel may require general MIR frame objects to be mapped to the
  /// lowered MIR frame objects they have created; that's what this is for.
  usz lowered;
  ///  Goes unassigned+unused by compiler, but backend probably needs
  /// this so it's here for convenience.
  isz offset;
} MIRFrameObject;

typedef struct MIRFunction {
  string name;

  uint32_t inst_count;

  /// TL;DR: Locals go here so they can be referenced by index via
  /// stack type MIROperand.
  Vector(MIRFrameObject) frame_objects;

  MIRBlockVector blocks;

  IRFunction *origin;
} MIRFunction;

/// dwisott
MIRInstruction *mir_makenew(uint32_t opcode);
/// Copy the entire instruction
MIRInstruction *mir_makecopy(MIRInstruction *original);

/// Clear the given instructions operands.
void mir_op_clear(MIRInstruction *);

/// Caller is responsible for calling vector_delete() on returned vector.
MIRFunctionVector mir_from_ir(CodegenContext *context);

const char *mir_operand_kind_string(MIROperandKind);

/// If opcode is within common opcode range (less than or equal to
/// MIR_COUNT), return a pointer to a NULL-terminated string containing
/// a human-readable name of the opcode.
const char *mir_common_opcode_mnemonic(uint32_t opcode);

/// \param opcode_mnemonic
///   A function that will be used to get a human-readable mnemonic for
///   any given opcode. If NULL returned, will fallback to mir_common_opcode_mnemonic().
FUNCTION_POINTER(const char*, OpcodeMnemonicFunction, uint32_t);
void print_mir_instruction_with_function_with_mnemonic(MIRFunction *function, MIRInstruction *mir, OpcodeMnemonicFunction opcode_mnemonic);
void print_mir_instruction_with_mnemonic(MIRInstruction *inst, OpcodeMnemonicFunction opcode_mnemonic);
void print_mir_block_with_mnemonic(MIRBlock *block, OpcodeMnemonicFunction opcode_mnemonic);
void print_mir_function_with_mnemonic(MIRFunction *function, OpcodeMnemonicFunction opcode_mnemonic);

/// Same as above but with mir_common_opcode_mnemonic passed implicitly.
void print_mir_operand(MIRFunction *function, MIROperand *op);
void print_mir_instruction(MIRInstruction *inst);
void print_mir_block(MIRBlock *block);
void print_mir_function(MIRFunction *function);

MIROperand mir_op_function(MIRFunction *f);
MIROperand mir_op_block(MIRBlock *block);
MIROperand mir_op_local_ref(MIRFunction *function, usz size);
MIROperand mir_op_local_ref_fo(MIRFunction *function, MIRFrameObject *fo);
MIROperand mir_op_local_ref_ir(MIRFunction *function, IRStackAllocation *alloca);
MIROperand mir_op_reference(MIRInstruction *inst);
MIROperand mir_op_reference_ir(MIRFunction *function, IRInstruction *inst);
MIROperand mir_op_immediate(int64_t imm);
MIROperand mir_op_name(const char *name);
MIROperand mir_op_register(RegisterDescriptor reg, uint16_t size, bool defining_use);

/// Return true iff the given MIRInstruction has A. the correct amount
/// of operands and B. the operands match the given kinds; otherwise,
/// return false.
/// USAGE: if (mir_operand_kinds_match(my_inst, 2, MIR_OP_REGISTER, MIR_OP_REGISTER)) { /* ... */ }
/// NOTE: UB if operand_count is larger than amount of passed `int` type
bool mir_operand_kinds_match(MIRInstruction *inst, usz operand_count, ...);

void mir_add_op(MIRInstruction *inst, MIROperand op);
/// Return a pointer to operand at index within instruction.
MIROperand *mir_get_op(MIRInstruction *inst, size_t index);

/// Return a pointer to frame object at operand within function.
MIRFrameObject *mir_get_frame_object(MIRFunction *function, MIROperandLocal op);

void mir_remove_instruction(MIRInstruction *mi);

void mir_insert_instruction_with_reg(MIRBlock *bb, MIRInstruction *mi, usz index, MIRRegister reg);
/// NOTE: Automatically sets vreg of instruction iff block has MIRFunction reference.
void mir_insert_instruction(MIRBlock*, MIRInstruction*, usz index);
void mir_prepend_instruction(MIRFunction*, MIRInstruction*);
void mir_append_instruction(MIRFunction*, MIRInstruction*);

void mir_push_with_reg_into_block(MIRFunction *f, MIRBlock *block, MIRInstruction *mi, MIRRegister reg);
void mir_push_with_reg(MIRFunction *mir, MIRInstruction *mi, MIRRegister reg);

/// DEPRECATED
MIRInstruction *mir_find_by_vreg(MIRFunction *mir, size_t reg);

/// Create an MIR function from an IR function.
MIRFunction *mir_function(IRFunction *ir_f);

/// Create an MIR basic block from an IR basic block
MIRBlock *mir_block(MIRFunction *function, IRBlock *ir_bb);

/// Create an MIR basic block from another MIR basic block.
/// Updates "lowered" member/field of original to new block.
/// This is mostly needed during ISel, as the general MIR has a block
/// that we want to create a new version of, and by setting the lowered
/// field, we are able to properly update references to the old block to
/// references to the new block.
MIRBlock *mir_block_copy(MIRFunction *function, MIRBlock *bb);

#endif /* MACHINE_IR_H */
