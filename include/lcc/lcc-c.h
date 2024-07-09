#ifndef LCC_LCC_H
#define LCC_LCC_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct LccContext* LccContextRef;
typedef struct LccTarget const* LccTargetRef;
typedef struct LccFormat const* LccFormatRef;

typedef struct LccStringView {
    const char* string;
    int64_t length;
} LccStringView;

typedef struct LccLocation {
    uint32_t position;
    uint16_t length;
    uint16_t file_id;
} LccLocation;

// Keep this in the same order as lcc::Value::Kind for easy conversions
typedef enum LccValueKind {
    LCC_BLOCK,
    LCC_FUNCTION,
    LCC_INTEGER_CONSTANT,
    LCC_ARRAY_CONSTANT,
    LCC_POISON,
    LCC_GLOBAL_VARIABLE,

    /// INSTRUCTIONS.
    LCC_INST_ALLOCA,
    LCC_INST_CALL,
    LCC_INST_COPY,
    LCC_INST_INTRINSIC,
    LCC_INST_LOAD,
    LCC_INST_PARAMETER,
    LCC_INST_PHI,
    LCC_INST_STORE,

    /// TERMINATORS.
    LCC_INST_BRANCH,
    LCC_INST_COND_BRANCH,
    LCC_INST_RETURN,
    LCC_INST_UNREACHABLE,

    /// UNARY INSTRUCTIONS.
    LCC_INST_ZEXT,
    LCC_INST_SEXT,
    LCC_INST_TRUNC,
    LCC_INST_BITCAST,
    LCC_INST_NEG,

    /// BINARY INSTRUCTIONS.
    LCC_INST_ADD,
    LCC_INST_SUB,
    LCC_INST_MUL,
    LCC_INST_SDIV,
    LCC_INST_UDIV,
    LCC_INST_SREM,
    LCC_INST_UREM,
    LCC_INST_SHL,
    LCC_INST_SAR,
    LCC_INST_SHR,
    LCC_INST_AND,
    LCC_INST_OR,

    /// COMPARE INSTRUCTIONS.
    LCC_INST_EQ,
    LCC_INST_NE,
    LCC_INST_LT,
    LCC_INST_LE,
    LCC_INST_GT,
    LCC_INST_GE,
} LccValueKind;

// Keep this in the same order as lcc::Linkage for easy conversions
typedef enum LccLinkage {
    /// Local variable.
    ///
    /// This is just a dummy value that is used for local variables
    /// only. In particular, a top-level declaration that is marked
    /// as local is treated as a variable local to the top-level
    /// function.
    LCC_LINKAGE_LOCAL_VAR,

    /// Not exported. Will be deleted if unused.
    ///
    /// This is used for variables and functions that are defined in
    /// and local to this module. A variable or function marked with
    /// this attribute will be *deleted* if it is not used anywhere
    /// and will not be accessible to outside code.
    LCC_LINKAGE_INTERNAL,

    /// Like internal, but will not be deleted.
    ///
    /// This is for variables and functions that are not really exported
    /// and behave just like internal variables and functions, except that
    /// their name will be included in the object file’s symbol table.
    LCC_LINKAGE_USED,

    /// Exported. May be used by other modules.
    ///
    /// This is used for variables and functions that are defined in
    /// this module and exported. Variables and functions marked with
    /// this attribute will not be deleted even if they are not
    /// referenced anywhere.
    LCC_LINKAGE_EXPORTED,

    /// Imported from another module or from C.
    ///
    /// This is used for variables and functions imported from outside
    /// code, whether via importing an Intercept module or simply declaring
    /// an external symbol. This linkage type means that the object is
    /// not defined in this module and that it will be made accessible at
    /// link time only. However, this module will not export the symbol.
    LCC_LINKAGE_IMPORTED,

    /// Imported *and* exported.
    ///
    /// This sort of combines exported and imported in that it means that
    /// the symbol is exported from this module, which will make it accessible
    /// to other *Intercept modules* that import this module, but unlike
    /// regular exports, this module does not have a definition of the symbol.
    LCC_LINKAGE_REEXPORTED,
} LccLinkage;

// Keep this in the same order as lcc::IntrinsicKind for easy conversions
typedef enum LccIntrinsicKind {
    /// Issue a software breakpoint.
    LCC_INTRINSIC_DEBUG_TRAP,

    /// Copy memory; similar to C `memmove()`.
    LCC_INTRINSIC_MEMCOPY,

    /// Fill memory; similar to C `memset()`.
    LCC_INTRINSIC_MEMSET,

    /// Perform a system call.
    LCC_INTRINSIC_SYSCALL,
} LccIntrinsicKind;

// Keep this in the same order as lcc::CallingConvention for easy conversions
typedef enum LccCallingConvention {
    /// C calling convention.
    C,

    /// Intercept internal calling convention.
    INTERCEPT,
} LccCallingConvention;

typedef struct LccModule* LccModuleRef;
typedef struct LccValue* LccValueRef;
typedef struct LccType* LccTypeRef;

// ==== Modules

/// Gets the target info for 64 bit Linux.
LccTargetRef lcc_target_x86_64_linux();
/// Gets the target info for 64 bit Windows.
LccTargetRef lcc_target_x86_64_windows();

/// Gets the emission format for LLVM's Textual IR.
LccFormatRef lcc_format_llvm_textual_ir();
/// Gets the emission format for AT&T assembly targeting GNU's as.
LccFormatRef lcc_format_gnu_as_att_assembly();

/// Create an LCC context.
LccContextRef lcc_context_create(LccTargetRef target, LccFormatRef format);
/// Create an LCC module in the given context.
LccModuleRef lcc_module_create(LccContextRef context);

/// Get the context associated with this LCC module.
LccContextRef lcc_module_get_context(LccModuleRef module);

/// Get the number of functions declared/defined in this LCC module.
int64_t lcc_module_get_function_count(LccModuleRef module);
/// Get the function declared/defined in this LCC module at the given index.
LccValueRef lcc_module_get_function_at_index(LccModuleRef module, int64_t index);

/// Get the number of (global) variables declared/defined in this LCC module.
int64_t lcc_module_get_variable_count(LccModuleRef module);
/// Get the (global) variable declared/defined in this LCC module at the given index.
LccValueRef lcc_module_get_variable_at_index(LccModuleRef module, int64_t index);

/// Add a function to this LCC module.
void lcc_module_add_function(LccModuleRef module, LccValueRef function);
/// Add a (global) variable to this LCC module.
void lcc_module_add_variable(LccModuleRef module, LccValueRef variable);

// ==== Values

// TODO(local): allocate values

/// Returns true of the given value is of the given kind, false otherwise.
bool lcc_is_of_kind(LccValueRef value, LccValueKind kind);

/// Returns true if this value is a global variable, false otherwise.
bool lcc_is_global_varialbe(LccValueRef value);
/// Returns true if this value is an instruction, false otherwise.
bool lcc_is_instruction(LccValueRef value);
/// Returns true if this value is a block, false otherwise.
bool lcc_is_block(LccValueRef value);
/// Returns true if this value is a function, false otherwise.
bool lcc_is_function(LccValueRef value);

/// Gets the LCC context associated with this value.
LccContextRef lcc_get_context(LccValueRef value);
/// Gets the LCC module associated with this value.
LccModuleRef lcc_get_module(LccValueRef value);

/// Gets the kind of this value.
LccValueKind lcc_get_kind(LccValueRef value);
/// Gets the type of this value.
LccTypeRef lcc_get_type(LccValueRef value);
/// Get the source location of this value.
LccLocation lcc_get_location(LccValueRef instruction);

/// Gets the name of this value.
LccStringView lcc_get_name(LccValueRef value);
/// Sets the name of this value.
void lcc_set_name(LccValueRef value, const char* name);

/// Gets the linkage of this value.
LccLinkage lcc_get_linkage(LccValueRef value);
/// Sets the linkage of this value.
void lcc_set_linkage(LccValueRef value, LccLinkage linkage);

/// Get the function associated with this value.
LccValueRef lcc_get_function(LccValueRef value);

// ==== Global Variables

/// Get the initializer for this value.
LccValueRef lcc_get_variable_initializer(LccValueRef value);

// ==== Blocks

/// Get the instruction count for this block.
int64_t lcc_get_block_instruction_count(LccValueRef block);
/// Get the instruction in this block at the given index.
LccValueRef lcc_get_block_instruction_at_index(LccValueRef block, int64_t index);

/// Returns true if this block is closed, false otherwise.
bool lcc_is_block_closed(LccValueRef block);

LccValueRef lcc_block_insert(LccValueRef block, LccValueRef instruction, bool force);
LccValueRef lcc_block_insert_force(LccValueRef block, LccValueRef instruction);

// TODO(local): create instruction builders

// ==== Functions

/// Gets the number of blocks in this function.
int64_t lcc_get_function_block_count(LccValueRef function);
/// Gets the block in this function at the given index.
LccValueRef lcc_get_function_block_at_index(LccValueRef function, int64_t index);

/// Gets the number of parameters in this function.
int64_t lcc_get_function_parameter_count(LccValueRef function);
/// Gets the parameter in this function at the given index.
LccValueRef lcc_get_function_parameter_at_index(LccValueRef function, int64_t index);

// TODO(local): turn this into `LccValueRef lcc_append_block(LccValueRef function);` ?

/// Append the given block to the given function.
void lcc_function_append_block(LccValueRef function, LccValueRef block);

/// Gets the calling convention for this function.
LccCallingConvention lcc_get_function_calling_convention(LccValueRef function);

// ==== Instructions

/// Get the block this instruction is within.
LccValueRef lcc_get_instruction_block(LccValueRef instruction);

/// Get the number of users of this instruction.
int64_t lcc_get_instruction_user_count(LccValueRef instruction);
/// Get the user of this instruction at the given index.
LccValueRef lcc_get_instruction_user_at_index(LccValueRef instruction, int64_t index);

/// Returns true if this instruction can terminate a block, false otherwise.
bool lcc_is_block_terminator(LccValueRef instruction);

/// Get the type allocated by this instruction.
LccTypeRef lcc_get_allocated_type(LccValueRef instruction);

/// Get a call instruction's callee.
LccValueRef lcc_get_callee(LccValueRef instruction);
/// Set a call instruction's callee.
LccTypeRef lcc_get_callee_type(LccValueRef instruction);

/// Get the number of arguments to this instruction.
int64_t lcc_get_instruction_argument_count(LccValueRef instruction);
/// Get the argument within this instruction at the given index.
LccValueRef lcc_get_instruction_argument_at_index(LccValueRef instruction, int64_t index);

/// Returns true if this instruction is force inlined, false otherwise.
bool lcc_is_instruction_force_inline(LccValueRef instruction);
/// Set whether or not this instruction is force inlined.
void lcc_set_instruction_force_inline(LccValueRef instruction, bool force_inline);

/// Returns true if this instruction is a tail call, false otherwise.
bool lcc_is_instruction_tail_call(LccValueRef instruction);
/// Set whether or not this instruction is a tail call.
void lcc_set_instruction_tail_call(LccValueRef instruction, bool tail_call);

/// Get the address to load from or store to.
LccValueRef lcc_get_instruction_address(LccValueRef instruction);
/// Set the address to load from or store to.
void lcc_set_instruction_address(LccValueRef instruction, LccValueRef address);

/// Get the value associated with this instruction.
LccValueRef lcc_get_instruction_value(LccValueRef instruction);
/// Set the value associtaed with this instruciton.
void lcc_set_instruction_value(LccValueRef instruction, LccValueRef value);

/// Remove an incoming value for a block in this PHI instruction.
/// If there is an entry for the block in question in the argument (operand) list of this PHI
/// instruction, the corresponding value is removed.
void lcc_remove_incoming(LccValueRef phi_instruction, LccValueRef block);
/// Register an incoming value from a block in this PHI instruction.
/// If the source block ends with an unreachable or a return instruction, this is a no-op.
/// If this PHI instruction already has a value coming from that block, the value is replaced
// with the new value.
void lcc_set_incoming(LccValueRef phi_instruction, LccValueRef value, LccValueRef block);

/// Gets the index of this parameter instruction.
uint32_t lcc_get_parameter_index(LccValueRef parameter_instruction);

/// Get athis branch instruction's target block.
LccValueRef lcc_get_target(LccValueRef instruction);
/// Set this branch instruction's target block.
void lcc_set_target(LccValueRef instruction, LccValueRef block);

/// Get this control flow instruction's condition.
LccValueRef lcc_get_condition(LccValueRef instruction);
/// Set this control flow instruction's condition.
void lcc_set_condition(LccValueRef instruction, LccValueRef block);

/// Get this control flow instruction's then block.
LccValueRef lcc_get_then(LccValueRef instruction);
/// Set this control flow instruction's then block.
void lcc_set_then(LccValueRef instruction, LccValueRef block);

/// Get this control flow instruction's else block.
LccValueRef lcc_get_else(LccValueRef instruction);
/// Set this control flow instruction's else block.
void lcc_set_else(LccValueRef instruction, LccValueRef block);

/// Returns true if this instruction has a value, false otherwise.
bool lcc_has_value(LccValueRef instruction);
/// Get this instruction's value.
LccValueRef lcc_get_value(LccValueRef instruction);
/// Set this instruction's value.
void lcc_set_value(LccValueRef instruction, LccValueRef block);

/// Get this instruction's lhs value.
LccValueRef lcc_get_lhs(LccValueRef instruction);
/// Set this instruction's lhs value.
void lcc_set_lhs(LccValueRef instruction, LccValueRef block);

/// Get this instruction's rhs value.
LccValueRef lcc_get_rhs(LccValueRef instruction);
/// Set this instruction's rhs value.
void lcc_set_rhs(LccValueRef instruction, LccValueRef block);

/// Get this unary instruction's operand.
LccValueRef lcc_get_unary_operand(LccValueRef instruction);
/// Set this unary instruction's operand.
void lcc_set_unary_operand(LccValueRef instruction, LccValueRef block);

// ==== Instruction Constructors

LccValueRef lcc_build_alloca(LccTypeRef type, LccLocation location);
LccValueRef lcc_build_call(LccValueRef callee, LccTypeRef callee_type, LccValueRef* arguments, int64_t argument_count, LccLocation location);
LccValueRef lcc_build_intrinsic(LccIntrinsicKind intrinsic, LccValueRef* operands, int64_t operand_count, LccLocation location);
LccValueRef lcc_build_load(LccTypeRef type, LccValueRef address, LccLocation location);
LccValueRef lcc_build_store(LccValueRef value, LccValueRef address, LccLocation location);
LccValueRef lcc_build_phi(LccTypeRef type, LccLocation location);
LccValueRef lcc_build_param(LccTypeRef type, uint32_t index, LccLocation location);
LccValueRef lcc_build_branch(LccValueRef target_block, LccLocation location);
LccValueRef lcc_build_cond_branch(LccValueRef condition, LccValueRef then_block, LccValueRef else_block, LccLocation location);
LccValueRef lcc_build_return_void(LccLocation location);
LccValueRef lcc_build_return(LccLocation location, LccValueRef value);
LccValueRef lcc_build_unreachable(LccLocation location);
LccValueRef lcc_build_add(LccValueRef lhs, LccValueRef rhs, LccLocation location);
LccValueRef lcc_build_sub(LccValueRef lhs, LccValueRef rhs, LccLocation location);
LccValueRef lcc_build_mul(LccValueRef lhs, LccValueRef rhs, LccLocation location);
LccValueRef lcc_build_sdiv(LccValueRef lhs, LccValueRef rhs, LccLocation location);
LccValueRef lcc_build_srem(LccValueRef lhs, LccValueRef rhs, LccLocation location);
LccValueRef lcc_build_udiv(LccValueRef lhs, LccValueRef rhs, LccLocation location);
LccValueRef lcc_build_urem(LccValueRef lhs, LccValueRef rhs, LccLocation location);
LccValueRef lcc_build_shl(LccValueRef lhs, LccValueRef rhs, LccLocation location);
LccValueRef lcc_build_sar(LccValueRef lhs, LccValueRef rhs, LccLocation location);
LccValueRef lcc_build_shl(LccValueRef lhs, LccValueRef rhs, LccLocation location);
LccValueRef lcc_build_and(LccValueRef lhs, LccValueRef rhs, LccLocation location);
LccValueRef lcc_build_or(LccValueRef lhs, LccValueRef rhs, LccLocation location);
LccValueRef lcc_build_xor(LccValueRef lhs, LccValueRef rhs, LccLocation location);
LccValueRef lcc_build_equal(LccValueRef lhs, LccValueRef rhs, LccLocation location);
LccValueRef lcc_build_not_equal(LccValueRef lhs, LccValueRef rhs, LccLocation location);
LccValueRef lcc_build_less(LccValueRef lhs, LccValueRef rhs, LccLocation location);
LccValueRef lcc_build_less_equal(LccValueRef lhs, LccValueRef rhs, LccLocation location);
LccValueRef lcc_build_greater(LccValueRef lhs, LccValueRef rhs, LccLocation location);
LccValueRef lcc_build_greater_equal(LccValueRef lhs, LccValueRef rhs, LccLocation location);
LccValueRef lcc_build_zero_extend(LccValueRef operand, LccLocation location);
LccValueRef lcc_build_sign_extend(LccValueRef operand, LccLocation location);
LccValueRef lcc_build_truncate(LccValueRef operand, LccLocation location);
LccValueRef lcc_build_bitcast(LccValueRef operand, LccLocation location);
LccValueRef lcc_build_negate(LccValueRef operand, LccLocation location);
LccValueRef lcc_build_(LccLocation location);

// ==== Constants

/// Creates a constant integer value.
LccValueRef lcc_integer_constant(LccTypeRef type, uint64_t value);
/// Gets the integer value of a constant.
uint64_t lcc_get_constant_integer(LccValueRef constant_value);

// TODO(local): constant float

// TODO(local): constant array value at index

/// Gets the size of an array constant.
size_t lcc_get_constant_array_size(LccValueRef constant_value);

#ifdef __cplusplus
}
#endif

#endif // LCC_LCC_H
