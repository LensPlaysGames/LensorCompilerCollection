#ifndef INTERCEPT_IR_IR_H
#define INTERCEPT_IR_IR_H

#include <codegen.h>
#include <codegen/codegen_forward.h>
#include <inttypes.h>
#include <stdbool.h>
#include <vector.h>

#define BINARY_INSTRUCTION_CASE_HELPER(enumerator, name) case IR_##enumerator:
#define ALL_BINARY_INSTRUCTION_CASES()                   ALL_BINARY_INSTRUCTION_TYPES(BINARY_INSTRUCTION_CASE_HELPER)

#define DEFINE_IR_INSTRUCTION_TYPE(type, ...) CAT(IR_, type),
typedef enum IRType {
  ALL_IR_INSTRUCTION_TYPES(DEFINE_IR_INSTRUCTION_TYPE)

  /// This is also used to mark freed instructions.
  IR_COUNT
} IRType;
#undef DEFINE_IR_INSTRUCTION_TYPE

typedef struct IRPhiArgument {
  /// The value of the argument itself.
  IRInstruction *value;
  /// Stores the predecessor to the Phi node in the direction of the
  /// argument assignment.
  ///    [a]
  ///  [t] [o]
  ///    \ [b]
  ///    [j]
  /// For example, if arg->value->block == o, then arg->block == b.
  IRBlock *block;
} IRPhiArgument;

typedef union IRValue {
  IRInstruction *inst;
  IRFunction *func;
} IRValue;

// clang-format off

/// ===========================================================================
///  Iterators
/// ===========================================================================
/// These iterator macros may not be used if you plan on inserting or deleting
/// stuff from the range you’re iterating over.
#define FOREACH_BLOCK(block, function)                                                           \
  for (IRBlock * block,                                                                          \
       ** const CAT(block, _begin_ptr) = ir_blocks_begin_impl(function),                         \
       **CAT(block, _ptr) = CAT(block, _begin_ptr),                                              \
       ** const CAT(block, _end_ptr) = ir_blocks_end_impl(function);                             \
       DEBUG_ITERATOR(block, function, blocks) &&                                                \
           CAT(block, _ptr) != CAT(block, _end_ptr) ? (block = *CAT(block, _ptr), true) : false; \
       ++CAT(block, _ptr))

#define FOREACH_INSTRUCTION(inst, block)                                                     \
  for (IRInstruction * inst,                                                                 \
       ** const CAT(inst, _begin_ptr) = ir_instructions_begin_impl(block),                   \
       **CAT(inst, _ptr) = CAT(inst, _begin_ptr),                                            \
       ** const CAT(inst, _end_ptr) = ir_instructions_end_impl(block);                       \
       DEBUG_ITERATOR(inst, block, instructions) &&                                          \
           CAT(inst, _ptr) != CAT(inst, _end_ptr) ? (inst = *CAT(inst, _ptr), true) : false; \
       ++CAT(inst, _ptr))

#define FOREACH_USER(user, inst)                                                             \
  for (IRInstruction * user,                                                                 \
       ** const CAT(user, _begin_ptr) = ir_users_begin_impl(inst),                           \
       **CAT(user, _ptr) = CAT(user, _begin_ptr),                                            \
       ** const CAT(user, _end_ptr) = ir_users_end_impl(inst);                               \
       DEBUG_ITERATOR(user, inst, users) &&                                                  \
           CAT(user, _ptr) != CAT(user, _end_ptr) ? (user = *CAT(user, _ptr), true) : false; \
       ++CAT(user, _ptr))

/// Helper to detect iterator invalitation.
#ifdef NDEBUG
# define DEBUG_ITERATOR(...) (true)
#else
# define DEBUG_ITERATOR(name, parent, collection_name)   \
  (ir_debug_iterators_impl(                              \
    CAT(name, _begin_ptr),                               \
    CAT(name, _end_ptr),                                 \
    CAT(CAT(ir_, collection_name), _begin_impl)(parent), \
    CAT(CAT(ir_, collection_name), _end_impl)(parent)    \
  ), true)
#endif

#define FOREACH_INSTRUCTION_IN_FUNCTION(i, b, f) \
  FOREACH_BLOCK(b, f)                            \
    FOREACH_INSTRUCTION(i, b)

#define FOREACH_INSTRUCTION_IN_CONTEXT(i, b, f, ctx) \
  foreach_val(f, ctx->functions)          \
    FOREACH_INSTRUCTION_IN_FUNCTION(i, b, f)

/// ===========================================================================
///  Instruction Data Access
/// ===========================================================================
///
/// Note:
///   - Functions/macros whose names start with `ir_insert_` create and insert
///     an instruction into the current block at the current insert point.
///
///   - Functions/macros whose names start with `ir_create_` create instructions
///     without inserting them anywhere.
///

/// Helper to define a pair of accessors as a generic macro.
#define IR_PROPERTY(name, obj, ...)                                           \
  _Generic((VA_FIRST(__VA_ARGS__ __VA_OPT__(,) ((struct no_generic_argument*)NULL))), \
    struct no_generic_argument*: CAT(name, _impl_get),                        \
    default: CAT(name, _impl_set)                                             \
  )(obj __VA_OPT__(,) __VA_ARGS__)

/// Same as IR_PROPERTY, but function takes 2 arguments.
#define IR_PROPERTY2(name, obj, arg1, ...)                                    \
  _Generic((VA_FIRST(__VA_ARGS__ __VA_OPT__(,) ((struct no_generic_argument*)NULL))), \
    struct no_generic_argument*: CAT(name, _impl_get),                        \
    default: CAT(name, _impl_set)                                             \
  )(obj, arg1 __VA_OPT__(,) __VA_ARGS__)

/// Access the offset of an alloca.
#define ir_alloca_offset(alloc, ...) IR_PROPERTY(ir_alloca_offset, alloc, __VA_ARGS__)

/// Access the size of an alloca.
#define ir_alloca_size(alloc, ...) IR_PROPERTY(ir_alloca_size, alloc, __VA_ARGS__)

/// Access a function attribute.
#define ir_attribute(func, attr, ...) IR_PROPERTY2(ir_attribute, func, attr __VA_OPT__(,) __VA_ARGS__)

/// Get an iterator to the beginning of an instruction or block list.
#define ir_begin(obj) _Generic((obj),   \
  IRBlock*: ir_instructions_begin_impl, \
  IRFunction*: ir_blocks_begin_impl     \
)(obj)

/// Access the nth argument of a call.
#define ir_call_arg(call, n, ...) IR_PROPERTY2(ir_call_arg, call, n, __VA_ARGS__)

/// Access force inline attribute of a call instruction.
#define ir_call_force_inline(call, ...) IR_PROPERTY(ir_call_force_inline, call, __VA_ARGS__)

/// Access the tail call attribute of a call instruction.
#define ir_call_tail(call, ...) IR_PROPERTY(ir_call_tail, call, __VA_ARGS__)

/// Access the callee of a call.
#define ir_callee(call, ...) IR_PROPERTY(ir_callee, call, __VA_ARGS__)

/// Access the condition of a conditional branch.
#define ir_cond(cond, ...) IR_PROPERTY(ir_cond, cond, __VA_ARGS__)

/// Get the number of blocks in a function or instructions in a block.
#define ir_count(obj) _Generic((obj), \
  IRBlock*: ir_count_impl_b,          \
  IRFunction*: ir_count_impl_f        \
)(obj)

/// Access the destination block of an unconditional branch.
#define ir_dest(br, ...) IR_PROPERTY(ir_dest, br, __VA_ARGS__)

/// Access the else branch of a conditional branch.
#define ir_else(cond, ...) IR_PROPERTY(ir_else, cond, __VA_ARGS__)

/// Get an iterator to the end of an instruction or block list.
#define ir_end(obj) _Generic((obj),   \
  IRBlock*: ir_instructions_end_impl, \
  IRFunction*: ir_blocks_end_impl     \
)(obj)


/// Access the registers used by a function.
#define ir_func_regs_in_use(func, ...) IR_PROPERTY(ir_func_regs_in_use, func, __VA_ARGS__)

/// Access the ID of a block or instruction.
#define ir_id(obj, ...)   _Generic((VA_FIRST(__VA_ARGS__ __VA_OPT__(,) ((struct no_generic_argument*)NULL))), \
    struct no_generic_argument*: _Generic((obj), \
      IRInstruction *: ir_id_i_impl_get,         \
      IRBlock *: ir_id_b_impl_get                \
    ),                                           \
    default: _Generic((obj),                     \
      IRInstruction *: ir_id_i_impl_set,         \
      IRBlock *: ir_id_b_impl_set                \
    )                                            \
  )(obj __VA_OPT__(,) __VA_ARGS__)

/// Access the immediate value of an instruction.
#define ir_imm(inst, ...) IR_PROPERTY(ir_imm, inst, __VA_ARGS__)

/// Access the intrinsic kind of an intrinsic call.
#define ir_intrinsic_kind(call, ...) IR_PROPERTY(ir_intrinsic_kind, call, __VA_ARGS__)

/// Get an iterator from an instruction or block.
#define ir_it(obj) _Generic((obj), \
  IRInstruction*: ir_it_impl_i,    \
  IRBlock*: ir_it_impl_b           \
)(obj)

/// Access the LHS of a binary expression.
#define ir_lhs(expr, ...) IR_PROPERTY(ir_lhs, expr, __VA_ARGS__)

/// Access the linkage of a function or static variable.
#define ir_linkage(obj) _Generic((obj), \
  IRFunction*: ir_linkage_impl_f,       \
  IRStaticVariable*: ir_linkage_impl_v  \
)(obj)

/// Access a location of an instruction or function.
#define ir_location(obj, ...) \
  _Generic((VA_FIRST(__VA_ARGS__ __VA_OPT__(,) ((struct no_generic_argument*)NULL))), \
    struct no_generic_argument*: _Generic((obj), \
      IRInstruction *: ir_location_i_impl_get,   \
      IRFunction *: ir_location_f_impl_get       \
    ),                                           \
    default: _Generic((obj),                     \
      IRInstruction *: ir_location_i_impl_set,   \
      IRFunction *: ir_location_f_impl_set       \
    )                                            \
  )(obj __VA_OPT__(,) __VA_ARGS__)

/// Access the MIR instruction, block, or function corresponding to an IR entity.
#define ir_mir(obj, ...) \
  _Generic((VA_FIRST(__VA_ARGS__ __VA_OPT__(,) ((struct no_generic_argument*)NULL))), \
    struct no_generic_argument*: _Generic((obj), \
      IRInstruction *: ir_mir_i_impl_get,        \
      IRBlock *: ir_mir_b_impl_get,              \
      IRFunction *: ir_mir_f_impl_get            \
    ),                                           \
    default: _Generic((obj),                     \
      IRInstruction *: ir_mir_i_impl_set,        \
      IRBlock *: ir_mir_b_impl_set,              \
      IRFunction *: ir_mir_f_impl_set            \
    )                                            \
  )(obj __VA_OPT__(,) __VA_ARGS__)

/// Access the name of a function or block.
#define ir_name(obj, ...) _Generic((VA_FIRST(__VA_ARGS__ __VA_OPT__(,) ((struct no_generic_argument*)NULL))), \
    struct no_generic_argument*: _Generic((obj), \
      IRBlock *: ir_name_b_impl_get,             \
      IRFunction *: ir_name_f_impl_get           \
    ),                                           \
    default: _Generic((obj),                     \
      IRBlock *: ir_name_b_impl_set,             \
      IRFunction *: ir_name_f_impl_set           \
    )                                            \
  )(obj __VA_OPT__(,) __VA_ARGS__)

/// Access the single operand of an instruction.
#define ir_operand(inst, ...) IR_PROPERTY(ir_operand, inst, __VA_ARGS__)

/// Access the parent of an instruction or block.
#define ir_parent(obj) _Generic((obj), \
  IRInstruction*: ir_parent_impl_i,    \
  IRBlock*: ir_parent_impl_b           \
)(obj)

/// Access the result register of an instruction.
#define ir_register(inst, ...) IR_PROPERTY(ir_register, inst, __VA_ARGS__)

/// Access the RHS of a binary expression.
#define ir_rhs(expr, ...) IR_PROPERTY(ir_rhs, expr, __VA_ARGS__)

/// Access initialiser of static variable.
#define ir_static_var_init(var, ...) IR_PROPERTY(ir_static_var_init, var, __VA_ARGS__)

/// Access the address of a store.
#define ir_store_addr(store, ...) IR_PROPERTY(ir_store_addr, store, __VA_ARGS__)

/// Access the value of a store.
#define ir_store_value(store, ...) IR_PROPERTY(ir_store_value, store, __VA_ARGS__)

/// Access the then branch of a conditional branch.
#define ir_then(cond, ...) IR_PROPERTY(ir_then, cond, __VA_ARGS__)

/// Get the type of an IR object.
#define ir_typeof(obj) _Generic((obj), \
  IRInstruction*: ir_typeof_impl_i,    \
  IRFunction*: ir_typeof_impl_f        \
)(obj)

/// Convert an instruction or function to a value.
#define ir_val(obj) _Generic((obj),                                   \
  IRInstruction*: (IRValue){.inst = (IRInstruction*)(obj)},           \
  IRFunction*: (IRValue){.func = (IRFunction*)(obj)}                  \
)

// clang-format on

/// Get the nth block of a function.
NODISCARD IRBlock *ir_block_get(IRFunction *function, usz n);

/// Add an argument to a call instruction.
///
/// \param call The call instruction to add the argument to.
/// \param value The value to add.
void ir_call_add_arg(IRInstruction *call, IRInstruction *value);

/// Get the number of arguments of a call.
NODISCARD usz ir_call_args_count(IRInstruction *call);

/// Get the function type of the callee of a call.
NODISCARD Type *ir_call_callee_type(IRInstruction *call);

/// Insert an argument into a call.
///
/// The argument will be inserted at the given index,
/// and all arguments starting at that index will be
/// shifted to the right by one.
void ir_call_insert_arg(IRInstruction *call, usz n, IRInstruction *value);

/// Check if a call is a direct call.
NODISCARD bool ir_call_is_direct(IRInstruction *call);

/// Remove an argument from a call.
void ir_call_remove_arg(IRInstruction *call, usz n);

/// Replace an argument of a call.
void ir_call_replace_arg(IRInstruction *call, usz n, IRInstruction *value);

/// Check if a basic block is closed.
NODISCARD bool ir_is_closed(IRBlock *block);

/// Get the entry block of a function.
NODISCARD IRBlock *ir_entry_block(IRFunction *function);

/// Check if a function is a definition.
NODISCARD bool ir_func_is_definition(IRFunction *function);

/// Get the referenced function from a func ref.
NODISCARD IRFunction *ir_func_ref_func(IRInstruction *func_ref);

/// Get the nth instruction of a block.
NODISCARD IRInstruction *ir_inst_get(IRBlock *block, usz n);

/// Check if an instruction is a branch instruction.
NODISCARD bool ir_is_branch(IRInstruction* i);

/// Get the kind of an instruction.
NODISCARD IRType ir_kind(IRInstruction *i);

/// Get a string representation of an IR kind.
NODISCARD span ir_kind_to_str(IRType t);

/// Get a reference to a function parameter value on entry.
NODISCARD IRInstruction *ir_parameter(IRFunction *func, usz index);

/// Add an argument to a PHI instruction.
///
/// If the PHI already has an argument from the given block, this
/// function will replace it.
///
/// \param phi The PHI instruction to add the value to.
/// \param from The predecessor block with which to associate the value.
/// \param value The value to add.
void ir_phi_add_arg(IRInstruction *phi, IRBlock *from, IRInstruction *value);

/// Get the nth argument of a PHI instruction.
NODISCARD const IRPhiArgument *ir_phi_arg(IRInstruction *phi, usz n);

/// Get the number of arguments of a PHI instruction.
NODISCARD usz ir_phi_args_count(IRInstruction *phi);

/// Remove a PHI argument from a PHI instruction.
///
/// If the PHI has no argument from the given block, this
/// function does nothing.
///
/// \param phi The PHI instruction to remove the argument from.
/// \param block The predecessor block whose value to remove.
void ir_phi_remove_arg(IRInstruction *phi, IRBlock *block);

/// Set the type of an instruction.
void ir_set_type(IRInstruction *i, Type *type);

/// Get the variable referenced by a static ref.
NODISCARD IRStaticVariable *ir_static_ref_var(IRInstruction *ref);

/// Get the string data from an IR_LIT_STRING.
NODISCARD span ir_string_data(CodegenContext *ctx, IRInstruction *lit);

/// Get the terminator instruction of a block.
///
/// It is ill-formed to call this on an unfinished basic block
/// during IR generation. After IR generation, all blocks should
/// have a terminator instruction at all times.
NODISCARD IRInstruction *ir_terminator(IRBlock *block);

/// Get the use count of an instruction, i.e. how often an
/// instruction is used by other instructions.
NODISCARD usz ir_use_count(IRInstruction *i);

/// Get the nth user of an instruction.
NODISCARD IRInstruction *ir_user_get(IRInstruction *inst, usz n);

/// ===========================================================================
///  Instruction Creation
/// ===========================================================================
/// Create a basic block.
NODISCARD IRBlock *ir_block(CodegenContext *ctx);

/// Create a function.
NODISCARD IRFunction *ir_create_function(
  CodegenContext *context,
  string name,
  Type *function_type,
  SymbolLinkage linkage
);

/// Create an alloca instruction.
NODISCARD IRInstruction *ir_create_alloca(CodegenContext *context, Type *type);

/// Create an alloca instruction with a size.
/// FIXME: JANK.
NODISCARD IRInstruction *ir_create_alloca_sized(CodegenContext *context, Type *type, usz size);

/// Create a bitcast instruction.
NODISCARD IRInstruction *ir_create_bitcast(
  CodegenContext *context,
  Type *to_type,
  IRInstruction *value
);

/// Create an unconditional branch.
NODISCARD IRInstruction *ir_create_br(
  CodegenContext *context,
  IRBlock *destination
);

/// Create a call instruction.
#define ir_create_call(ctx, callee) _Generic((callee), IRInstruction *: ir_create_call_impl_i, IRFunction *: ir_create_call_impl_f)(ctx, callee)

/// Create a conditional branch.
NODISCARD IRInstruction *ir_create_cond_br(
  CodegenContext *context,
  IRInstruction *condition,
  IRBlock *then_block,
  IRBlock *else_block
);

/// Create a copy of a value.
NODISCARD IRInstruction *ir_create_copy(
  CodegenContext *context,
  IRInstruction *source
);

/// Create a function reference.
NODISCARD IRInstruction *ir_create_func_ref(CodegenContext *context, IRFunction *function);

/// Create an immediate value.
NODISCARD IRInstruction *ir_create_immediate(
  CodegenContext *context,
  Type *type,
  u64 immediate
);

/// Create an integer literal.
NODISCARD IRInstruction *ir_create_int_lit(CodegenContext *ctx, usz value);

/// Create a reference to an interned string literal.
NODISCARD IRInstruction *ir_create_interned_str_lit(CodegenContext *context, usz string_index);

/// Create an intrinsic instruction.
///
/// Note: Prefer to lower intrinsics to other IR instructions. This
/// is only for instructions that need to be lowered to special ASM
/// instructions or depend on late compile-time constants.
NODISCARD IRInstruction *ir_create_intrinsic(CodegenContext *context, Type *t, enum IntrinsicKind intrinsic);

/// Type needs to be passed in as a reminder that we don’t care
/// about the type of the address so long as it’s a pointer or
/// reference.
NODISCARD IRInstruction *ir_create_load(
  CodegenContext *context,
  Type *type,
  IRInstruction *address
);

/// Create a call to the memcpy intrinsic.
NODISCARD IRInstruction *ir_create_memcpy(
  CodegenContext *context,
  IRInstruction *dest,
  IRInstruction *src,
  IRInstruction *size
);

/// Create a not instruction.
NODISCARD IRInstruction *ir_create_not(
  CodegenContext *context,
  IRInstruction *source
);

/// Create a PHI instruction.
NODISCARD IRInstruction *ir_create_phi(CodegenContext *context, Type *type);

/// Create a register instruction.
///
/// This is intended for use by the backend.
NODISCARD IRInstruction *ir_create_register(CodegenContext *context, Type *type, Register result);

/// Create a return instruction.
///
/// \param context The codegen context.
/// \param ret The return value (may be NULL).
/// \return The created instruction.
NODISCARD IRInstruction *ir_create_return(
  CodegenContext *context,
  IRInstruction *ret
);

/// Create a sign extension instruction.
NODISCARD IRInstruction *ir_create_sext(
  CodegenContext *context,
  Type *result_type,
  IRInstruction *value
);

/// Create a variable with static storage duration.
///
/// \param context The codegen context.
/// \param decl The expression that declares the variable (may be NULL).
/// \param type The type of the variable.
/// \param name The name of the variable.
/// \return An handle to the variable that can be used to create references.
NODISCARD IRStaticVariable *ir_create_static(
  CodegenContext *context,
  Node *decl,
  Type *type,
  string name
);

/// Create a reference to a variable with static storage duration.
NODISCARD IRInstruction *ir_create_static_ref(CodegenContext *context, IRStaticVariable *var);

/// Create a store instruction.
///
/// \param context The codegen context.
/// \param data The data to store at the address.
/// \param address The address to store the data at.
/// \return The created instruction.
NODISCARD IRInstruction *ir_create_store(
  CodegenContext *context,
  IRInstruction *data,
  IRInstruction *address
);

/// Create a truncate instruction.
NODISCARD IRInstruction *ir_create_trunc(
  CodegenContext *context,
  Type *result_type,
  IRInstruction *value
);

/// Create an unreachable instruction.
NODISCARD IRInstruction *ir_create_unreachable(CodegenContext *context);

/// Create a zero extension instruction.
NODISCARD IRInstruction *ir_create_zext(
  CodegenContext *context,
  Type *result_type,
  IRInstruction *value
);

/// Binary instructions.
#define F(_, name) \
  IRInstruction *ir_create_##name(CodegenContext *context, IRInstruction *lhs, IRInstruction *rhs);
ALL_BINARY_INSTRUCTION_TYPES(F)
#undef F

/// ===========================================================================
///  Instruction Insertion
/// ===========================================================================
/// Attach a block to the current function.
///
/// This also sets the insert point to that block.
///
/// \param context The codegen context.
/// \param block The block to attach.
/// \return The attached block.
IRBlock *ir_block_attach(CodegenContext *context, IRBlock *block);

/// Insert an instruction into the current insert point.
///
/// \param context The codegen context.
/// \param instruction The instruction to insert.
/// \return The inserted instruction.
IRInstruction *ir_insert(
  CodegenContext *context,
  IRInstruction *instruction
);

/// Insert an instruction after another instruction.
///
/// \param context The codegen context.
/// \param after The instruction after which to insert.
/// \param instruction The instruction to insert.
/// \return The inserted instruction.
IRInstruction *ir_insert_after(
  IRInstruction *after,
  IRInstruction *instruction
);

/// Insert an instruction at the end of a block.
///
/// It is an error to call this function on a closed block or
/// with an instruction that is already inserted somewhere.
///
/// \param block The block to append the instruction to.
/// \param instruction The instruction to append.
/// \return The inserted instruction.
/// \see ir_force_insert_at_end()
IRInstruction *ir_insert_at_end(
  IRBlock *block,
  IRInstruction *instruction
);

/// Insert an instruction at the end of a block without any checking whatsoever.
///
/// This performs no check whatsoever and will overwrite the
/// \c parent_block field of the instruction to be inserted
/// without trying to remove it from its parent. Use this only
/// in edge cases when moving instructions between blocks.
///
/// Prefer to use \c ir_insert_at_end() instead if possible.
///
/// \param block The block to append the instruction to.
/// \param instruction The instruction to append.
/// \return The inserted instruction.
/// \see ir_force_insert_at_end()
IRInstruction *ir_force_insert_at_end(
  IRBlock *block,
  IRInstruction *instruction
);

/// Insert an instruction before another instruction.
///
/// \param context The codegen context.
/// \param before The instruction before which to insert.
/// \param instruction The instruction to insert.
/// \return The inserted instruction.
IRInstruction *ir_insert_before(
  IRInstruction *before,
  IRInstruction *instruction
);

/// These `ir_insert_X` functions are the same as calling
/// `ir_insert(context, ir_X(...))`.
IRInstruction *ir_insert_alloca(CodegenContext *context, Type *type);
IRInstruction *ir_insert_bitcast(CodegenContext *context, Type *to_type, IRInstruction *value);
IRInstruction *ir_insert_br(CodegenContext *context, IRBlock *destination);
IRInstruction *ir_insert_cond_br(CodegenContext *context, IRInstruction *condition, IRBlock *then_block, IRBlock *else_block);
IRInstruction *ir_insert_copy(CodegenContext *context, IRInstruction *source);
IRInstruction *ir_insert_func_ref(CodegenContext *context, IRFunction *function);
IRInstruction *ir_insert_immediate(CodegenContext *context, Type *type, u64 immediate);
IRInstruction *ir_insert_intrinsic(CodegenContext *context, Type *t, enum IntrinsicKind intrinsic);
IRInstruction *ir_insert_load(CodegenContext *context, Type *type, IRInstruction *address);
IRInstruction *ir_insert_not(CodegenContext *context, IRInstruction *source);
IRInstruction *ir_insert_phi(CodegenContext *context, Type *type);
IRInstruction *ir_insert_return(CodegenContext *context, IRInstruction *ret);
IRInstruction *ir_insert_sext(CodegenContext *context, Type *result_type, IRInstruction *value);
IRInstruction *ir_insert_static_ref(CodegenContext *context, IRStaticVariable *var);
IRInstruction *ir_insert_store(CodegenContext *context, IRInstruction *data, IRInstruction *address);
IRInstruction *ir_insert_trunc(CodegenContext *context, Type *result_type, IRInstruction *value);
IRInstruction *ir_insert_zext(CodegenContext *context, Type *result_type, IRInstruction *value);

/// Binary instructions.
#define F(_, name) \
  IRInstruction *ir_insert_##name(CodegenContext *context, IRInstruction *lhs, IRInstruction *rhs);
ALL_BINARY_INSTRUCTION_TYPES(F)
#undef F

/// ===========================================================================
///  Operations on instructions.
/// ===========================================================================
/// Get the context from a function.
NODISCARD CodegenContext *ir_context(IRFunction *func);

/// Delete a block and all of its instructions from a function. If
/// there are any PHI nodes referencing this block, the incoming
/// values will be removed.
void ir_delete_block(IRBlock *block);

/// Delete a function from the context. This frees the function,
/// its blocks and instructions, and removes it from the list
/// of functions in the context.
void ir_delete_function(IRFunction *f);

/// Remove an instruction from its block without checking
/// if it is still used.
void ir_force_remove(IRInstruction *instruction);

/// Mark a block as ending w/ `unreachable` and remove it
/// from PHIs etc.
void ir_make_unreachable(IRBlock *block);

/// Move all instructions from one block to the end of another.
///
/// This also merges PHIs properly and updates all PHIs that
/// point to `from` to point to `into` instead. `into` must not
/// have a terminator.
///
/// The merged block is left empty and should be deleted.
///
/// \param into The block to which to append the instructions.
/// \param from The block to steal the instructions from.
void ir_merge_blocks(IRBlock *into, IRBlock *from);

/// Print IR.
void ir_print_instruction(FILE *file, IRInstruction *instruction);
void ir_print_block(FILE *file, IRBlock *block);
void ir_print_function(FILE *file, IRFunction *function);
void ir_print(FILE *file, CodegenContext *context);

/// Print CFG for a function in DOT format.
void ir_print_dot_cfg(CodegenContext *ctx);

/// Print DJ graph for a function in DOT format.
void ir_print_dot_dj(CodegenContext *ctx);

/// Remove an instruction from its block.
///
/// It is an error to call this function if it is still
/// used anywhere. If you need to remove a used instruction,
/// call ir_force_remove() instead.
///
/// \param instruction The instruction to remove.
void ir_remove(IRInstruction *instruction);

/// Replace an instruction with a new instruction.
///
/// If the new instruction is not inserted anywhere, it will
/// be inserted in the same place as the old instruction. All
/// uses of the old instruction will be replaced with the new
/// one. The old instruction will be deleted.
///
/// \param old The instruction to replace and delete.
/// \param new The instruction to replace it with.
/// \return The new instruction.
IRInstruction *ir_replace(IRInstruction *old, IRInstruction *new);

/// Replace the uses of an instruction with a new instruction.
///
/// Unlike ir_replace(), this does not remove the old instruction.
/// This is useful if that instruction has side effects. The new
/// instruction is expected to already be inserted somewhere and
/// will not be altered.
///
/// If the new instruction uses the old instruction, that use
/// will not be removed. This can be used to, e.g. ‘wrap’ an
/// instruction with a copy or cast.
///
/// \param old The instruction whose uses to replace.
/// \param new The instruction to replace it with.
void ir_replace_uses(IRInstruction *old, IRInstruction *new);

/// Set IDs for all instructions in a function.
///
/// \param f The function to set IDs for.
/// \param include_params Whether to include IR_PARAMETER instructions.
///        Pass true if you’re unsure.
void ir_set_func_ids(IRFunction *f);
void ir_set_ids(CodegenContext *context);

/// ===========================================================================
///  Internal Functions
/// ===========================================================================
/// Do not call these directly.
NODISCARD Type *ir_typeof_impl_i(IRInstruction *);
NODISCARD Type *ir_typeof_impl_f(IRFunction *);
NODISCARD IRInstruction *ir_create_call_impl_i(CodegenContext *, IRInstruction *);
NODISCARD IRInstruction *ir_create_call_impl_f(CodegenContext *, IRFunction *);
bool ir_attribute_impl_get(IRFunction *, enum FunctionAttribute);
void ir_attribute_impl_set(IRFunction *, enum FunctionAttribute, bool);
NODISCARD usz ir_func_regs_in_use_impl_get(IRFunction *);
void ir_func_regs_in_use_impl_set(IRFunction *, usz);
NODISCARD span ir_name_b_impl_get(IRBlock *);
void ir_name_b_impl_set(IRBlock *, string);
NODISCARD span ir_name_f_impl_get(IRFunction *);
void ir_name_f_impl_set(IRFunction *, string);
NODISCARD IRBlock **ir_blocks_begin_impl(IRFunction *);
NODISCARD IRBlock **ir_blocks_end_impl(IRFunction *);
NODISCARD IRInstruction **ir_instructions_begin_impl(IRBlock *);
NODISCARD IRInstruction **ir_instructions_end_impl(IRBlock *);
NODISCARD IRInstruction **ir_users_begin_impl(IRInstruction *);
NODISCARD IRInstruction **ir_users_end_impl(IRInstruction *);
NODISCARD IRBlock *ir_parent_impl_i(IRInstruction *);
NODISCARD IRFunction *ir_parent_impl_b(IRBlock *);
NODISCARD IRInstruction **ir_it_impl_i(IRInstruction *);
NODISCARD IRBlock **ir_it_impl_b(IRBlock *);
NODISCARD SymbolLinkage ir_linkage_impl_f(IRFunction *);
NODISCARD SymbolLinkage ir_linkage_impl_v(IRStaticVariable *);
NODISCARD usz ir_count_impl_b(IRBlock *);
NODISCARD usz ir_count_impl_f(IRFunction *);
NODISCARD IRInstruction *ir_call_arg_impl_get(IRInstruction *, usz);
void ir_call_arg_impl_set(IRInstruction *, usz, IRInstruction *);
NODISCARD IRValue ir_callee_impl_get(IRInstruction *);
void ir_callee_impl_set(IRInstruction *, IRValue, bool direct);
NODISCARD u32 ir_id_i_impl_get(IRInstruction *);
void ir_id_i_impl_set(IRInstruction *, u32);
NODISCARD u32 ir_id_b_impl_get(IRBlock *);
void ir_id_b_impl_set(IRBlock *, u32);
void ir_debug_iterators_impl(const void*, const void *, const void *, const void *);

#define DECLARE_ACCESSORS(name, obj_type, field_type) \
  NODISCARD field_type name##_impl_get(obj_type);     \
  void name##_impl_set(obj_type, field_type)

DECLARE_ACCESSORS(ir_alloca_offset, IRInstruction *, usz);
DECLARE_ACCESSORS(ir_alloca_size, IRInstruction *, usz);
DECLARE_ACCESSORS(ir_call_force_inline, IRInstruction *, bool);
DECLARE_ACCESSORS(ir_call_tail, IRInstruction *, bool);
DECLARE_ACCESSORS(ir_cond, IRInstruction *, IRInstruction *);
DECLARE_ACCESSORS(ir_dest, IRInstruction *, IRBlock *);
DECLARE_ACCESSORS(ir_else, IRInstruction *, IRBlock *);
DECLARE_ACCESSORS(ir_imm, IRInstruction *, usz);
DECLARE_ACCESSORS(ir_intrinsic_kind, IRInstruction *, enum IntrinsicKind);
DECLARE_ACCESSORS(ir_lhs, IRInstruction *, IRInstruction *);
DECLARE_ACCESSORS(ir_location_i, IRInstruction *, loc);
DECLARE_ACCESSORS(ir_location_f, IRFunction *, loc);
DECLARE_ACCESSORS(ir_mir_i, IRInstruction *, MIRInstruction *);
DECLARE_ACCESSORS(ir_mir_b, IRBlock *, MIRBlock *);
DECLARE_ACCESSORS(ir_mir_f, IRFunction *, MIRFunction *);
DECLARE_ACCESSORS(ir_operand, IRInstruction *, IRInstruction *);
DECLARE_ACCESSORS(ir_register, IRInstruction *, Register);
DECLARE_ACCESSORS(ir_rhs, IRInstruction *, IRInstruction *);
DECLARE_ACCESSORS(ir_static_var_init, IRStaticVariable *, IRInstruction *);
DECLARE_ACCESSORS(ir_store_addr, IRInstruction *, IRInstruction *);
DECLARE_ACCESSORS(ir_store_value, IRInstruction *, IRInstruction *);
DECLARE_ACCESSORS(ir_then, IRInstruction *, IRBlock *);

#undef DECLARE_ACCESSORS

#endif /* INTERCEPT_IR_IR_H */
