#include <codegen.h>
#include <codegen/llvm/llvm_target.h>
#include <ctype.h>
#include <ir/ir.h>

#define NO_INDEX (-1u) /// Index for valueless instructions.

/// IR generation context.
typedef struct LLVMContext {
  CodegenContext *cg;
  string_buffer out;

  /// Used intrinsics.
  bool llvm_debugtrap_used : 1;
  bool llvm_memcpy_used    : 1;
} LLVMContext;

/// Forward decl because mutual recursion.
static void emit_type(LLVMContext *ctx, Type *t);

/// Emit struct members between braces.
static void emit_struct_members(LLVMContext *ctx, Type *t) {
  string_buffer *out = &ctx->out;
  format_to(out, "{ ");

  /// Add the struct members.
  ///
  /// FIXME: LLVM addresses struct fields by index, but it has no concept
  /// of `alignas`, so we have to insert padding bytes manually into the
  /// struct if a member is overaligned; this however, invalidates the
  /// indices of the members after the padding, so actually, members also
  /// need to store a `backend_index`, which is the *actual* index of the
  /// member in the LLVM struct type.
  Type *canon = type_canonical(t);
  bool first = true;
  foreach (m, canon->structure.members) {
    if (first) first = false;
    else format_to(out, ", ");
    emit_type(ctx, m->type);
  }

  format_to(out, " }");
}

/// Emit an LLVM type.
static void emit_type(LLVMContext *ctx, Type *t) {
  /// Get canonical type or last reference.
  string_buffer *out = &ctx->out;
  Type *canon = type_canonical(t);
  ASSERT(canon, "Cannot emit incomplete type in LLVM codegen: %T", t);

  STATIC_ASSERT(TYPE_COUNT == 8, "Handle all type kinds");
  switch (canon->kind) {
    case TYPE_COUNT: UNREACHABLE();
    case TYPE_NAMED: UNREACHABLE();
    case TYPE_PRIMITIVE:
      if (canon == t_void) return format_to(out, "void");
      if (canon == t_void_ptr) return format_to(out, "ptr");
      if (canon == t_byte) return format_to(out, "i8");
      if (canon == t_integer || canon == t_integer_literal)
        return format_to(out, "i%u", ctx->cg->ffi.integer_size);
      UNREACHABLE();

    /// All pointers are just 'ptr'.
    case TYPE_POINTER: return format_to(out, "ptr");

    /// References too. We add some attributes to them if they’re
    /// parameters, but that doesn’t happen here.
    case TYPE_REFERENCE: return format_to(out, "ptr");

    /// This is the easy one.
    case TYPE_INTEGER: return format_to(out, "i%Z", canon->integer.bit_width);

    /// Named structs are referred to by their name; anonymous structs
    /// are always emitted in-line.
    case TYPE_STRUCT: {
      if (canon->structure.decl->struct_decl->name.data) {
        format_to(out, "%%struct.%S", canon->structure.decl->struct_decl->name);
        return;
      }

      emit_struct_members(ctx, canon);
      return;
    }

    case TYPE_ARRAY:
      format_to(out, "[%Z x ", canon->array.size);
      emit_type(ctx, canon->array.of);
      format_to(out, "]");
      return;

    /// Should never need this as function types are only used in calls and
    /// declarations, where they need to be emitted manually anyway.
    case TYPE_FUNCTION: UNREACHABLE();
  }
}

/// Check if a bitcast should be elided.
static bool is_noop_bitcast(IRInstruction *inst) {
  ASSERT(ir_kind(inst) == IR_BITCAST);

  /// Types are the same.
  Type *from = type_canonical(ir_typeof(ir_operand(inst)));
  Type *to = type_canonical(ir_typeof(inst));
  if (type_equals_canon(from, to)) return true;

  /// Either is a function pointer and the other is a function. No
  /// need to check for the function type since Sema has already
  /// verified that this conversion is valid.
  if (from->kind == TYPE_POINTER && to->kind == TYPE_FUNCTION) return true;
  if (from->kind == TYPE_FUNCTION && to->kind == TYPE_POINTER) return true;

  /// Not pointless (hopefully).
  return false;
}

/// Check if an instruction is a value.
///
/// This determines whether an instruction should be emitted as
/// `%N = ...`. We use numbered temporaries (%0, %1) and LLVM is
/// very strict about those; if we miss one, it will refuse to
/// compile our program. That’s a good thing since it serves as
/// an extra form of validation.
///
/// We already have a function that does something similar, that
/// is `ir_is_value()`, but unfortunately, it currently seems to
/// have different semantics and considers calls to void functions
/// values, whereas LLVM does not; furthermore, we it also considers
/// immediates values, whereas LLVM always inlines them.
static bool llvm_is_numbered_value(IRInstruction *inst) {
  STATIC_ASSERT(IR_COUNT == 40, "Handle all IR instructions");
  switch (ir_kind(inst)) {
    case IR_COUNT: break;
    case IR_IMMEDIATE:   /// Inlined.
    case IR_STATIC_REF:  /// Inlined.
    case IR_FUNC_REF:    /// Inlined.
    case IR_PARAMETER:   /// Inlined.
    case IR_COPY:        /// Replaced with the operand.
    case IR_LIT_INTEGER: /// Always global.
    case IR_LIT_STRING:  /// Always global.
    case IR_RETURN:
    case IR_BRANCH:
    case IR_UNREACHABLE:
    case IR_STORE:
      return false;

    /// Conditional branches are not values, but the conditions are,
    /// and since we currently don’t have a bool type, we need to
    /// allocate an extra number to narrow the condition to one.
    ///
    /// TODO: Remove this once we have bools in Intercept.
    case IR_BRANCH_CONDITIONAL:
      return true;

    /// The frontend uses this to convert between types that have the
    /// same size; however, we can’t convert from a type to itself, and
    /// we can especially not convert a function to a function pointer
    /// like that; the latter conversion is implicit.
    case IR_BITCAST:
      return !is_noop_bitcast(inst);

    case IR_LOAD:
    case IR_PHI:
    case IR_ADD:
    case IR_SUB:
    case IR_MUL:
    case IR_DIV:
    case IR_MOD:
    case IR_SHL:
    case IR_SAR:
    case IR_SHR:
    case IR_AND:
    case IR_OR:
    case IR_LT:
    case IR_LE:
    case IR_GT:
    case IR_GE:
    case IR_EQ:
    case IR_NE:
    case IR_ZERO_EXTEND:
    case IR_SIGN_EXTEND:
    case IR_TRUNCATE:
    case IR_NOT:
    case IR_ALLOCA:
    case IR_POISON:
      return true;

    case IR_CALL:
      return !type_equals(ir_call_callee_type(inst)->function.return_type, t_void);

    case IR_INTRINSIC: {
      STATIC_ASSERT(INTRIN_BACKEND_COUNT == 3, "Handle all intrinsics");
      switch (ir_intrinsic_kind(inst)) {
        IGNORE_FRONTEND_INTRINSICS()
        case INTRIN_BUILTIN_SYSCALL: return true;
        case INTRIN_BUILTIN_DEBUGTRAP: return false;
        case INTRIN_BUILTIN_MEMCPY: return false;
      }

      UNREACHABLE();
    }

    /// If we encounter this one, then, er, idk, scream violently I guess.
    case IR_REGISTER: ICE("LLVM backend cannot emit IR_REGISTER instructions");
  }

  UNREACHABLE();
}

/// Emit a string as an LLVM string value.
void emit_string_data(LLVMContext *ctx, span s, bool print_type) {
  string_buffer *out = &ctx->out;
  if (print_type) format_to(out, "[%Z x i8] ", s.size + 1);
  format_to(out, "c\"");

  /// Emit data.
  foreach_index (i, s) {
    char c = s.data[i];
    if (isprint(c) && c != '"' && c != '\\' && c != '\n' && c != '\r' && c != '\t' && c != '\v' && c != '\f') {
      format_to(out, "%c", c);
    } else {
      char arr[4] = {0};
      snprintf(arr, sizeof(arr), "\\%02X", c);
      vector_append(*out, literal_span(arr));
    }
  }

  /// Emit null terminator and close string.
  format_to(out, "\\00\"");
}

/// Emit an LLVM value.
///
/// This emits a value, meaning a reference to an instruction, global,
/// inline immediate etc. This is intended to be used when emitting
/// operands of instructions.
static void emit_value(LLVMContext *ctx, IRInstruction *value, bool print_type) {
  string_buffer *out = &ctx->out;
  STATIC_ASSERT(IR_COUNT == 40, "Handle all IR instructions");

  /// Emit the type if requested.
  if (print_type) {
    /// If this is a function reference, then emit the
    /// type as `ptr`. The function type is only used
    /// by calls, and calls emit the type manually.
    if (ir_kind(value) == IR_FUNC_REF) {
      format_to(out, "ptr ");
    } else {
      emit_type(ctx, ir_typeof(value));
      format_to(out, " ");
    }
  }

  /// Emit the value.
  switch (ir_kind(value)) {
    case IR_COUNT: UNREACHABLE();

    case IR_POISON:
      format_to(out, "poison");
      return;

    /// Immediates are always emitted in-line.
    case IR_IMMEDIATE:
    case IR_LIT_INTEGER:
      format_to(out, "%I", (i64) ir_imm(value));
      return;

    /// Copies just forward the copied value.
    case IR_COPY:
      emit_value(ctx, ir_operand(value), false);
      return;

    /// The first N temporaries are the parameters.
    case IR_PARAMETER:
      format_to(out, "%%%U", ir_imm(value));
      return;

    /// These are referenced by name.
    case IR_STATIC_REF:
      format_to(out, "@%S", ir_static_ref_var(value)->name);
      return;

    case IR_FUNC_REF:
      format_to(out, "@%S", ir_name(ir_func_ref_func(value)));
      return;

    /// Strings are emitted in-line since the semantics of
    /// string literals is different from C.
    case IR_LIT_STRING:
      emit_string_data(ctx, ir_string_data(ctx->cg, value), false);
      return;

    case IR_REGISTER:
      ICE("LLVM backend cannot emit IR_REGISTER instructions");

    case IR_RETURN:
    case IR_BRANCH:
    case IR_BRANCH_CONDITIONAL:
    case IR_UNREACHABLE:
    case IR_STORE:
      ICE("Refusing to emit non-value as value");

    case IR_BITCAST:
      /// If this is a noop bitcast, just emit the operand. If
      /// this is a real bitcast, emit the index.
      if (ir_id(value) == NO_INDEX) emit_value(ctx, ir_operand(value), false);
      else format_to(out, "%%%u", ir_id(value));
      return;

    case IR_INTRINSIC: {
      STATIC_ASSERT(INTRIN_BACKEND_COUNT == 3, "Handle all intrinsics");
      switch (ir_intrinsic_kind(value)) {
        IGNORE_FRONTEND_INTRINSICS()
        case INTRIN_BUILTIN_SYSCALL:
          format_to(out, "%%%u", ir_id(value));
          return;

        /// Not a value.
        case INTRIN_BUILTIN_DEBUGTRAP:
        case INTRIN_BUILTIN_MEMCPY:
          ICE("Refusing to emit non-value as value");
      }

      UNREACHABLE();
    }

    /// These all have values, so emit their indices.
    case IR_CALL:
    case IR_LOAD:
    case IR_PHI:
    case IR_ADD:
    case IR_SUB:
    case IR_MUL:
    case IR_DIV:
    case IR_MOD:
    case IR_SHL:
    case IR_SAR:
    case IR_SHR:
    case IR_AND:
    case IR_OR:
    case IR_LT:
    case IR_LE:
    case IR_GT:
    case IR_GE:
    case IR_EQ:
    case IR_NE:
    case IR_ZERO_EXTEND:
    case IR_SIGN_EXTEND:
    case IR_TRUNCATE:
    case IR_NOT:
    case IR_ALLOCA:
      format_to(out, "%%%u", ir_id(value));
      return;
  }
}

/// Emit the index of an instruction, if any.
static void emit_instruction_index(LLVMContext *ctx, IRInstruction *inst) {
  string_buffer *out = &ctx->out;
  if (ir_id(inst) != NO_INDEX) format_to(out, "    %%%u = ", ir_id(inst));
  else format_to(out, "    ");
}

/// Emit a binary instruction.
static void emit_binary(
  LLVMContext *ctx,
  const char *op,
  IRInstruction *inst
) {
  string_buffer *out = &ctx->out;
  emit_instruction_index(ctx, inst);
  format_to(out, "%s ", op);
  emit_value(ctx, ir_lhs(inst), true);
  format_to(out, ", ");
  emit_value(ctx, ir_rhs(inst), false);
  format_to(out, "\n");
}

/// Emit a binary instruction that cares about signedness.
static void emit_binary_signed(
  LLVMContext *ctx,
  const char *op_signed,
  const char *op_unsigned,
  IRInstruction *inst
) {
  string_buffer *out = &ctx->out;
  emit_instruction_index(ctx, inst);
  format_to(out, "%s ", type_is_signed(ir_typeof(ir_lhs(inst))) ? op_signed : op_unsigned);
  emit_value(ctx, ir_lhs(inst), true);
  format_to(out, ", ");
  emit_value(ctx, ir_rhs(inst), false);
  format_to(out, "\n");
}

/// Emit a conversion operation (`X ... to Y`).
static void emit_conversion(
  LLVMContext *ctx,
  const char *op,
  IRInstruction *inst
) {
  string_buffer *out = &ctx->out;
  emit_instruction_index(ctx, inst);
  format_to(out, "%s ", op);
  emit_value(ctx, ir_operand(inst), true);
  format_to(out, " to ");
  emit_type(ctx, ir_typeof(inst));
  format_to(out, "\n");
}

/// Emit an add of a pointer and an int.
static void emit_pointer_add(
  LLVMContext *ctx,
  IRInstruction *inst,
  IRInstruction *pointer,
  IRInstruction *integer
) {
  string_buffer *out = &ctx->out;

  /// If the displacement is known at compile time and a multiple of
  /// the pointee size, convert it to a GEP and emit that instead.
  Type *ptr_type = ir_typeof(pointer);
  if (ir_kind(integer) == IR_IMMEDIATE && ir_imm(integer) % type_sizeof(ptr_type->pointer.to) == 0) {
    emit_instruction_index(ctx, inst);
    format_to(out, "getelementptr ");
    emit_type(ctx, ptr_type->pointer.to);
    format_to(out, ", ");
    emit_value(ctx, pointer, true);
    format_to(out, ", i64 %Z\n", ir_imm(integer) / type_sizeof(ptr_type->pointer.to));
    return;
  }

  /// Otherwise, convert the pointer to an int.
  format_to(out, "    %%ptrtoint.%u = ptrtoint ", ir_id(inst));
  emit_value(ctx, pointer, true);
  format_to(out, " to ");
  emit_type(ctx, ir_typeof(integer));
  format_to(out, "\n");

  /// Add them.
  format_to(out, "    %%add.%u = add ", ir_id(inst));
  emit_value(ctx, integer, true);
  format_to(out, ", %%ptrtoint.%u\n", ir_id(inst));

  /// Cast back to a pointer.
  format_to(out, "    %%%u = inttoptr ", ir_id(inst));
  emit_type(ctx, ir_typeof(integer));
  format_to(out, " %%add.%u to ptr\n", ir_id(inst));
}

/// Emit an LLVM instruction.
///
/// This emits an instruction as part of a function body. For emitting
/// instructions in other places, see `emit_value`.
static void emit_instruction(LLVMContext *ctx, IRInstruction *inst) {
  string_buffer *out = &ctx->out;
  STATIC_ASSERT(IR_COUNT == 40, "Handle all IR instructions");
  switch (ir_kind(inst)) {
    case IR_COUNT: UNREACHABLE();

    /// These are emitted in-line.
    case IR_IMMEDIATE:
    case IR_POISON:
    case IR_COPY:
    case IR_STATIC_REF:
    case IR_FUNC_REF:
    case IR_PARAMETER:
    case IR_LIT_INTEGER:
    case IR_LIT_STRING:
      break;

    case IR_REGISTER:
      ICE("LLVM backend cannot emit IR_REGISTER instructions");

    case IR_INTRINSIC: {
      STATIC_ASSERT(INTRIN_BACKEND_COUNT == 3, "Handle all intrinsics");
      switch (ir_intrinsic_kind(inst)) {
        IGNORE_FRONTEND_INTRINSICS()

        /// We need to emit inline assembly for this.
        case INTRIN_BUILTIN_SYSCALL: {
          emit_instruction_index(ctx, inst);
          format_to(out, "call i64 asm sideeffect \"");

          usz args = ir_call_args_count(inst);
          if (args >= 5) format_to(out, "movq $0, %%r10\\0A");
          if (args >= 6) format_to(out, "movq $1, %%r8\\0A");
          if (args >= 7) format_to(out, "movq $2, %%r9\\0A");
          format_to(out, "syscall\\0A\", \"={ax},{ax}");
          if (args >= 2) format_to(out, ",{di}");
          if (args >= 3) format_to(out, ",{si}");
          if (args >= 4) format_to(out, ",{dx}");
          if (args >= 5) format_to(out, ",~{r10}");
          if (args >= 6) format_to(out, ",~{r8}");
          if (args >= 7) format_to(out, ",~{r9}");
          for (usz i = 5; i <= args; i++) format_to(out, ",r");
          format_to(out, ",~{memory},~{dirflag},~{fpsr},~{flags}\"(");

          bool first = true;
          for (usz i = 0; i < args; i++) {
            if (!first) format_to(out, ", ");
            else first = false;
            emit_value(ctx, ir_call_arg(inst, i), true);
          }

          format_to(out, ")\n");
          return;
        }

        /// Just call the LLVM intrinsic for this.
        case INTRIN_BUILTIN_DEBUGTRAP:
          emit_instruction_index(ctx, inst);
          format_to(out, "call void @llvm.debugtrap()\n");
          ctx->llvm_debugtrap_used = true;
          return;

        /// Same thing.
        case INTRIN_BUILTIN_MEMCPY:
          emit_instruction_index(ctx, inst);
          format_to(out, "call void @llvm.memcpy.p0.p0.i%Z(\n", type_sizeof(t_integer));
          emit_value(ctx, ir_call_arg(inst, 0), true);
          format_to(out, ", ");
          emit_value(ctx, ir_call_arg(inst, 1), true);
          format_to(out, ", ");
          emit_value(ctx, ir_call_arg(inst, 2), true);
          format_to(out, ", i1 0)\n"); /// Note: 0 = not volatile.
          ctx->llvm_memcpy_used = true;
          return;
      }

      UNREACHABLE();
    }

    case IR_CALL: {
      emit_instruction_index(ctx, inst);
      Type *call_ty = ir_call_callee_type(inst);
      format_to(out, "call ");
      emit_type(ctx, call_ty->function.return_type);

      if (!ir_call_is_direct(inst)) emit_value(ctx, ir_callee(inst).inst, false);
      else format_to(out, " @%S", ir_name(ir_callee(inst).func));

      format_to(out, "(");
      for (usz i = 0; i < ir_call_args_count(inst); i++) {
        if (i) format_to(out, ", ");
        emit_value(ctx, ir_call_arg(inst, i), true);
      }
      format_to(out, ")\n");
    } break;

    case IR_LOAD:
      emit_instruction_index(ctx, inst);
      format_to(out, "load ");
      emit_type(ctx, ir_typeof(inst));
      format_to(out, ", ");
      emit_value(ctx, ir_operand(inst), true);
      format_to(out, "\n");
      break;

    case IR_RETURN:
      format_to(out, "    ret ");
      if (ir_operand(inst)) emit_value(ctx, ir_operand(inst), true);
      else format_to(out, "void");
      format_to(out, "\n");
      break;

    case IR_BRANCH:
      format_to(out, "    br label %%bb%u\n", ir_id(ir_dest(inst)));
      break;

    case IR_BRANCH_CONDITIONAL:
      /// Narrow the condition to a bool.
      format_to(out, "    %%%u = icmp ne ", ir_id(inst));
      emit_value(ctx, ir_cond(inst), true);
      format_to(out, ", 0\n");

      /// Emit the branch.
      format_to(
        out,
        "    br i1 %%%u, label %%bb%u, label %%bb%u\n",
        ir_id(inst),
        ir_id(ir_then(inst)),
        ir_id(ir_else(inst))
      );
      break;

    case IR_UNREACHABLE:
      format_to(out, "    unreachable\n");
      break;

    case IR_PHI:
      emit_instruction_index(ctx, inst);
      format_to(out, "phi ");
      emit_type(ctx, ir_typeof(inst));
      format_to(out, " ");
      for (usz i = 0; i < ir_phi_args_count(inst); i++) {
        if (i) format_to(out, ", ");
        const IRPhiArgument *arg = ir_phi_arg(inst, i);
        format_to(out, "[ ");
        emit_value(ctx, arg->value, false);
        format_to(out, ", %%bb%u ]", ir_id(arg->block));
      }
      format_to(out, "\n");
      break;

    /// In LLVM, comparisons return an i1. However, we don’t have
    /// bools yet in intercept, so we need to convert the result
    /// back to whatever type Intercept wants it to be.
    ///
    /// TODO: Simplify this once we have bools.
    case IR_EQ:
    case IR_NE:
    case IR_LT:
    case IR_LE:
    case IR_GT:
    case IR_GE: {
      /// Emit the comparison with a temporary name.
      format_to(out, "    %%i1.%u = icmp ", ir_id(inst));
      switch (ir_kind(inst)) {
        default: UNREACHABLE();
        case IR_EQ: format_to(out, "eq "); break;
        case IR_NE: format_to(out, "ne "); break;
        case IR_LT: format_to(out, "%s", type_is_signed(ir_typeof(ir_lhs(inst))) ? "slt " : "ult "); break;
        case IR_LE: format_to(out, "%s", type_is_signed(ir_typeof(ir_lhs(inst))) ? "sle " : "ule "); break;
        case IR_GT: format_to(out, "%s", type_is_signed(ir_typeof(ir_lhs(inst))) ? "sgt " : "ugt "); break;
        case IR_GE: format_to(out, "%s", type_is_signed(ir_typeof(ir_lhs(inst))) ? "sge " : "uge "); break;
      }
      emit_value(ctx, ir_lhs(inst), true);
      format_to(out, ", ");
      emit_value(ctx, ir_rhs(inst), false);

      /// ALWAYS zero-extend an i1, as sign-extending would broadcast the sign bit.
      format_to(out, "\n    %%%u = zext i1 %%i1.%u to ", ir_id(inst), ir_id(inst));
      emit_type(ctx, ir_typeof(inst));
      format_to(out, "\n");
    } break;

    /// The frontend lowers pointer arithmetic to adds, but
    /// we need to emit a GEP for this since LLVM doesn’t
    /// let us add ints to pointers.
    case IR_ADD:
      /// Pointer arithmetic.
      if (type_is_pointer(ir_typeof(ir_lhs(inst))) && !type_is_pointer(ir_typeof(ir_rhs(inst))))
        emit_pointer_add(ctx, inst, ir_lhs(inst), ir_rhs(inst));
      else if (!type_is_pointer(ir_typeof(ir_lhs(inst))) && type_is_pointer(ir_typeof(ir_rhs(inst))))
        emit_pointer_add(ctx, inst, ir_rhs(inst), ir_lhs(inst));
      else
        emit_binary(ctx, "add", inst);
      break;

    case IR_SUB: emit_binary(ctx, "sub", inst); break;
    case IR_MUL: emit_binary(ctx, "mul", inst); break;
    case IR_SHL: emit_binary(ctx, "shl", inst); break;
    case IR_SAR: emit_binary(ctx, "ashr", inst); break;
    case IR_SHR: emit_binary(ctx, "lshr", inst); break;
    case IR_AND: emit_binary(ctx, "and", inst); break;
    case IR_OR: emit_binary(ctx, "or", inst); break;
    case IR_DIV: emit_binary_signed(ctx, "sdiv", "udiv", inst); break;
    case IR_MOD: emit_binary_signed(ctx, "srem", "urem", inst); break;

    case IR_ZERO_EXTEND: emit_conversion(ctx, "zext", inst); break;
    case IR_SIGN_EXTEND: emit_conversion(ctx, "sext", inst); break;
    case IR_TRUNCATE: emit_conversion(ctx, "trunc", inst); break;

    case IR_BITCAST: {
      if (ir_id(inst) == NO_INDEX) break;

      /// LLVM doesn’t allow bitcasts between aggregates, so
      /// we gotta do it the janky way.
      Type *from = type_canonical(ir_typeof(ir_operand(inst)));
      Type *to = type_canonical(ir_typeof(inst));
      if (from->kind == TYPE_STRUCT || to->kind == TYPE_STRUCT || from->kind == TYPE_ARRAY || to->kind == TYPE_ARRAY) {
        format_to(out, "    %%alloca.%u = alloca ", ir_id(inst));
        emit_type(ctx, to);
        format_to(out, ", align %Z\n", type_alignof(to));
        format_to(out, "    store ");
        emit_value(ctx, ir_operand(inst), true);
        format_to(out, ", ptr %%alloca.%u\n", ir_id(inst));
        format_to(out, "    %%%u = load ", ir_id(inst));
        emit_type(ctx, ir_typeof(inst));
        format_to(out, ", ptr %%alloca.%u\n", ir_id(inst));
        break;
      }

      /// If we’re converting between ptr and integers, we need
      /// to use inttoptr and ptrtoint. Otherwise, just emit a
      /// normal bitcast.
      if (from->kind == TYPE_POINTER && to->kind == TYPE_INTEGER)
        emit_conversion(ctx, "ptrtoint", inst);
      else if (from->kind == TYPE_INTEGER && to->kind == TYPE_POINTER)
        emit_conversion(ctx, "inttoptr", inst);
      else
        emit_conversion(ctx, "bitcast", inst);
    } break;

    case IR_STORE:
      format_to(out, "    store ");
      emit_value(ctx, ir_store_value(inst), true);
      format_to(out, ", ");
      emit_value(ctx, ir_store_addr(inst), true);
      format_to(out, "\n");
      break;

    /// There is no `not` instruction, so we emit a `xor` instead.
    case IR_NOT:
      emit_instruction_index(ctx, inst);
      format_to(out, "xor ");
      emit_value(ctx, ir_operand(inst), true);
      format_to(out, ", -1\n");
      break;

    case IR_ALLOCA: {
      Type *t = type_canonical(ir_typeof(inst));
      ASSERT(type_is_pointer(t));
      emit_instruction_index(ctx, inst);
      format_to(out, "alloca ");
      emit_type(ctx, t->pointer.to);

      /// Specifying the alignment isn’t strictly necessary,
      /// but it’s probably a good idea.
      format_to(out, ", align %Z\n", type_alignof(ir_typeof(inst)));
    } break;
  }
}

/// Emit an LLVM function.
///
/// This declares external functions and defines
/// defined functions.
static void emit_function(LLVMContext *ctx, IRFunction *f) {
  string_buffer *out = &ctx->out;
  if (f != *ctx->cg->functions.data) format_to(out, "\n");

  /// Write function header.
  Type *ftype = ir_typeof(f);
  format_to(out, "%s ", ir_func_is_definition(f) ? "define" : "declare");
  if (ir_linkage(f) == LINKAGE_INTERNAL) format_to(out, "private ");
  emit_type(ctx, ftype->function.return_type);
  format_to(out, " @%S(", ir_name(f));

  /// Write the argument types.
  foreach_index (i, ftype->function.parameters) {
    Parameter *p = ftype->function.parameters.data + i;
    format_to(out, "%s", i == 0 ? "" : ", ");
    emit_type(ctx, p->type);

    /// Reference parameters get some optimisation hints. This
    /// is also what Clang uses for C++ references.
    Type *canon = type_canonical(p->type);
    if (type_is_reference(canon)) {
      usz element_size = type_sizeof(canon->reference.to);
      format_to(
        out,
        " noundef nonnull align %Z dereferenceable(%Z)",
        element_size,
        element_size
      );
    }

    /// Parameter name.
    format_to(out, " %%%Z", i);
  }

  /// Write attributes.
  format_to(out, ")");
  if (ir_attribute(f, FUNC_ATTR_INLINE)) format_to(out, " alwaysinline");
  if (ir_attribute(f, FUNC_ATTR_NOINLINE)) format_to(out, " noinline");
  if (ir_attribute(f, FUNC_ATTR_NORETURN)) format_to(out, " noreturn");
  format_to(out, " nounwind%s\n", ir_func_is_definition(f) ? " {" : "");

  /// Extern functions don’t have a body.
  if (!ir_func_is_definition(f)) return;

  /// Assign indices to all instructions and blocks.
  u32 value_index = (u32) ftype->function.parameters.size;
  u32 block_index = 0;
  FOREACH_BLOCK (block, f) {
    ir_id(block, block_index++);
    FOREACH_INSTRUCTION (inst, block) {
      /// Values are numbered.
      u32 id = llvm_is_numbered_value(inst) ? value_index++ : NO_INDEX;
      ir_id(inst, id);
    }
  }

  /// Emit the function body.
  FOREACH_BLOCK (block, f) {
    if (ir_id(block)) format_to(out, "\n");
    format_to(out, "bb%u:", ir_id(block));
    if (ir_name(block).size) format_to(out, " ; %S", ir_name(block));
    format_to(out, "\n");
    FOREACH_INSTRUCTION (inst, block) {
      emit_instruction(ctx, inst);
    }
  }

  /// Close the function.
  format_to(out, "}\n");
}

void codegen_emit_llvm(CodegenContext *cg) {
  LLVMContext ctx = {
    .cg = cg,
  };

  /// We don’t want colours in our LLVM IR.
  disable_colours();

  /// Mangle all function names.
  foreach_val (f, cg->functions) mangle_function_name(f);

  /// Emit named types.
  bool type_emitted = false;
  foreach_val (t, cg->ast->_types_) {
    if (t->kind != TYPE_STRUCT || t->structure.decl->struct_decl->name.size == 0) continue;
    type_emitted = true;
    format_to(&ctx.out, "%%struct.%S = type ", t->structure.decl->struct_decl->name);
    emit_struct_members(&ctx, t);
    format_to(&ctx.out, "\n");
  }

  /// Add a newline after the types.
  if (type_emitted) format_to(&ctx.out, "\n");

  /// Emit global variables.
  foreach_val (var, cg->static_vars) {
    format_to(&ctx.out, "@%S = private global ", var->name);
    emit_type(&ctx, var->type);
    format_to(&ctx.out, " ");
    if (var->init) {
      switch (ir_kind(var->init)) {
        case IR_LIT_INTEGER: format_to(&ctx.out, "%U", ir_imm(var->init)); break;
        case IR_LIT_STRING: emit_string_data(&ctx, ir_string_data(cg, var->init), false); break;
        default: UNREACHABLE();
      }
    } else {
      format_to(&ctx.out, "zeroinitializer");
    }

    /// Globals must be aligned manually, else they are thought
    /// to have an alignment of 1, which breaks default-aligned
    /// loads and stores.
    format_to(&ctx.out, ", align %Z\n", type_alignof(var->type));
  }

  /// Add a newline after the globals.
  if (cg->static_vars.size) format_to(&ctx.out, "\n");

  /// Emit each function.
  foreach_val (f, cg->functions) emit_function(&ctx, f);

  /// Emit intrinsic declarations.
  if (ctx.llvm_debugtrap_used) format_to(&ctx.out, "declare void @llvm.debugtrap()\n");
  if (ctx.llvm_memcpy_used) format_to(&ctx.out, "declare void @llvm.memcpy.p0.p0.i%Z(ptr, ptr, i64, i1)\n", type_sizeof(t_integer));

  /// Write to file.
  fprint(cg->code, "%S", as_span(ctx.out));
  vector_delete(ctx.out);
}
