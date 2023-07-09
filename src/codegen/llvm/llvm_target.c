#include <codegen.h>
#include <codegen/intermediate_representation.h>
#include <codegen/llvm/llvm_target.h>
#include <ctype.h>
#include <stdint.h>

#define NO_INDEX (-1u) /// Index for valueless instructions.

/// IR generation context.
typedef struct LLVMContext {
    CodegenContext *cg;
    string_buffer out;
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
    ASSERT(inst->kind == IR_BITCAST);

    /// Types are the same.
    Type *from = type_canonical(inst->operand->type);
    Type *to = type_canonical(inst->type);
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
    STATIC_ASSERT(IR_COUNT == 38, "Handle all IR instructions");
    switch (inst->kind) {
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
            return true;

        case IR_CALL:
            return !type_equals(ir_call_get_callee_type(inst)->function.return_type, t_void);

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
    STATIC_ASSERT(IR_COUNT == 38, "Handle all IR instructions");

    /// Emit the type if requested.
    if (print_type) {
        /// If this is a function reference, then emit the
        /// type as `ptr`. The function type is only used
        /// by calls, and calls emit the type manually.
        if (value->kind == IR_FUNC_REF) {
            format_to(out, "ptr ");
        } else {
            emit_type(ctx, value->type);
            format_to(out, " ");
        }
    }

    /// Emit the value.
    switch (value->kind) {
        case IR_COUNT: UNREACHABLE();

        /// Immediates are always emitted in-line.
        case IR_IMMEDIATE:
        case IR_LIT_INTEGER:
            format_to(out, "%I", (i64) value->imm);
            return;

        /// Copies just forward the copied value.
        case IR_COPY:
            emit_value(ctx, value->operand, false);
            return;

        /// The first N temporaries are the parameters.
        case IR_PARAMETER:
            format_to(out, "%%%U", value->imm);
            return;

        /// These are referenced by name.
        case IR_STATIC_REF:
        case IR_FUNC_REF:
            format_to(out, "@%S", value->static_ref->name);
            return;

        /// Strings are emitted in-line since the semantics of
        /// string literals is different from C.
        case IR_LIT_STRING:
            emit_string_data(ctx, as_span(value->str), false);
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
            if (value->index == NO_INDEX) emit_value(ctx, value->operand, false);
            else format_to(out, "%%%u", value->index);
            return;

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
            format_to(out, "%%%u", value->index);
            return;
    }
}

/// Emit the index of an instruction, if any.
static void emit_instruction_index(LLVMContext *ctx, IRInstruction *inst) {
    string_buffer *out = &ctx->out;
    if (inst->index != NO_INDEX) format_to(out, "    %%%u = ", inst->index);
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
    emit_value(ctx, inst->lhs, true);
    format_to(out, ", ");
    emit_value(ctx, inst->rhs, false);
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
    format_to(out, "%s ", type_is_signed(inst->lhs->type) ? op_signed : op_unsigned);
    emit_value(ctx, inst->lhs, true);
    format_to(out, ", ");
    emit_value(ctx, inst->rhs, false);
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
    emit_value(ctx, inst->lhs, true);
    format_to(out, " to ");
    emit_type(ctx, inst->type);
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
    if (integer->kind == IR_IMMEDIATE && integer->imm % type_sizeof(pointer->type->pointer.to) == 0) {
        emit_instruction_index(ctx, inst);
        format_to(out, "getelementptr ");
        emit_type(ctx, pointer->type->pointer.to);
        format_to(out, ", ");
        emit_value(ctx, pointer, true);
        format_to(out, ", i64 %Z\n", integer->imm / type_sizeof(pointer->type->pointer.to));
        return;
    }

    /// Otherwise, convert the pointer to an int.
    format_to(out, "    %%ptrtoint.%u = ptrtoint ", inst->index);
    emit_value(ctx, pointer, true);
    format_to(out, " to ");
    emit_type(ctx, integer->type);
    format_to(out, "\n");

    /// Add them.
    format_to(out, "    %%add.%u = add ", inst->index);
    emit_value(ctx, integer, true);
    format_to(out, ", %%ptrtoint.%u\n", inst->index);

    /// Cast back to a pointer.
    format_to(out, "    %%%u = inttoptr ", inst->index);
    emit_type(ctx, integer->type);
    format_to(out, " %%add.%u to ptr\n", inst->index);
}

/// Emit an LLVM instruction.
///
/// This emits an instruction as part of a function body. For emitting
/// instructions in other places, see `emit_value`.
static void emit_instruction(LLVMContext *ctx, IRInstruction *inst) {
    string_buffer *out = &ctx->out;
    STATIC_ASSERT(IR_COUNT == 38, "Handle all IR instructions");
    switch (inst->kind) {
        case IR_COUNT: UNREACHABLE();

        /// These are emitted in-line.
        case IR_IMMEDIATE:
        case IR_COPY:
        case IR_STATIC_REF:
        case IR_FUNC_REF:
        case IR_PARAMETER:
        case IR_LIT_INTEGER:
        case IR_LIT_STRING:
            break;

        case IR_REGISTER:
            ICE("LLVM backend cannot emit IR_REGISTER instructions");

        case IR_CALL: {
            emit_instruction_index(ctx, inst);
            Type *call_ty = ir_call_get_callee_type(inst);
            format_to(out, "call ");
            emit_type(ctx, call_ty->function.return_type);

            if (inst->call.is_indirect) emit_value(ctx, inst->call.callee_instruction, false);
            else format_to(out, " @%S", inst->call.callee_function->name);

            format_to(out, "(");
            foreach_val (arg, inst->call.arguments) {
                if (arg != *inst->call.arguments.data) format_to(out, ", ");
                emit_value(ctx, arg, true);
            }
            format_to(out, ")\n");
        } break;

        case IR_LOAD:
            emit_instruction_index(ctx, inst);
            format_to(out, "load ");
            emit_type(ctx, inst->type);
            format_to(out, ", ");
            emit_value(ctx, inst->operand, true);
            format_to(out, "\n");
            break;

        case IR_RETURN:
            format_to(out, "    ret ");
            if (inst->operand) emit_value(ctx, inst->operand, true);
            else format_to(out, "void");
            format_to(out, "\n");
            break;

        case IR_BRANCH:
            format_to(out, "    br label %%bb%Z\n", inst->destination_block->id);
            break;

        case IR_BRANCH_CONDITIONAL:
            /// Narrow the condition to a bool.
            format_to(out, "    %%%u = icmp ne ", inst->index);
            emit_value(ctx, inst->cond_br.condition, true);
            format_to(out, ", 0\n");

            /// Emit the branch.
            format_to(
                out,
                "    br i1 %%%u, label %%bb%Z, label %%bb%Z\n",
                inst->index,
                inst->cond_br.then->id,
                inst->cond_br.else_->id
            );
            break;

        case IR_UNREACHABLE:
            format_to(out, "    unreachable\n");
            break;

        case IR_PHI:
            emit_instruction_index(ctx, inst);
            format_to(out, "phi ");
            emit_type(ctx, inst->type);
            format_to(out, " ");
            foreach_val (arg, inst->phi_args) {
                if (arg != *inst->phi_args.data) format_to(out, ", ");
                format_to(out, "[ ");
                emit_value(ctx, arg->value, false);
                format_to(out, ", %%bb%Z ]", arg->block->id);
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
            format_to(out, "    %%i1.%u = icmp ", inst->index);
            switch (inst->kind) {
                default: UNREACHABLE();
                case IR_EQ: format_to(out, "eq "); break;
                case IR_NE: format_to(out, "ne "); break;
                case IR_LT: format_to(out, "%s", type_is_signed(inst->lhs->type) ? "slt " : "ult "); break;
                case IR_LE: format_to(out, "%s", type_is_signed(inst->lhs->type) ? "sle " : "ule "); break;
                case IR_GT: format_to(out, "%s", type_is_signed(inst->lhs->type) ? "sgt " : "ugt "); break;
                case IR_GE: format_to(out, "%s", type_is_signed(inst->lhs->type) ? "sge " : "uge "); break;
            }
            emit_value(ctx, inst->lhs, true);
            format_to(out, ", ");
            emit_value(ctx, inst->rhs, false);

            /// ALWAYS zero-extend an i1, as sign-extending would broadcast the sign bit.
            format_to(out, "\n    %%%u = zext i1 %%i1.%u to ", inst->index, inst->index);
            emit_type(ctx, inst->type);
            format_to(out, "\n");
        } break;

        /// The frontend lowers pointer arithmetic to adds, but
        /// we need to emit a GEP for this since LLVM doesn’t
        /// let us add ints to pointers.
        case IR_ADD:
            /// Pointer arithmetic.
            if (type_is_pointer(inst->lhs->type) && !type_is_pointer(inst->rhs->type))
                emit_pointer_add(ctx, inst, inst->lhs, inst->rhs);
            else if (!type_is_pointer(inst->lhs->type) && type_is_pointer(inst->rhs->type))
                emit_pointer_add(ctx, inst, inst->rhs, inst->lhs);
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
            if (inst->index == NO_INDEX) break;

            /// LLVM doesn’t allow bitcasts between aggregates, so
            /// we gotta do it the janky way.
            Type *from = type_canonical(inst->operand->type);
            Type *to = type_canonical(inst->type);
            if (from->kind == TYPE_STRUCT || to->kind == TYPE_STRUCT ||
                from->kind == TYPE_ARRAY || to->kind == TYPE_ARRAY) {
                format_to(out, "    %%alloca.%u = alloca ", inst->index);
                emit_type(ctx, to);
                format_to(out, ", align %Z\n", type_alignof(to));
                format_to(out, "    store ");
                emit_value(ctx, inst->operand, true);
                format_to(out, ", ptr %%alloca.%u\n", inst->index);
                format_to(out, "    %%%u = load ", inst->index);
                emit_type(ctx, inst->type);
                format_to(out, ", ptr %%alloca.%u\n", inst->index);
                break;
            }

            /// Otherwise, just emit a normal bitcast.
            emit_conversion(ctx, "bitcast", inst);
        } break;

        case IR_STORE:
            format_to(out, "    store ");
            emit_value(ctx, inst->store.value, true);
            format_to(out, ", ");
            emit_value(ctx, inst->store.addr, true);
            format_to(out, "\n");
            break;

        /// There is no `not` instruction, so we emit a `xor` instead.
        case IR_NOT:
            emit_instruction_index(ctx, inst);
            format_to(out, "xor ");
            emit_value(ctx, inst->operand, true);
            format_to(out, ", -1\n");
            break;

        case IR_ALLOCA: {
            Type *t = type_canonical(inst->type);
            ASSERT(type_is_pointer(t));
            emit_instruction_index(ctx, inst);
            format_to(out, "alloca ");
            emit_type(ctx, t->pointer.to);

            /// Specifying the alignment isn’t strictly necessary,
            /// but it’s probably a good idea.
            format_to(out, ", align %Z\n", type_alignof(inst->type));
        } break;
    }
}

/// Emit an LLVM function.
///
/// This declares external functions and defines
/// defined functions.
static void emit_function(LLVMContext *ctx, IRFunction *f) {
    string_buffer *out = &ctx->out;
    if (f != *f->context->functions.data) format_to(out, "\n");

    /// Write function header.
    format_to(out, "%s ", f->is_extern ? "declare" : "define");
    if (!f->is_extern && !string_eq(f->name, literal_span("main")))
        format_to(out, "private "); /// TODO: Make private only if not exported.
    emit_type(ctx, f->type->function.return_type);
    format_to(out, " @%S(", f->name);

    /// Write the argument types.
    foreach_index (i, f->type->function.parameters) {
        Parameter *p = f->type->function.parameters.data + i;
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
    if (f->attr_forceinline) format_to(out, " alwaysinline");
    if (f->attr_noreturn) format_to(out, " noreturn");
    format_to(out, " nounwind%s\n", f->is_extern ? "" : " {");

    /// Extern functions don’t have a body.
    if (f->is_extern) return;

    /// Assign indices to all instructions and blocks.
    u32 value_index = (u32) f->type->function.parameters.size;
    usz block_index = 0;
    list_foreach (block, f->blocks) {
        block->id = block_index++;
        list_foreach (inst, block->instructions) {
            /// Values are numbered.
            inst->index = llvm_is_numbered_value(inst)
                            ? value_index++
                            : NO_INDEX;
        }
    }

    /// Emit the function body.
    list_foreach (block, f->blocks) {
        if (block->id) format_to(out, "\n");
        format_to(out, "bb%Z:", block->id);
        if (block->name.size) format_to(out, " ; %S", block->name);
        format_to(out, "\n");
        list_foreach (inst, block->instructions) {
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
            switch (var->init->kind) {
                case IR_LIT_INTEGER: format_to(&ctx.out, "%U", var->init->imm); break;
                case IR_LIT_STRING: emit_string_data(&ctx, as_span(var->init->str), false); break;
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

    /// Write to file.
    fprint(cg->code, "%S", as_span(ctx.out));
    vector_delete(ctx.out);
}
