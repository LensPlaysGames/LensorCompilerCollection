#include <ast.h>
#include <codegen.h>
#include <codegen/codegen_forward.h>
#include <codegen/instruction_selection.h>
#include <codegen/machine_ir.h>
#include <codegen/opt/opt.h>
#include <codegen/register_allocation.h>
#include <codegen/x86_64/arch_x86_64.h>
#include <codegen/x86_64/arch_x86_64_common.h>
#include <codegen/x86_64/arch_x86_64_isel.h>
#include <codegen/x86_64/arch_x86_64_tgt_assembly.h>
#include <codegen/x86_64/arch_x86_64_tgt_generic_object.h>
#include <error.h>
#include <inttypes.h>
#include <ir/ir.h>
#include <module.h>
#include <parser.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <typechecker.h>
#include <utils.h>
#include <vector.h>

#define X86_64_GENERATE_MACHINE_CODE

/// FIXME: JANK.
Register *caller_saved_registers = NULL;
size_t caller_saved_register_count = 0;

/// FIXME: JANK.
Register *argument_registers = NULL;
size_t argument_register_count = 0;

span unreferenced_block_name = literal_span_raw("");

NODISCARD static bool is_caller_saved(Register r) {
  for (size_t i = 0; i < caller_saved_register_count; ++i) {
    if (caller_saved_registers[i] == r) {
      return 1;
    }
  }
  return 0;
}
NODISCARD static bool is_callee_saved(Register r) {
  return !is_caller_saved(r);
}

// Maximum size of parameter that can go in a register vs on the stack.
// TODO: Has to do with calling convention?
static const usz max_register_size = 8;

#ifdef X86_64_GENERATE_MACHINE_CODE

#include <codegen/generic_object.h>

#endif // X86_64_GENERATE_MACHINE_CODE

/// Creates a context for the CG_FMT_x86_64_MSWIN architecture.
CodegenContext *codegen_context_x86_64_mswin_create() {
  caller_saved_register_count = MSWIN_CALLER_SAVED_REGISTER_COUNT;
  caller_saved_registers = mswin_caller_saved_registers;
  argument_register_count = MSWIN_ARGUMENT_REGISTER_COUNT;
  argument_registers = mswin_argument_registers;

  CodegenContext *cg_ctx = calloc(1,sizeof(CodegenContext));
  cg_ctx->ffi.cchar_size = 8;
  cg_ctx->ffi.cshort_size = 16;
  cg_ctx->ffi.cint_size = 32;
  cg_ctx->ffi.clong_size = 32;
  cg_ctx->ffi.cllong_size = 64;
  cg_ctx->ffi.pointer_size = 64;
  cg_ctx->ffi.integer_size = 64;
  return cg_ctx;
}

/// Creates a context for the x86_64/CG_CALL_CONV_SYSV.
CodegenContext *codegen_context_x86_64_linux_create() {
  caller_saved_register_count = LINUX_CALLER_SAVED_REGISTER_COUNT;
  caller_saved_registers = linux_caller_saved_registers;
  argument_register_count = LINUX_ARGUMENT_REGISTER_COUNT;
  argument_registers = linux_argument_registers;

  CodegenContext *cg_ctx = calloc(1,sizeof(CodegenContext));
  cg_ctx->ffi.cchar_size = 8;
  cg_ctx->ffi.cshort_size = 16;
  cg_ctx->ffi.cint_size = 32;
  cg_ctx->ffi.clong_size = 64;
  cg_ctx->ffi.cllong_size = 64;
  cg_ctx->ffi.pointer_size = 64;
  cg_ctx->ffi.integer_size = 64;
  return cg_ctx;
}

/// Free a context created by codegen_context_x86_64_mswin_create.
void codegen_context_x86_64_mswin_free(CodegenContext *ctx) {
  (void)ctx;
}

void codegen_context_x86_64_linux_free(CodegenContext *ctx) {
  (void)ctx;
}

typedef enum Clobbers {
  CLOBBERS_NEITHER,
  CLOBBERS_REFERENCE,
  CLOBBERS_LEFT,
  CLOBBERS_RIGHT,
  CLOBBERS_BOTH,
  CLOBBERS_OTHER,
} Clobbers;

Clobbers does_clobber(IRInstruction *instruction) {
  STATIC_ASSERT(IR_COUNT == 40, "Exhaustive handling of IR instruction types that correspond to two-address instructions in x86_64.");
  switch (ir_kind(instruction)) {
  case IR_ADD:
  case IR_DIV:
  case IR_MUL:
  case IR_MOD:
  case IR_AND:
  case IR_OR:
    return CLOBBERS_RIGHT;

  case IR_SUB:
  case IR_SHL:
  case IR_SHR:
  case IR_SAR:
    return CLOBBERS_LEFT;

  case IR_NOT:
    return CLOBBERS_REFERENCE;

  default:
    break;
  }
  return CLOBBERS_NEITHER;
}

static usz emit_memcpy_impl(
  CodegenContext *context,
  Type *element_type,
  IRInstruction **to,
  IRInstruction **from,
  usz byte_size,
  IRInstruction *before
) {
  /// Increment that we’ll be adding to the pointers.
  usz iter_amount = type_sizeof(element_type);
  IRInstruction *increment = ir_create_immediate(context, t_integer, iter_amount);
  ir_insert_before(before, increment);

  /// Unroll the memcpy() loop.
  for (; iter_amount <= byte_size ; byte_size -= iter_amount) {
    /// Load and store value.
    IRInstruction *load = ir_insert_before(before, ir_create_load(context, element_type, *from));
    ir_insert_before(before, ir_create_store(context, load, *to));

    /// Increment pointers if we have more to copy.
    if (byte_size - iter_amount) {
      *from = ir_create_add(context, *from, increment);
      *to = ir_create_add(context, *to, increment);
      ir_insert_before(before, *from);
      ir_insert_before(before, *to);
    }
  }

  return byte_size;
}

static void emit_memcpy(
  CodegenContext *context,
  IRInstruction *to,
  IRInstruction *from,
  usz bytes_to_copy,
  IRInstruction *insert_before_this
) {
  /// Copy in integer-sized blocks.
  bytes_to_copy = emit_memcpy_impl(
    context,
    t_integer,
    &to,
    &from,
    bytes_to_copy,
    insert_before_this
  );

  /// Copy in byte-sized blocks if there’s more to copy.
  if (bytes_to_copy) emit_memcpy_impl(
    context,
    t_byte,
    &to,
    &from,
    bytes_to_copy,
    insert_before_this
  );
}

typedef enum SysVArgumentClass {
  SYSV_REGCLASS_INVALID,
  SYSV_REGCLASS_INTEGER,
  SYSV_REGCLASS_SSE,
  SYSV_REGCLASS_SSEUP,
  SYSV_REGCLASS_x87,
  SYSV_REGCLASS_x87UP,
  SYSV_REGCLASS_COMPLEX_x87,
  SYSV_REGCLASS_NO_CLASS,
  SYSV_REGCLASS_MEMORY,
} SysVArgumentClass;

SysVArgumentClass sysv_classify_argument(Type *given_type) {
  Type *type = type_canonical(given_type);
  // TODO: Use type_is_integer instead of t_integer comparisons, etc.
  if (type_is_pointer(type) ||
      type_is_reference(type) ||
      (type->kind == TYPE_INTEGER && type->integer.bit_width <= 64) ||
      type == t_integer ||
      type == t_byte) {
    return SYSV_REGCLASS_INTEGER;
  }
  usz size = type_sizeof(type);
  // Anything 8 bytes or less can go in a register.
  if (size <= 8) return SYSV_REGCLASS_INTEGER;
  // Anything larger than "two eight-bytes" (16 bytes) goes in memory.
  // "If the size of an object is larger than four eightbytes, or it
  // contains unaligned fields, it has class MEMORY."
  // Technically, the only things that get lowered into multiple
  // registers are two eightbytes or smaller; so while the "rule"
  // above *does* say four eightbytes, it actually is only two.
  // TODO: Check for "unaligned fields".
  if (size > 16) return SYSV_REGCLASS_MEMORY;
  // At this point we have a 9-16 byte type.
  // "If the size of the aggregate exceeds a single eightbyte,
  // each is classified separately. Each eightbyte gets
  // initialized to class NO_CLASS."
  // Things that are 9-16 bytes are where SYSV tries to split things
  // across registers, basically.
  if (type_is_struct(type)) return SYSV_REGCLASS_INTEGER;
  if (type_is_array(type)) {
    // Classify base type of array.
    SysVArgumentClass base_class = sysv_classify_argument(type->array.of);
    if (type->array.size == 1) return base_class;
    usz base_size = type_sizeof(type->array.of);
    // If an aggregate exceeds two eightbytes, the whole argument is passed in memory.
    if (type->array.size * base_size > 16) return SYSV_REGCLASS_MEMORY;
    // Otherwise, the aggregate is less than or equal to two
    // eightbytes, and can be passed in one or two registers.
    return SYSV_REGCLASS_INTEGER;
  }
  return SYSV_REGCLASS_INVALID;
}

/// @return How many registers an argument takes up.
usz sysv_argument_register_count_x86_64(CodegenContext *context, Type *function, usz parameter_index) {
  ASSERT(context->call_convention == CG_CALL_CONV_SYSV, "Don't call sysv_* things unless you are using the SYSV ABI!!");
  ASSERT(function->kind == TYPE_FUNCTION);

  if (parameter_index >= function->function.parameters.size)
    ICE("SysV: Parameter index out of bounds");

  Parameter *parameter = function->function.parameters.data + parameter_index;

  SysVArgumentClass class = SYSV_REGCLASS_INVALID;
  class = sysv_classify_argument(parameter->type);
  ASSERT(class != SYSV_REGCLASS_INVALID, "Could not classify argument according to SYSV ABI, sorry");

  if (class == SYSV_REGCLASS_INTEGER) {
    if (type_sizeof(parameter->type) > 8) return 2;
    return 1;
  }
  return 0;
}

usz sysv_argument_register_index_x86_64(CodegenContext *context, Type *function, usz parameter_index) {
  ASSERT(context->call_convention == CG_CALL_CONV_SYSV, "Don't call sysv_* things unless you are using the SYSV ABI!!");
  ASSERT(function->kind == TYPE_FUNCTION);

  if (parameter_index >= function->function.parameters.size)
    ICE("Parameter index out of bounds");

  usz argument_register_offset = 0;
  for (usz i = 0; i < parameter_index; ++i)
    argument_register_offset += sysv_argument_register_count_x86_64(context, function, i);

  return argument_register_offset;
}

/// Insert instructions to load a parameter split across two registers
/// in place of given `parameter` instruction.
void sysv_load_two_register_parameter(CodegenContext *context, IRInstruction *parameter) {
  Type *param_type = ir_typeof(parameter);
  usz size = type_sizeof(param_type);
  ASSERT(size > 8, "%T is less than or equal to eight bytes, and should be passed in a single register, not two.", param_type);
  ASSERT(size <= 16, "Can only pass things that are two-eightbytes or less in general purpose registers.");

  Type *func_type = ir_typeof(ir_parent(ir_parent(parameter)));
  usz ri = sysv_argument_register_index_x86_64(context, func_type, ir_imm(parameter));
  ASSERT(ri + 1 < argument_register_count);

  /// Get value in registers.
  IRInstruction *eightbyte1 = ir_create_register(context, t_integer, argument_registers[ri]);
  IRInstruction *eightbyte2 = ir_create_register(context, t_integer, argument_registers[ri + 1]);
  ir_insert_before(parameter, eightbyte1);
  ir_insert_before(parameter, eightbyte2);

  /// Replace the parameter value.
  for (usz i = 0; i < ir_use_count(parameter);) {
    IRInstruction *user = ir_user_get(parameter, 0);

    /// Stores of the parameter value are inlined.
    if (ir_kind(user) == IR_STORE && ir_store_value(user) == parameter) {
      IRInstruction *addr1 = ir_store_addr(user);
      IRInstruction *offset = ir_insert_after(addr1, ir_create_immediate(context, t_integer, 8));
      IRInstruction *addr2 = ir_insert_after(offset, ir_create_add(context, addr1, offset));
      ir_insert_after(addr2, ir_create_store(context, eightbyte2, addr2));
      ir_insert_after(addr2, ir_create_store(context, eightbyte1, addr1));

      /// Remove the store.
      ir_remove(user);
      continue;
    }

    /// Increment only if we need to skip the use.
    i++;
  }

  /// If the parameter has still uses, we’ve encountered something
  /// that we don’t know how to handle, so create a temporary copy
  /// on the stack and replace the parameter with a load.
  if (ir_use_count(parameter)) {
    IRInstruction *addr1 = ir_insert_before(parameter, ir_create_alloca_sized(context, param_type, 16));
    IRInstruction *offset = ir_insert_before(parameter, ir_create_immediate(context, t_integer, 8));
    IRInstruction *addr2 = ir_insert_before(parameter, ir_create_add(context, addr1, offset));
    ir_insert_before(parameter, ir_create_store(context, eightbyte1, addr1));
    ir_insert_before(parameter, ir_create_store(context, eightbyte2, addr2));
    ir_replace(parameter, ir_create_load(context, param_type, addr1));
  } else {
    ir_remove(parameter);
  }
}

static void lower_memory_parameter(
  CodegenContext *ctx,
  IRInstruction *param,
  Type *addr_type,
  usz offs_val
) {
  Type *type = ir_typeof(param);
  IRInstruction *rbp = ir_insert_before(param, ir_create_register(ctx, t_integer, REG_RBP));
  IRInstruction *offset = ir_insert_before(param, ir_create_immediate(ctx, t_integer, offs_val));
  IRInstruction *addr = ir_insert_before(param, ir_create_add(ctx, rbp, offset));
  ir_set_type(addr, addr_type);

  /// Replace the parameter value.
  for (usz i = 0; i < ir_use_count(param); ++i) {
    IRInstruction *user = ir_user_get(param, 0);

    /// Stores of the parameter value are converted to memcpy()s.
    if (ir_kind(user) == IR_STORE && ir_store_value(user) == param) {
      IRInstruction *cpy = ir_create_memcpy(
        ctx,
        ir_store_addr(user),
        addr,
        ir_insert_before(user, ir_create_immediate(ctx, t_integer, type_sizeof(type)))
      );

      ir_replace(user, cpy);
    }

    else {
      ir_replace(user, ir_create_load(ctx, type, addr));
    }
  }

  ir_remove(param);
}

static void lower_parameter(CodegenContext *context, IRInstruction *inst) {
  switch (context->call_convention) {
    case CG_CALL_CONV_SYSV: {
      IRFunction *parent_func = ir_parent(ir_parent(inst));
      if (parameter_is_in_register_x86_64(context, parent_func, ir_imm(inst))) {
        // Classify argument into register class.
        // NOTE: This has probably already been done, and we could
        // cache it and use that computed value, if we have somewhere
        // to store it.
        Type *type = ir_typeof(inst);
        SysVArgumentClass class = SYSV_REGCLASS_INVALID;
        class = sysv_classify_argument(type);

        ASSERT(class != SYSV_REGCLASS_INVALID, "Could not classify argument according to SYSV ABI, sorry");
        switch (class) {
          case SYSV_REGCLASS_INTEGER: {
            if (type_sizeof(type) > 8) sysv_load_two_register_parameter(context, inst);
            else ir_replace(inst, ir_create_register(context, type, argument_registers[ir_imm(inst)]));
          } break;

          case SYSV_REGCLASS_MEMORY: {
            // FIXME: Tail calls, leaf functions, etc. may alter the size of the stack frame here.
            // Skip pushed RBP and return addess.
            usz parameter_index = ir_imm(inst);
            usz offs_val = 16;
            Type *ftype = ir_typeof(parent_func);
            usz i = ftype->function.parameters.size - 1;
            foreach_rev(param, ftype->function.parameters) {
              if (i <= parameter_index) break;
              offs_val += type_sizeof(param->type);
              --i;
            }

            lower_memory_parameter(
              context,
              inst,
              ast_make_type_pointer(context->ast, type->source_location, type),
              offs_val
            );
          } break;
          default:
            TODO("Handle lowering of SYSV Register Classification: %d\n", class);
        }
      }
    } break;

    case CG_CALL_CONV_MSWIN: {
      Type *type = type_canonical(ir_typeof(inst));
      // NOTE: Arrays and strings in Intercept are passed like
      // structs in C, so this doesn't apply to *Intercept*
      // arrays/strings.
      // __m128 types, arrays, and strings are never passed by immediate value.
      // Structs and unions of size 8, 16, 32, or 64 bits, and __m64
      // types, are passed as if they were integers of the same size.
      usz idx = ir_imm(inst);
      if (idx < argument_register_count) {
        ir_replace(inst, ir_create_register(context, type, argument_registers[idx]));
      } else {
        // Calculate offset to caller-allocated stack memory for large parameters.
        // FIXME: Tail calls, leaf functions, etc. may alter the size of the stack frame here.
        // FIXME: Siraide and I have discussed; it seems like trying
        // to get tail calls/minimal stack frames to work with stack-
        // based parameter passing is just asking too much. We should
        // disallow tail calls/emitting minimal stack frames when a
        // function has stack based parameters.
        // Skip pushed RBP and return address.
        Type *func_type = ir_typeof(ir_parent(ir_parent(inst)));
        usz offs_val = 32 + 16; // 32 = shadow stack, 16 = stack frame
        usz i = func_type->function.parameters.size - 1;
        foreach_rev(param, func_type->function.parameters) {
          if (i <= idx) break;
          offs_val += 8;
          --i;
        }

        // Lower type to a pointer, because that's how the calls have
        // been altered as well.
        Type *ptr = type_sizeof(type) > 8
                    ? ast_make_type_pointer(context->ast, type->source_location, type)
                    : type;

        lower_memory_parameter(
          context,
          inst,
          ptr,
          offs_val
        );
      }
    } break;

    default: ICE("Unhandled call convention for parameter lowering.");
  }
}

/// Forward decl because mutual recursion.
static void lower_instruction(CodegenContext *context, IRInstruction *inst);

/// Lower a store instruction.
static void lower_store(CodegenContext *ctx, IRInstruction *store) {
  /// Ignore stores supported by the hardware.
  IRInstruction *value = ir_store_value(store);
  Type *value_type = ir_typeof(value);
  if (type_sizeof(value_type) <= max_register_size) return;

  /// Handle stores whose values are loads.
  if (ir_kind(value) == IR_LOAD) {
    /// Convert to memory copy.
    IRInstruction *value_addr = ir_operand(value);
    IRInstruction *cpy = ir_create_memcpy(
      ctx,
      ir_store_addr(store),
      value_addr,
      ir_insert_before(store, ir_create_immediate(ctx, t_integer, type_sizeof(value_type)))
    );

    ir_replace(store, cpy);
    lower_instruction(ctx, cpy);
    return;
  }

  /// Don’t know how to handle anything else.
  ICE("Unsupported store of non-register-size value");
}

static IRInstruction *alloca_copy_of(CodegenContext *context, IRInstruction *copy, IRInstruction *insert_before_this) {
  IRInstruction *alloca = ir_create_alloca(context, ir_typeof(copy));
  IRInstruction *store = ir_create_store(context, copy, alloca);
  ir_insert_before(insert_before_this, alloca);
  ir_insert_before(insert_before_this, store);
  lower_store(context, store);
  return alloca;
}

static void lower_instruction(CodegenContext *context, IRInstruction *inst) {
  switch (ir_kind(inst)) {
    default: UNREACHABLE();

    case IR_RETURN: {
      STATIC_ASSERT(CG_CALL_CONV_COUNT == 2, "Exhaustive handling of calling convention return register during x86_64 lowering");
      if (ir_operand(inst)) ASSERT(
        type_sizeof(ir_typeof(ir_operand(inst))),
        "Sorry, can only return integer-sized objects from functions at the moment"
      );

      // TODO: Use desc.result_register instead of REG_RAX.
      switch (context->call_convention) {
        case CG_CALL_CONV_MSWIN: {
          // Cast returned value iff not equal. Probably not needed in
          // all cases, but definitely needed in some (i.e. returning
          // byte-typed variable from program).
          IRInstruction *op = ir_operand(inst);
          if (op && !type_equals(ir_typeof(inst), ir_typeof(op))) {
            // Just like NODE_CAST handling in codegen.c
            Type *t_to = ir_typeof(ir_parent(ir_parent(inst)))->function.return_type;
            Type *t_from = ir_typeof(op);

            usz to_sz = type_sizeof(t_to);
            usz from_sz = type_sizeof(t_from);

            bool from_signed = type_is_signed(t_from);

            if (from_sz == to_sz) {
              /// FIXME: This has been commented out for a while, do we need this?
              // INSTRUCTION(bitcast, IR_BITCAST);
              // bitcast->operand = instruction->operand;
              // bitcast->type = t_to;
              // mark_used(instruction->operand, bitcast);
              // insert_instruction_before(bitcast, instruction);
              // ir_remove_use(instruction->operand, instruction);
              // instruction->operand = bitcast;
            } else if (from_sz < to_sz) {
              // smaller to larger: sign extend if needed, otherwise zero extend.
              if (from_signed) {
                IRInstruction *sext = ir_insert_before(inst, ir_create_sext(context, t_to, op));
                ir_operand(inst, sext);
              } else {
                IRInstruction *zext = ir_insert_before(inst, ir_create_zext(context, t_to, op));
                ir_operand(inst, zext);
              }
            } else if (from_sz > to_sz) {
              IRInstruction *trunc = ir_insert_before(inst, ir_create_trunc(context, t_to, op));
              ir_operand(inst, trunc);
            }
          }

          /// Return value goes in rax.
          ir_register(inst, REG_RAX);

          /// FIXME: I don’t think the return itself needs a type? The
          /// type of the operand should be enough.
          ir_set_type(inst, t_integer);
        } break;
        case CG_CALL_CONV_SYSV: {
          ir_register(inst, REG_RAX);
          ir_set_type(inst, ir_operand(inst) ? ir_typeof(ir_operand(inst)) : t_integer);
        } break;
        default: UNREACHABLE();
      }
    } break;

    case IR_STORE: lower_store(context, inst); break;

    /// Handle intrinsics that require early lowering.
    STATIC_ASSERT(INTRIN_BACKEND_COUNT == 3, "Handle backend intrinsics in codegen");
    case IR_INTRINSIC: {
      switch (ir_intrinsic_kind(inst)) {
        IGNORE_FRONTEND_INTRINSICS()

        /// These are very low level and will be handled later on.
        case INTRIN_BUILTIN_DEBUGTRAP:
        case INTRIN_BUILTIN_SYSCALL:
          break;

        /// Lower memory copies.
        case INTRIN_BUILTIN_MEMCPY: {
          /// If the size is known at compile time, we can inline it.
          /// TODO: Should call libc `memcpy()` or our runtime’s
          /// `__intercept_memcpy()` once we have something like that
          /// if the size is too large.
          IRInstruction *size = ir_call_arg(inst, 2);
          if (ir_kind(size) == IR_IMMEDIATE) {
            emit_memcpy(
              context,
              ir_call_arg(inst, 0),
              ir_call_arg(inst, 1),
              ir_imm(size),
              inst
            );

            ir_remove(inst);
            break;
          }

          /// Size must be known at compile time.
          ICE("Sorry, Non-constant-sized memory copies not supported");
        } break;
      }
    } break;

    case IR_CALL: {
      size_t argcount = ir_call_args_count(inst);

      switch (context->call_convention) {
        default: ICE("Unhandled calling convention in x86_64 lowering of calls");
        case CG_CALL_CONV_SYSV: {
          Type *ftype = ir_call_callee_type(inst);
          ASSERT(
            ftype->kind == TYPE_FUNCTION,
            "Expected callee of IR_CALL to be a function, but got %T\n",
            ftype
          );

          /// Determine Sys V argument classes.
          usz regs_used = 0;
          Vector(usz) two_register_args = {0};
          foreach_index (i, ftype->function.parameters) {
            Parameter *parameter = ftype->function.parameters.data + i;
            SysVArgumentClass class = sysv_classify_argument(parameter->type);
            if (class == SYSV_REGCLASS_INTEGER) {
              if (regs_used + 2 <= argument_register_count && type_sizeof(parameter->type) > 8) {
                vector_push(two_register_args, i);
                regs_used += 2;
              } else if (regs_used < argument_register_count) {
                ASSERT(regs_used < argument_register_count, "Invalid argument register index");
                IRInstruction *copy = ir_create_copy(context, ir_call_arg(inst, i));
                ir_register(copy, argument_registers[regs_used++]);
                ir_insert_before(inst, copy);
                ir_call_arg(inst, i, copy);
              } else {
                TODO("SysV: All argument registers are used, we have to start spilling to stack.");
              }
            } else if (class == SYSV_REGCLASS_MEMORY) {
              TODO("SysV: Handle memory class parameter");
            } else ICE("SysV: Unhandled register class of parameter %d", (int) class);
          }

          foreach_rev(i, two_register_args) {
            Type *t_integer_ptr = ast_make_type_pointer(context->ast, t_integer->source_location, t_integer);
            IRInstruction *argument = ir_call_arg(inst, *i);

            usz ri = sysv_argument_register_index_x86_64(context, ftype, *i);
            ASSERT(ri + 1 < argument_register_count);

            // FIXME: Second eightbyte shouldn't have to be fully eight bytes.
            ASSERT(
              type_sizeof(ir_typeof(argument)) == 16,
              "SysV ABI requires alignment of a multiple of 16 for aggregate types from (8 to 16]: %T",
              ir_typeof(argument)
            );

            IRInstruction *load1, *load2;

            /// If the argument is a load instruction, instead convert
            /// it to two 8-byte loads instead.
            if (ir_kind(argument) == IR_LOAD) {
              IRInstruction *addr1 = ir_operand(argument);
              IRInstruction *offset = ir_insert_before(inst, ir_create_immediate(context, t_integer, 8));
              IRInstruction *addr2 = ir_insert_before(inst, ir_create_add(context, addr1, offset));
              load1 = ir_insert_before(inst, ir_create_load(context, t_integer, addr1));
              load2 = ir_insert_before(inst, ir_create_load(context, t_integer, addr2));
            }

            /// Otherwise, create a copy and load from that.
            else {
              /// FIXME: Is this case even possible?
              // Load first eightbyte of the parameter.
              IRInstruction *addr1 = ir_insert_before(inst, ir_create_copy(context, argument));
              ir_set_type(addr1, t_integer_ptr); /// FIXME: Is this necessary?
              load1 = ir_insert_before(inst, ir_create_load(context, t_integer, addr1));

              IRInstruction *offset = ir_insert_before(inst, ir_create_immediate(context, t_integer, 8));

              // Load second eightbyte of the parameter.
              IRInstruction *addr2 = ir_insert_before(inst, ir_create_add(context, addr1, offset));
              ir_set_type(addr2, t_integer_ptr); /// FIXME: Is this necessary?
              load2 = ir_insert_before(inst, ir_create_load(context, t_integer, addr2));
            }

            ir_register(load1, argument_registers[ri]);
            ir_register(load2, argument_registers[ri + 1]);

            // Remove argument from call, and replace with two new arguments.
            ir_call_replace_arg(inst, *i, load1);
            ir_call_insert_arg(inst, *i + 1, load2);

            /// If the old argument is now unused, delete it.
            if (ir_use_count(argument) == 1) {
              ASSERT(ir_user_get(argument, 0) == inst);
            }
          }
        } break;

        case CG_CALL_CONV_MSWIN: {
          // TODO: desc.result_register, not REG_RAX
          Type *ty = ir_typeof(inst);
          if (!type_is_void(ty)) {
            ir_register(inst, REG_RAX);
            if (type_sizeof(ty) > max_register_size) {
              Type *large_type = ty;

              // Lower to pointer return type
              ty = ast_make_type_pointer(context->ast, large_type->source_location, large_type);
              ir_set_type(inst, ty);

              // Insert extra argument (pointer to local allocation of return type)
              IRInstruction *alloca = ir_insert_before(inst, ir_create_alloca(context, ty));
              ir_call_insert_arg(inst, 0, alloca);

              // Replace uses of the call result with a load from the return value on the stack.
              IRInstruction *load = ir_insert_after(inst, ir_create_load(context, large_type, alloca));
              ir_replace_uses(inst, load);
            } else {
              /// TODO: Can’t we just... set the call’s result register and be done w/ it?
              // This is a bit scuffed. Firstly, *reasonably*, we set the call's
              // result to the result register. But then in order to get
              // future uses of the call's "virtual register" to properly be
              // the virtual register that contains the result, we need to copy
              // the result register into a virtual register and then replace
              // all uses of the call result with that virtual register... As I
              // said, a bit scuffed.
              IRInstruction *copy = ir_insert_after(inst, ir_create_copy(context, inst));
              ir_replace_uses(inst, copy);
            }
          }

          usz idx = 0;
          argcount = ir_call_args_count(inst);

          // Lower aggregates in possible-register arguments by allocating a copy of them on the stack.
          for (; idx < argument_register_count && idx < argcount; idx++) {
            IRInstruction *arg = ir_call_arg(inst, idx);
            Type *type = type_canonical(ir_typeof(arg));
            if (type_sizeof(type) > 8) {
              ir_call_arg(inst, idx, alloca_copy_of(context, arg, inst));
            } else {
              IRInstruction *copy = ir_insert_before(inst, ir_create_copy(context, arg));
              ir_register(copy, argument_registers[idx]);
              ir_call_arg(inst, idx, copy);
            }
          }

          // Lower all arguments not able to go in a register by allocating a copy of them on the stack.
          if (argcount >= argument_register_count) {
            for (usz i = argcount - 1; i >= argument_register_count; i--) {
              IRInstruction *arg = ir_call_arg(inst, i);
              if (type_sizeof(ir_typeof(arg)) > 8)
                ir_call_arg(inst, i, alloca_copy_of(context, arg, inst));
            }
          }
        } break;
      } // switch (calling convention)
    } break;

    case IR_BITCAST: {
      IRInstruction *copy = ir_create_copy(context, ir_operand(inst));
      ir_set_type(copy, ir_typeof(inst));
      ir_replace(inst, copy);
    } break;
  }
}

static void lower(CodegenContext *context) {
  ASSERT(argument_registers, "arch_x86_64 backend can not lower IR when argument registers have not been initialized.");
  Vector(IRInstruction *) worklist = {0};

  /// Lower parameter references first as that can create more loads
  /// and stores that may need to be lowered. Skip extern functions.
  foreach_val (func, context->functions) {
    if (!ir_func_is_definition(func)) continue;
    Type *ty = ir_typeof(func);
    for (usz i = 0; i < ty->function.parameters.size; i++) {
      IRInstruction *param = ir_parameter(func, i);
      if (ir_parent(param) == NULL) continue;
      if (ir_use_count(param) == 0) ir_remove(param);
      lower_parameter(context, param);
    }
  }

  /// Collect all instructions for which lowering entails inserting other
  /// instructions; we’ll have to lower those in reverse order to avoid
  /// iterator invalidation bugs.
  ///
  /// Any instructions whose lowering doesn’t cause iterator invalidation
  /// are lowered right away.
  FOREACH_INSTRUCTION_IN_CONTEXT(inst, b, f, context) {
    switch (ir_kind(inst)) {
    default: break;
    case IR_ALLOCA:
      /// Worry about stack alignment
      if (ir_alloca_size(inst) < 8) ir_alloca_size(inst, 8);
      break;

    /// Replacing a single instruction with a new instruction is fine.
    case IR_BITCAST: {
      IRInstruction *copy = ir_create_copy(context, ir_operand(inst));
      ir_set_type(copy, ir_typeof(inst));
      ir_replace(inst, copy);
    } break;

    case IR_RETURN:
    case IR_CALL:
    case IR_INTRINSIC:
    case IR_STORE:
      vector_push(worklist, inst);
      break;
    }
  }

  /// Lower all instructions that require inserting other instructions.
  foreach_rev(inst, worklist) lower_instruction(context, *inst);

  /// Finally, clean up loads that are no longer referenced.
  /// Non-register-size loads are only allowed as the operands
  /// of certain instructions; those instructions have already
  /// retrieved the values they need in the lowering passes
  /// above, which means we should now remove any unused loads
  /// so the backend doesn’t complain about them.
  vector_clear(worklist);
  FOREACH_INSTRUCTION_IN_CONTEXT(inst, b, f, context)
    if (ir_kind(inst) == IR_LOAD && ir_use_count(inst) == 0)
      vector_push(worklist, inst);
  foreach_rev(inst, worklist) ir_remove(*inst);
  vector_delete(worklist);
}

static size_t interfering_regs(IRInstruction *instruction) {
  ASSERT(instruction, "Can not get register interference of NULL instruction.");
  size_t mask = 0;

  // FIXME: It'd be really great if we /didn't/ have to loop over every
  // single user of every single instruction here, but I don't see another
  // way of doing this, really.
  // Divisor of div/mod are not allowed to go in RAX/RDX; that's where
  // the dividend must go.
  FOREACH_USER(user, instruction) {
    if ((ir_kind(user) == IR_DIV || ir_kind(user) == IR_MOD) && ir_rhs(user) == instruction) {
      mask |= ((usz)1 << REG_RAX);
      mask |= ((usz)1 << REG_RDX);
    }
  }

  switch(ir_kind(instruction)) {
  case IR_SHL:
  case IR_SHR:
  case IR_SAR:
    mask |= ((usz)1 << REG_RCX);
    break;
  case IR_DIV:
  case IR_MOD:
    mask |= ((usz)1 << REG_RAX);
    mask |= ((usz)1 << REG_RDX);
    break;
  case IR_CALL: // FIXME: This seems specific to calling convention...
    mask |= ((usz)1 << REG_RAX);
  default:
    break;
  }
  // Shift mask right because it doesn't include REG_NONE
  return mask >> 1;
}

void codegen_lower_x86_64(CodegenContext *context) { lower(context); }

void codegen_lower_early_x86_64(CodegenContext *context) {
  IRInstructionVector to_lower = {0};
  FOREACH_INSTRUCTION_IN_CONTEXT(instruction, b, f, context) {
    switch (ir_kind(instruction)) {
      default: break;

      /// Only certain stores can be lowered at all.
      case IR_STORE: {
        IRInstruction *val = ir_store_value(instruction);
        if (type_sizeof(ir_typeof(val)) <= max_register_size || ir_kind(val) == IR_LOAD)
          vector_push(to_lower, instruction);
      } break;
    }
  }

  foreach_rev(instruction, to_lower) lower_instruction(context, *instruction);
  vector_delete(to_lower);
}

bool parameter_is_in_register_x86_64(CodegenContext *context, IRFunction *function, usz parameter_index) {
  ASSERT(context->arch == ARCH_X86_64);

  Type *ftype = ir_typeof(function);
  if (parameter_index >= ftype->function.parameters.size)
    ICE("Parameter index out of bounds");

  Type *param_type = ftype->function.parameters.data[parameter_index].type;

  switch (context->call_convention) {

  case CG_CALL_CONV_MSWIN: {
    if (parameter_index >= 4) return false;
    if (type_sizeof(param_type) > 8) return false;
  } return true;

  case CG_CALL_CONV_SYSV: {
    SysVArgumentClass class = sysv_classify_argument(param_type);
    ASSERT(class != SYSV_REGCLASS_INVALID, "Could not classify argument according to SYSV ABI, sorry");
    if (class == SYSV_REGCLASS_INTEGER) return true;
    if (class == SYSV_REGCLASS_MEMORY) return false;
    TODO("Handle SYSV Register Classification: %d\n", class);
  }

  default:
    ICE("Unhandled calling convention: %d\n", context->call_convention);
  }
}

static void mir_x86_64_function_exit_at(enum StackFrameKind frame_kind, MIRBlock *block, usz *index) {
  STATIC_ASSERT(FRAME_COUNT == 3, "Exhaustive handling of stack frame kinds in function entry MIR lowering");
  ASSERT(frame_kind < FRAME_COUNT, "Invalid stack frame kind!");
  switch (frame_kind) {
  case FRAME_NONE: break;

  case FRAME_FULL: {
    // MOV %RBP, %RSP
    MIRInstruction *restore_sp = mir_makenew(MX64_MOV);
    mir_add_op(restore_sp, mir_op_register(REG_RBP, r64, false));
    mir_add_op(restore_sp, mir_op_register(REG_RSP, r64, false));
    mir_insert_instruction(block, restore_sp, ++*index);
  } FALLTHROUGH;
  case FRAME_MINIMAL: {
    // POP %RBP
    MIRInstruction *restore_bp = mir_makenew(MX64_POP);
    mir_add_op(restore_bp, mir_op_register(REG_RBP, r64, false));
    mir_insert_instruction(block, restore_bp, ++*index);
  } break;

  case FRAME_COUNT: FALLTHROUGH;
  default: UNREACHABLE();

  }
}

void codegen_emit_x86_64(CodegenContext *context) {
  const MachineDescription desc = {
    .registers = general,
    .register_count = GENERAL_REGISTER_COUNT,
    .argument_registers = argument_registers,
    .argument_register_count = argument_register_count,
    .result_register = REG_RAX,
    .instruction_register_interference = interfering_regs
  };

#ifdef X86_64_GENERATE_MACHINE_CODE
  GenericObjectFile object = {0};
  context->object = &object;
  {
    Section sec_code = {0};
    sec_code.name = ".text";
    sec_code.attributes |= SEC_ATTR_EXECUTABLE;
    vector_push(object.sections, sec_code);
    Section sec_rodata = {0};
    sec_rodata.name = ".rodata";
    vector_push(object.sections, sec_rodata);
    Section sec_data = {0};
    sec_data.name = ".data";
    sec_data.attributes |= SEC_ATTR_WRITABLE;
    vector_push(object.sections, sec_data);
    Section sec_bss = {0};
    sec_bss.name = ".bss";
    sec_bss.attributes |= SEC_ATTR_SPAN_FILL | SEC_ATTR_WRITABLE;
    vector_push(object.sections, sec_bss);
  }
  Section *sec_initdata = get_section_by_name(object.sections, ".data");
  Section *sec_uninitdata = get_section_by_name(object.sections, ".bss");
  Section *sec_rodata = get_section_by_name(object.sections, ".rodata");
#endif // x86_64_GENERATE_MACHINE_CODE

  /// Emit static variables.
  /// TODO: interning.
  bool have_data_section = false;
  foreach_val (var, context->static_vars) {
    /// Do not emit unused variables.
    if (optimise) {
      bool used = false;
      foreach_val (ref, var->references) {
        if (ir_use_count(ref)) {
          used = true;
          break;
        }
      }
      if (!used) continue;
    }

    /// Emit a data section directive if we haven't already.
    if (!have_data_section) {
      have_data_section = true;
      if (context->target == TARGET_GNU_ASM_ATT || context->target == TARGET_GNU_ASM_INTEL)
        fprint(context->code, ".section .data\n");
    }

    const SymbolLinkage linkage = var->decl->declaration.linkage;
    const bool exported = linkage == LINKAGE_EXPORTED || linkage == LINKAGE_REEXPORTED;
    const bool imported = linkage == LINKAGE_IMPORTED || linkage == LINKAGE_REEXPORTED;
    const GObjSymbolType sym_type = exported
                              ? GOBJ_SYMTYPE_EXPORT
                              : GOBJ_SYMTYPE_STATIC;

    // Do compile-time known static assignment.

    if (var->init) {
      if (ir_kind(var->init) == IR_LIT_INTEGER) {
        ASSERT(
          !imported,
          "Imported variables cannot have static initialisers"
        );

        usz imm = ir_imm(var->init);
        uint8_t *byte_repr = (uint8_t *)&imm;
        STATIC_ASSERT(TARGET_COUNT == 6, "Exhaustive handling of assembly targets");
        if (context->target == TARGET_GNU_ASM_ATT || context->target == TARGET_GNU_ASM_INTEL) {
          // TODO: Endianness selection
          // `%u` and the `(unsigned)` cast is because variadic arguments
          // of integral types are always promoted to at least `int` or
          // `unsigned` in C.
          if (linkage == LINKAGE_EXPORTED)
            fprint(context->code, ".global %S\n", var->name);
          fprint(context->code, "%S: .byte %u", var->name, (unsigned) byte_repr[0]);
          for (usz i = 1; i < type_sizeof(var->type); ++i)
            fprint(context->code, ",%u", (unsigned) byte_repr[i]);

          fprint(context->code, "\n");
        }

#ifdef X86_64_GENERATE_MACHINE_CODE
        STATIC_ASSERT(TARGET_COUNT == 6, "Exhaustive handling of object targets");
        if (context->target == TARGET_COFF_OBJECT || context->target == TARGET_ELF_OBJECT) {
          // Create symbol for var->name at current offset within the .data section
          GObjSymbol sym = {0};
          sym.type = sym_type;
          sym.name = strdup(var->name.data);
          sym.section_name = strdup(".data");
          sym.byte_offset = sec_initdata->data.bytes.size;
          vector_push(object.symbols, sym);
          // Write initialised bytes to .data section
          sec_write_n(sec_initdata, byte_repr, type_sizeof(var->type));
        }
#endif // x86_64_GENERATE_MACHINE_CODE

      } else if (ir_kind(var->init) == IR_LIT_STRING) {
        STATIC_ASSERT(TARGET_COUNT == 6, "Exhaustive handling of assembly targets");
        if (context->target == TARGET_GNU_ASM_ATT || context->target == TARGET_GNU_ASM_INTEL) {
          if (var->decl->declaration.linkage == LINKAGE_EXPORTED)
            fprint(context->code, ".global %S\n", var->name);
          fprint(context->code, "%S: .byte ", var->name);

          span s = ir_string_data(context, var->init);
          foreach (c, s) {
            if (c != s.data) fprint(context->code, ", ");
            fprint(context->code, "%u", (unsigned) *c);
          }
          fprint(context->code, ",0\n");
        }

#ifdef X86_64_GENERATE_MACHINE_CODE
        STATIC_ASSERT(TARGET_COUNT == 6, "Exhaustive handling of object targets");
        if (context->target == TARGET_COFF_OBJECT || context->target == TARGET_ELF_OBJECT) {
          // Create symbol for var->name at current offset within the .rodata section
          GObjSymbol sym = {0};
          sym.type = sym_type;
          sym.name = strdup(var->name.data);
          sym.section_name = strdup(".rodata");
          sym.byte_offset = sec_rodata->data.bytes.size;
          vector_push(object.symbols, sym);
          // Write string bytes to .rodata section
          span s = ir_string_data(context, var->init);
          sec_write_n(sec_rodata, s.data, s.size);
          sec_write_1(sec_rodata, 0);
        }
#endif // x86_64_GENERATE_MACHINE_CODE

      } else {
        ir_print_instruction(stdout, var->init);
        ICE("Unhandled literal IR type for static variable in x86_64 backend, sorry.");
      }
    } else {
      /// Allocate space for the variable.
      usz sz = type_sizeof(var->type);
      STATIC_ASSERT(TARGET_COUNT == 6, "Exhaustive handling of assembly targets");
      if (context->target == TARGET_GNU_ASM_ATT || context->target == TARGET_GNU_ASM_INTEL) {
        if (linkage == LINKAGE_EXPORTED || linkage == LINKAGE_USED)
          fprint(context->code, ".global %S\n", var->name);
        if (!imported)
        fprint(context->code,
               ".align %Z\n"
               "%S: .space %zu\n",
               (usz)type_alignof(var->type), var->name, sz);
      }

#ifdef X86_64_GENERATE_MACHINE_CODE
      STATIC_ASSERT(TARGET_COUNT == 6, "Exhaustive handling of object targets");
      if (context->target == TARGET_COFF_OBJECT || context->target == TARGET_ELF_OBJECT) {
        if (imported) {
          // Create symbol referencing external var->name
          GObjSymbol sym = {0};
          sym.type = GOBJ_SYMTYPE_EXTERNAL;
          sym.name = strdup(var->name.data);
          sym.section_name = strdup(".bss");
          vector_push(object.symbols, sym);
        } else {
          // Create symbol for var->name at current offset within the .bss section
          GObjSymbol sym = {0};
          sym.type = GOBJ_SYMTYPE_STATIC;
          sym.name = strdup(var->name.data);
          sym.section_name = strdup(".bss");
          // Align to type's alignment requirements.
          sec_uninitdata->data.fill.amount = ALIGN_TO(sec_uninitdata->data.fill.amount, type_alignof(var->type));
          sym.byte_offset = sec_uninitdata->data.fill.amount;
          vector_push(object.symbols, sym);
          // Write uninitialised bytes to .data section
          sec_uninitdata->data.fill.amount += sz;
        }
      }
#endif
    }
  }

  if (debug_ir) ir_print(stdout, context);

  // Mangle function names, and assign block labels.
  usz block_cnt = 0;
  foreach_val (function, context->functions) {
    mangle_function_name(function);
    FOREACH_BLOCK (block, function) {
        /// FIXME: This should be unnecessary now that we have
        ///        proper optimisation passes:
/*      if (optimise) {
        /// Determine whether this block is ever referenced anywhere.
        bool referenced = false;
        for (IRBlock *b = (function->blocks).first; b; b = b->next) {
          for (IRInstruction *i = (b->instructions).first; i; i = i->next) {
            switch (i->kind) {
              default: break;
              case IR_UNREACHABLE: goto next_block;
              case IR_BRANCH:
                if (i->destination_block == block) {
                  /// Direct branches to the next block are no-ops.
                  if (i->destination_block == block->next) goto next_block;
                  referenced = true;
                  goto done;
                }
                break;
              case IR_BRANCH_CONDITIONAL:
                if (i->cond_br.then == block) {
                  referenced = true;
                  goto done;
                }
                if (i->cond_br.else_ == block) {
                  referenced = true;
                  goto done;
                }
                break;
            }
          }
        next_block:;
        }

      done:
        if (!referenced) {
          block->name = string_dup(unreferenced_block_name);
          continue;
        }
      }*/

      ir_name(block, format(".L%U", block_cnt++));
    }
  }

  /*ir_set_ids(context);
  ir_print(stdout, context);*/

  MIRFunctionVector machine_instructions_from_ir = mir_from_ir(context);

  // TODO: Either embed x86_64 isel or somehow make this path knowable (i.e. via install).
  ISelPatterns patterns =  isel_parse_file(ISEL_TABLE_LOCATION_X86_64);

  //isel_print_patterns(&patterns, mir_x86_64_opcode_mnemonic);

  if (debug_ir) {
    print("================ ISel ================\n"
          "Before:\n");
    foreach_val (f, machine_instructions_from_ir)
      print_mir_function_with_mnemonic(f, mir_x86_64_opcode_mnemonic);
  }

  // ISel in code...
  foreach_val (function, machine_instructions_from_ir) {
    if (!function->origin || !ir_func_is_definition(function->origin)) continue;
    foreach_val (block, function->blocks) {
      MIRInstructionVector instructions_to_remove = {0};
      foreach_index (i, block->instructions) {
        MIRInstruction* instruction = block->instructions.data[i];
        switch (instruction->opcode) {
        default: break;

        case MIR_TRUNCATE: {
          vector_push(instructions_to_remove, instruction);

          MIROperand *src = mir_get_op(instruction, 0);
          MIROperand *from_op = mir_get_op(instruction, 1);
          MIROperand *to_op = mir_get_op(instruction, 2);
          i64 sz_from = from_op->value.imm;
          i64 sz_to = to_op->value.imm;
          ASSERT(sz_from > sz_to, "Truncate must be from larger to smaller size");
          ASSERT(sz_from <= 8, "Cannot truncate something larger than a register!");

          i64 mask = 0;
          switch (sz_to) {
          case 1: mask = 0xff; break;
          case 2: mask = 0xffff; break;
          case 4: mask = 0xffffffff; break;
          default: ICE("Unhandled truncate \"to\" size %I", sz_to);
          }

          if (src->kind == MIR_OP_IMMEDIATE) {
            i64 imm = src->value.imm & mask;
            MIRInstruction *move = mir_makenew(MX64_MOV);
            mir_add_op(move, mir_op_immediate(imm));
            mir_add_op(move, mir_op_reference(instruction));
            mir_insert_instruction_with_reg(instruction->block, move, i++, instruction->reg);
            break;
          }

          MIRInstruction *move = mir_makenew(MX64_MOV);
          mir_add_op(move, *src);
          mir_add_op(move, mir_op_reference(instruction));
          mir_insert_instruction(instruction->block, move, i++);

          MIRInstruction *and = mir_makenew(MX64_AND);
          mir_add_op(and, mir_op_immediate(mask));
          mir_add_op(and, mir_op_reference(instruction));
          mir_insert_instruction_with_reg(instruction->block, and, i++, instruction->reg);
        } break; // case MIR_TRUNCATE

        /// Handle low-level intrinsics. The first operand
        /// is the intrinsic kind.
        case MIR_INTRINSIC: {
          MIROperand *kind = mir_get_op(instruction, 0);
          ASSERT(kind->kind == MIR_OP_IMMEDIATE, "Intrinsic kind must be an immediate");
          STATIC_ASSERT(INTRIN_BACKEND_COUNT == 3, "Handle backend intrinsics in codegen");
          switch (kind->value.imm) {
            IGNORE_FRONTEND_INTRINSICS();

            /// Memcpy should already have been lowered.
            case INTRIN_BUILTIN_MEMCPY: UNREACHABLE();

            /// For syscalls, just emit a bunch of moves and the syscall.
            case INTRIN_BUILTIN_SYSCALL: {
              ASSERT(context->call_convention == CG_CALL_CONV_SYSV);
              ASSERT(instruction->operand_count <= 7);

              /// Syscall argument registers are slightly different from
              /// the regular C calling convention. Note: The syscall number
              /// is treated as the first argument by us.
              static enum Registers_x86_64 syscall_arg_regs[7] = {
                REG_RAX, REG_RDI, REG_RSI, REG_RDX, REG_R10, REG_R8, REG_R9
              };

              enum Registers_x86_64* arg_regs = syscall_arg_regs;
              bool first = true;
              FOREACH_MIR_OPERAND (instruction, op) {
                /// Don’t emit the intrinsic kind.
                if (first) {
                  first = false;
                  continue;
                }

                MIRInstruction *mov = mir_makenew(MIR_COPY);
                mir_add_op(mov, *op);
                mir_insert_instruction_with_reg(instruction->block, mov, i++, *arg_regs++);
              }

              /// Insert syscall.
              MIRInstruction *sys = mir_makenew(MX64_SYSCALL);
              mir_add_op(sys, mir_op_register(REG_RAX, r64, true));
              mir_insert_instruction_with_reg(instruction->block, sys, i++, REG_RAX);

              /// Syscalls clobber rcx as well as r8-r11.
              static enum Registers_x86_64 syscall_clobbers[5] = {
                REG_RCX, REG_R8, REG_R9, REG_R10, REG_R11
              };

              MIROperandRegister clobbered = {0};
              clobbered.size = r64;
              for (usz r = 0; r < sizeof syscall_clobbers / sizeof *syscall_clobbers; r++) {
                clobbered.value = syscall_clobbers[r];
                vector_push(sys->clobbers, clobbered);
              }

              /// Yeet intrinsic call.
              vector_push(instructions_to_remove, instruction);
            } break;

            /// For a debug trap, emit an int 3.
            case INTRIN_BUILTIN_DEBUGTRAP: {
              MIRInstruction *int3 = mir_makenew(MX64_INT3);
              mir_insert_instruction(instruction->block, int3, i++);
              vector_push(instructions_to_remove, instruction);
            } break;
          }
        } break; // case MIR_INTRINSIC

        }
      } // foreach (MIRInstruction)

      foreach_val (instruction, instructions_to_remove) {
        mir_remove_instruction(instruction);
      }
      vector_delete(instructions_to_remove);

    } // foreach (MIRBlock)
  } // foreach (MIRFunction)

  // ISel using DSL
  isel_do_selection(machine_instructions_from_ir, patterns);

  if (debug_ir) {
    print("After:\n");
    foreach_val (f, machine_instructions_from_ir)
      print_mir_function_with_mnemonic(f, mir_x86_64_opcode_mnemonic);
  }

  isel_patterns_delete(&patterns);

  if (debug_ir)
    print("================ RA ================\n");

  // RA -- Register Allocation
  foreach_val (f, machine_instructions_from_ir) {
    allocate_registers(f, &desc);
  }

  /// After RA, the last fixups before code emission are applied.
  /// Calculate stack offsets
  /// Lowering of MIR_CALL, among other things (caller-saved registers)
  /// Remove register to register moves when value and size are equal.
  /// Saving/restoration of callee-saved registers used in function.
  foreach_val (function, machine_instructions_from_ir) {
    if (!function->origin || !ir_func_is_definition(function->origin)) continue;

    // Calculate stack offsets of frame objects
    isz offset = 0;
    foreach (fo, function->frame_objects) {
      offset -= fo->size;
      fo->offset = offset;
    }

    ASSERT(function->blocks.size, "Zero blocks within non-extern MIRFunction... How did you manage this?");

    size_t func_regs = ir_func_regs_in_use(function->origin);

    { // Save callee-saved registers used in this function
      MIRBlock *first_block = vector_front(function->blocks);
      for (Register r = 1; r < sizeof(func_regs) * 8; ++r) {
        if (r == desc.result_register) continue;
        if (func_regs & ((usz)1 << r) && is_callee_saved(r)) {
          MIRInstruction *push = mir_makenew(MX64_PUSH);
          mir_add_op(push, mir_op_register(r, r64, false));
          mir_insert_instruction(first_block, push, 0);
        }
      }
    }

    { // Restore callee-saved registers used in this function
      // Okay, I know this looks weird to insert push and pop without
      // reversing iteration direction, but the key here is the insert
      // function we are using; this one adds to the end, whereas the push
      // one adds to the beginning. Therefore, we can do the same loop but
      // have reversed order of output instructions.
      for (Register r = 1; r < sizeof(func_regs) * 8; ++r) {
        if (r == desc.result_register) continue;
        if (func_regs & ((usz)1 << r) && is_callee_saved(r)) {
          MIRInstruction *pop = mir_makenew(MX64_POP);
          mir_add_op(pop, mir_op_register(r, r64, false));
          mir_append_instruction(function, pop);
        }
      }
    }

    foreach_index (block_index, function->blocks) {
      MIRBlock *block = function->blocks.data[block_index];
      MIRInstructionVector instructions_to_remove = {0};
      foreach_index (i, block->instructions) {
        MIRInstruction *instruction = block->instructions.data[i];
        switch (instruction->opcode) {
        default: break;

        // Basic jump threading
        case MX64_JMP: {
          if (mir_operand_kinds_match(instruction, 1, MIR_OP_BLOCK)) {
            MIROperand *op = mir_get_op(instruction, 0);
            MIRBlock *destination = op->value.block;

            // Remove a jump if it is to the next sequential block to be output in
            // code. This means we will fallthrough with no branch, increasing more
            // space for actually useful jumps in the BTB.
            MIRBlock *next_block = NULL;
            if (block_index + 1 < function->blocks.size)
              next_block = function->blocks.data[block_index + 1];
            if (destination == next_block) vector_push(instructions_to_remove, instruction);
          }
        } break; // case MX64_JMP

        case MIR_CALL: {
          // Tail call.
          if (ir_call_tail(instruction->origin)) {
            // Restore the frame pointer if we have one.
            mir_x86_64_function_exit_at(stack_frame_kind(instruction->block->function), instruction->block, &i);
            MIRInstruction *jump = mir_makenew(MX64_JMP);
            mir_add_op(jump, *mir_get_op(instruction, 0));
            mir_insert_instruction(instruction->block, jump, i++);

            /// Don’t forget to remove the call instruction.
            vector_push(instructions_to_remove, instruction);
            break;
          }

          size_t regs_pushed_count = 0;

          // Save return register if it is not the result of this
          // function call already; if it is, the RA has already asserted
          // that RAX can be clobbered by this instruction.
          // TODO: Determine a better way to figure out if we actually
          // need to save the result register over this call boundary.
          if (instruction->reg < MIR_ARCH_START && instruction->reg != desc.result_register && func_regs & (1 << desc.result_register)) {
            MIRInstruction *push = mir_makenew(MX64_PUSH);
            mir_add_op(push, mir_op_register(desc.result_register, r64, false));
            mir_insert_instruction(instruction->block, push, i++);
            regs_pushed_count++;
          }

          // Count caller-saved registers used in function, excluding result register (counted above).
          size_t x = func_regs;
          for (size_t r = REG_RAX + 1; r < sizeof(x) * 8; ++r)
            if (x & ((usz)1 << r) && is_caller_saved((MIRRegister)r))
              regs_pushed_count++;

          // Push caller saved registers
          // TODO: Don't push registers that are used for arguments.
          for (Register r = REG_RAX + 1; r < sizeof(func_regs) * 8; ++r) {
            if (func_regs & ((usz)1 << r) && is_caller_saved(r)) {
              MIRInstruction *push = mir_makenew(MX64_PUSH);
              mir_add_op(push, mir_op_register(r, r64, false));
              mir_insert_instruction(instruction->block, push, i++);
            }
          }

          // The amount of bytes that need to be pushed onto/popped off
          // of the stack, not including saving/restoring of registers.
          isz bytes_pushed = 0;

          if (context->call_convention == CG_CALL_CONV_MSWIN) {
            // Push arguments, if need be.
            isz argument_registers_left = (isz)argument_register_count;
            bool first = true;
            FOREACH_MIR_OPERAND(instruction, arg) {
              if (first) {
                first = false;
                continue;
              }
              --argument_registers_left;
              // If argument is passed on stack due to ABI.
              if (arg->kind == MIR_OP_LOCAL_REF) {
                // Push the base pointer.
                MIRInstruction *push = mir_makenew(MX64_PUSH);
                mir_add_op(push, mir_op_register(REG_RBP, r64, false));
                mir_insert_instruction(instruction->block, push, i++);
                bytes_pushed += 8;
                // Subtract local's offset from base pointer from the newly pushed base pointer.
                MIRInstruction *sub = mir_makenew(MX64_SUB);
                ASSERT(arg->value.local_ref < function->frame_objects.size, "Referenced frame object does not exist");
                mir_add_op(sub, mir_op_immediate(-function->frame_objects.data[arg->value.local_ref].offset)); // value to subtract
                mir_add_op(sub, mir_op_register(REG_RSP, r64, false)); // base address
                mir_add_op(sub, mir_op_immediate(0)); // zero offset from rsp
                mir_add_op(sub, mir_op_immediate(8)); // 8 == sizeof address on stack
                mir_insert_instruction(instruction->block, sub, i++);
              } else if (argument_registers_left < 0) {
                if (arg->kind == MIR_OP_REGISTER) {
                  if (arg->value.reg.size == r64) {
                    MIRInstruction *push = mir_makenew(MX64_PUSH);
                    mir_add_op(push, *arg);
                    mir_insert_instruction(instruction->block, push, i++);
                    bytes_pushed += 8;
                  } else if (arg->value.reg.size == r32) {
                    MIRInstruction *move = mir_makenew(MX64_MOV);
                    mir_add_op(move, *arg);
                    mir_add_op(move, mir_op_register(REG_RAX, r32, false));
                    mir_insert_instruction(instruction->block, move, i++);
                    MIRInstruction *push = mir_makenew(MX64_PUSH);
                    mir_add_op(push, mir_op_register(REG_RAX, r64, false));
                    mir_insert_instruction(instruction->block, push, i++);
                    bytes_pushed += 8;
                  } else {
                    MIRInstruction *move = mir_makenew(MX64_MOVZX);
                    mir_add_op(move, *arg);
                    mir_add_op(move, mir_op_register(REG_RAX, r64, false));
                    mir_insert_instruction(instruction->block, move, i++);
                    MIRInstruction *push = mir_makenew(MX64_PUSH);
                    mir_add_op(push, mir_op_register(REG_RAX, r64, false));
                    mir_insert_instruction(instruction->block, push, i++);
                    bytes_pushed += 8;
                  }
                } else if (arg->kind == MIR_OP_IMMEDIATE) {
                  MIRInstruction *move = mir_makenew(MX64_MOV);
                  mir_add_op(move, *arg);
                  if (arg->value.imm >= INT32_MIN && arg->value.imm <= INT32_MAX)
                    mir_add_op(move, mir_op_register(REG_RAX, r32, false));
                  else mir_add_op(move, mir_op_register(REG_RAX, r64, false));
                  mir_insert_instruction(instruction->block, move, i++);
                  MIRInstruction *push = mir_makenew(MX64_PUSH);
                  mir_add_op(push, mir_op_register(REG_RAX, r64, false));
                  mir_insert_instruction(instruction->block, push, i++);
                  bytes_pushed += 8;
                } else {
                  print_mir_operand(function, arg);
                  TODO("Unhandled stack argument operand with kind %s", mir_operand_kind_string(arg->kind));
                }
              }
            }
          }

          isz bytes_to_push = 0;
          // Align stack pointer before call, if necessary.
          if (regs_pushed_count & 0b1)
            bytes_to_push += 8;
          // Shadow stack
          if (context->call_convention == CG_CALL_CONV_MSWIN)
            bytes_to_push += 32;

          if (bytes_to_push) {
            MIRInstruction *sub = mir_makenew(MX64_SUB);
            mir_add_op(sub, mir_op_immediate(bytes_to_push));
            mir_add_op(sub, mir_op_register(REG_RSP, r64, false));
            mir_insert_instruction(instruction->block, sub, i++);
            bytes_pushed += bytes_to_push;
          }

          MIRInstruction *call = mir_makenew(MX64_CALL);
          call->origin = instruction->origin;
          instruction->lowered = call;
          mir_add_op(call, *mir_get_op(instruction, 0));
          mir_insert_instruction_with_reg(instruction->block, call, i++, instruction->reg);

          // Restore stack
          if (bytes_pushed) {
            MIRInstruction *add = mir_makenew(MX64_ADD);
            mir_add_op(add, mir_op_immediate((isz)bytes_pushed));
            mir_add_op(add, mir_op_register(REG_RSP, r64, false));
            mir_insert_instruction(instruction->block, add, i++);
          }

          // Restore caller saved registers used in called function.
          for (Register r = sizeof(func_regs) * 8 - 1; r > REG_RAX; --r) {
            if (func_regs & ((usz)1 << r) && is_caller_saved(r)) {
              MIRInstruction *pop = mir_makenew(MX64_POP);
              mir_add_op(pop, mir_op_register(r, r64, false));
              mir_insert_instruction(instruction->block, pop, i++);
            }
          }

          // If inst->reg is still a virtual register, then this call's
          // result just gets discarded (no use of it's vreg) so we can just
          // /not/ do this part.
          if (instruction->reg < MIR_ARCH_START && instruction->reg != desc.result_register) {
            MIRInstruction *move = mir_makenew(MX64_MOV);
            mir_add_op(move, mir_op_register(desc.result_register, r64, false));
            mir_add_op(move, mir_op_register(instruction->reg, r64, false));
            mir_insert_instruction(instruction->block, move, i++);

            // Restore return register.
            if (func_regs & (1 << desc.result_register)) {
              MIRInstruction *pop = mir_makenew(MX64_POP);
              mir_add_op(pop, mir_op_register(desc.result_register, r64, false));
              mir_insert_instruction(instruction->block, pop, i++);
            }
          }

          vector_push(instructions_to_remove, instruction);

        } break; // case MIR_CALL

        case MX64_MOVZX: {
          if (mir_operand_kinds_match(instruction, 2, MIR_OP_REGISTER, MIR_OP_REGISTER)) {
            MIROperand *src = mir_get_op(instruction, 0);
            MIROperand *dst = mir_get_op(instruction, 1);
            // There is no movzx r32, r64 ... that's just called a
            // `mov r32, r32`, due to false dependency nonsense. And we
            // don't even need to do that, because the register that we
            // *would* be zeroing out the top bits of has already had to
            // have been moved into or something that clears the top bits.
            if (src->value.reg.size == r32) vector_push(instructions_to_remove, instruction);
          }
        } break; // case MX64_MOVZX

        case MPSEUDO_R2R: {
          if (!mir_operand_kinds_match(instruction, 2, MIR_OP_REGISTER, MIR_OP_REGISTER))
            ICE("MPSEUDO_R2R instruction does not have two register operands.");
          MIROperand *src = mir_get_op(instruction, 0);
          MIROperand *dst = mir_get_op(instruction, 1);

          // Register move from self to self is a no-op.
          if (src->value.reg.value == dst->value.reg.value && src->value.reg.size == dst->value.reg.size)
            vector_push(instructions_to_remove, instruction);
          else if (src->value.reg.size < dst->value.reg.size) {
            // Register move from smaller register to larger register requires zero extension.
            instruction->opcode = MX64_MOVZX;
          } else if (src->value.reg.size > dst->value.reg.size) {
            // Register move from larger register to smaller register requires truncation.
            // To accomplish this, we simply move from a smaller version of the
            // same register; by reducing the source, we have effectively truncated
            // the result.
            instruction->opcode = MX64_MOV;
            src->value.reg.size = dst->value.reg.size;
          }

        } break;

        case MX64_MOV: {
          // MOV(eax, rax) -> ERROR (mov cannot move between mismatched size registers)
          // MOV(REG x, REG x) -> NOP (remove)
          if (mir_operand_kinds_match(instruction, 2, MIR_OP_REGISTER, MIR_OP_REGISTER)) {
            MIROperand *lhs = mir_get_op(instruction, 0);
            MIROperand *rhs = mir_get_op(instruction, 1);
            if (!lhs->value.reg.size) {
              putchar('\n');
              print_mir_instruction_with_mnemonic(instruction, mir_x86_64_opcode_mnemonic);
              print("%35WARNING%m: Zero sized register on rhs, assuming 64-bit...\n");
              putchar('\n');
              lhs->value.reg.size = r64;
            }
            if (!rhs->value.reg.size) {
              putchar('\n');
              print_mir_instruction_with_mnemonic(instruction, mir_x86_64_opcode_mnemonic);
              print("%35WARNING%m: Zero sized register on rhs, assuming 64-bit...\n");
              putchar('\n');
              rhs->value.reg.size = r64;
            }
            if (lhs->value.reg.size != rhs->value.reg.size)
              ICE("x86_64 cannot move between mismatched-sized registers %s and %s, sorry", regname(lhs->value.reg.value, lhs->value.reg.size), regname(rhs->value.reg.value, rhs->value.reg.size));
            if (lhs->value.reg.value == rhs->value.reg.value && lhs->value.reg.size == rhs->value.reg.size) {
              vector_push(instructions_to_remove, instruction);
            }
          }
        } break;

        } // switch (instruction->opcode)

      } // foreach (MIRInstruction*)

      foreach_val (instruction, instructions_to_remove) {
        mir_remove_instruction(instruction);
      }
      vector_delete(instructions_to_remove);

    } // foreach (MIRBlock*)

    if (debug_ir) print_mir_function_with_mnemonic(function, mir_x86_64_opcode_mnemonic);
  } // foreach (MIRFunction*)


  // CODE EMISSION
  // TODO: Allow for multiple targets here?

  STATIC_ASSERT(TARGET_COUNT == 6, "Exhaustive handling of target formats in x86_64 backend");

  // EMIT ASSEMBLY CODE
  if (context->target == TARGET_GNU_ASM_ATT || context->target == TARGET_GNU_ASM_INTEL)
    emit_x86_64_assembly(context, machine_instructions_from_ir);

#ifdef X86_64_GENERATE_MACHINE_CODE

  // EMIT MACHINE CODE (GENERAL OBJECT FILE)
  if (context->target == TARGET_COFF_OBJECT || context->target == TARGET_ELF_OBJECT)
    emit_x86_64_generic_object(context, machine_instructions_from_ir);

  if (context->target == TARGET_COFF_OBJECT || context->target == TARGET_ELF_OBJECT)
    generic_object_print(&object);

  if (context->target == TARGET_COFF_OBJECT)
    generic_object_as_coff_x86_64(&object, context->code);
  if (context->target == TARGET_ELF_OBJECT)
    generic_object_as_elf_x86_64(&object, context->code);

  generic_object_delete(&object);
#endif // x86_64_GENERATE_MACHINE_CODE
}
