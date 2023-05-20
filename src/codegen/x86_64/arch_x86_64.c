#include <codegen/x86_64/arch_x86_64.h>

#include <ast.h>
#include <codegen.h>
#include <codegen/x86_64/arch_x86_64_common.h>
#include <codegen/x86_64/arch_x86_64_isel.h>
#include <codegen/x86_64/arch_x86_64_tgt_assembly.h>
#include <codegen/x86_64/arch_x86_64_tgt_generic_object.h>
#include <codegen/codegen_forward.h>
#include <codegen/intermediate_representation.h>
#include <codegen/instruction_selection.h>
#include <codegen/machine_ir.h>
#include <codegen/register_allocation.h>
#include <error.h>
#include <inttypes.h>
#include <opt.h>
#include <parser.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <typechecker.h>
#include <vector.h>
#include <utils.h>

#define X86_64_GENERATE_MACHINE_CODE

Register *caller_saved_registers = NULL;
size_t caller_saved_register_count = 0;

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
  return cg_ctx;
}

/// Creates a context for the x86_64/CG_CALL_CONV_SYSV.
CodegenContext *codegen_context_x86_64_linux_create() {
  caller_saved_register_count = LINUX_CALLER_SAVED_REGISTER_COUNT;
  caller_saved_registers = linux_caller_saved_registers;
  argument_register_count = LINUX_ARGUMENT_REGISTER_COUNT;
  argument_registers = linux_argument_registers;

  CodegenContext *cg_ctx = calloc(1,sizeof(CodegenContext));
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
  STATIC_ASSERT(IR_COUNT == 38, "Exhaustive handling of IR instruction types that correspond to two-address instructions in x86_64.");
  switch (instruction->kind) {
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

static usz emit_memcpy_impl(CodegenContext *context, IRInstruction *to, IRInstruction *from, usz byte_size, usz iter_amount, IRInstruction *insert_before_this) {
  for (; byte_size >= iter_amount; byte_size -= iter_amount) {
    // Load iter_amount bytes from "from" address, and store iter_amount bytes into "to" address.
    INSTRUCTION(load, IR_LOAD);
    { // Generate load of element...
      load->operand = from;

      Type *t = type_canonical(from->type);
      if (!(t && type_is_pointer(t))) {
        //print("from type: %T\n", from->type);
        ir_femit_instruction(stdout, from);
        if (t) ICE("Can not emit IR_LOAD from type %T as it is not a pointer", t);
        else ICE("Can not emit IR_LOAD to NULL canonical type!");
      }
      if (type_is_pointer(t)) load->type = t->pointer.to;
      else load->type = t;

      mark_used(from, load);
    }
    insert_instruction_before(load, insert_before_this);

    INSTRUCTION(store, IR_STORE);
    { // Store loaded element into local stack array...
      store->store.addr = to;
      store->store.value = load;
      mark_used(to, store);
      mark_used(load, store);
    }
    insert_instruction_before(store, insert_before_this);

    if (byte_size - iter_amount < iter_amount || byte_size - iter_amount > byte_size) {
      // Do this iteration, then break.
      byte_size -= iter_amount;
      break;
    }

    { // Iterate "from" and "to" addresses by iter_amount bytes.

      // Generate an immediate corresponding to the byte size of this member
      INSTRUCTION(byte_size_immediate, IR_IMMEDIATE);
      byte_size_immediate->type = t_integer_literal;
      byte_size_immediate->imm = iter_amount;
      insert_instruction_before(byte_size_immediate, insert_before_this);

      INSTRUCTION(add, IR_ADD);
      add->type = from->type;
      set_pair_and_mark(add, from, byte_size_immediate);
      insert_instruction_before(add, insert_before_this);
      from = add;

      INSTRUCTION(dest_add, IR_ADD);
      dest_add->type = to->type;
      set_pair_and_mark(dest_add, to, byte_size_immediate);
      insert_instruction_before(dest_add, insert_before_this);
      to = dest_add;
    }
  }
  return byte_size;
}

static void emit_memcpy(CodegenContext *context, IRInstruction *to_, IRInstruction *from_, usz byte_size, IRInstruction *insert_before_this) {
  // Create two copies, one of each address: "from" and "to".
  // Cache address we are loading from.
  IRInstruction *from = ir_copy(context, from_);
  insert_instruction_before(from, insert_before_this);
  // Switch type to reflect loading 8 bytes.
  from->type = ast_make_type_pointer(context->ast, t_integer->source_location, t_integer);
  // Cache address we are storing to.
  IRInstruction *to = ir_copy(context, to_);
  insert_instruction_before(to, insert_before_this);
  // Switch type to reflect storing 8 bytes.
  to->type = ast_make_type_pointer(context->ast, t_integer->source_location, t_integer);

  if ((byte_size = emit_memcpy_impl(context, to, from, byte_size, 8, insert_before_this))) {
    // Switch type to reflect storing 1 byte.
    from->type = ast_make_type_pointer(context->ast, t_byte->source_location, t_byte);
    to->type = ast_make_type_pointer(context->ast, t_byte->source_location, t_byte);
    emit_memcpy_impl(context, to, from, byte_size, 1, insert_before_this);
  }
}

/// Given an `IR_LOAD` instruction, return true iff the load has been altered.
/// Otherwise, return false.
static bool lower_load(CodegenContext *context, IRInstruction *instruction) {
  ASSERT(context, "x86_64: lower_load(): Context must not be NULL");
  ASSERT(instruction, "x86_64: lower_load(): IRInstruction must not be NULL");
  Type *type = type_canonical(instruction->type);
  usz byte_size = type_sizeof(type);
  if (byte_size > max_register_size) {
    // TODO: Just copy the whole thing in one register if it is <= the max_register_size

    // Create space for a copy on the stack.
    INSTRUCTION(alloca, IR_ALLOCA);
    alloca->alloca.size = byte_size;
    alloca->type = ast_make_type_pointer(context->ast, type->source_location, type);
    ir_set_backend_flag(alloca, STORE_UNDERLYING);

    insert_instruction_before(alloca, instruction);

    emit_memcpy(context, alloca, instruction->operand, type_sizeof(instruction->type), instruction);

    // Replace `load` with `alloca`.
    ir_replace_uses(instruction, alloca);
    ir_remove(instruction);
    return true;
  }
  return false;
}

/// Return true iff the given store instruction has been altered.
/// Otherwise, return false.
static bool lower_store(CodegenContext *context, IRInstruction *instruction) {
  // If the value is an `alloca`, we are copying from a local
  // variable to somewhere. This means we need to emit the equivalent
  // of `memcpy(address, value, sizeof(*value));`
  if (ir_get_backend_flag(instruction->store.value, STORE_UNDERLYING)) {
    // `inst->store.value->result` register contains an address we should store from.
    // `inst->store.addr->result` register contains an address we should store to.

    ASSERT(instruction->store.value->type->kind == TYPE_POINTER,
           "ALLOCA must be of pointer type to store it properly... What did you do?");
    usz byte_size = type_sizeof(instruction->store.value->type->pointer.to);
    emit_memcpy(context, instruction->store.addr, instruction->store.value, byte_size, instruction);

    ir_remove(instruction);
    return true;
  }
  return false;
}

static IRInstruction *alloca_copy_of(CodegenContext *context, IRInstruction *copy, IRInstruction *insert_before_this) {
  INSTRUCTION(alloca, IR_ALLOCA);
  alloca->alloca.size = type_sizeof(copy->type);
  alloca->type = ast_make_type_pointer(context->ast, copy->type->source_location, copy->type);
  insert_instruction_before(alloca, insert_before_this);

  INSTRUCTION(store, IR_STORE);

  store->store.addr = alloca;
  mark_used(alloca, store);

  store->store.value = copy;
  mark_used(copy, store);

  insert_instruction_before(store, insert_before_this);

  return alloca;
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
      type == t_integer ||
      type == t_byte) {
    return SYSV_REGCLASS_INTEGER;
  }
  // FIXME: Probably more efficient to have the size check be the
  // outermost check, and then divy it up based on more type specifics.
  if (type_is_array(type) || type_is_struct(type)) {
    usz size = type_sizeof(type);
    // If the size of an object is larger than four eightbytes, or it
    // contains unaligned fields, it has class MEMORY.
    // TODO: Check for unaligned fields.
    // Technically, the only things that get lowered into multiple
    // registers are two eightbytes or smaller; so while the "rule"
    // above *does* say four eightbytes, it actually is only two.
    if (size > 16) return SYSV_REGCLASS_MEMORY;
    // If the size of the aggregate exceeds a single eightbyte,
    // each is classified separately. Each eightbyte gets
    // initialized to class NO_CLASS.
    else if (size > 8) {

      // At this point we have a 9-16 byte aggregate type.

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

      if (type_is_struct(type))
        return SYSV_REGCLASS_INTEGER;
    }
    // Anything 1, 2, 4, or 8 bytes can go in a register.
    else return SYSV_REGCLASS_INTEGER;
  }
  return SYSV_REGCLASS_INVALID;
}

/// @return How many registers an argument takes up.
usz sysv_argument_register_count_x86_64(CodegenContext *context, Type *function, usz parameter_index) {
  ASSERT(context->call_convention == CG_CALL_CONV_SYSV, "Don't call sysv_* things unless you are using the SYSV ABI!!");
  ASSERT(function->kind == TYPE_FUNCTION);

  if (parameter_index >= function->function.parameters.size)
    ICE("Parameter index out of bounds");

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
  for (usz i = 0; i < parameter_index; ++i) {
    argument_register_offset += sysv_argument_register_count_x86_64(context, function, i);
  }
  return argument_register_offset;
}

enum StackFrameKind {
  FRAME_FULL,
  FRAME_MINIMAL,
  FRAME_NONE,
  FRAME_COUNT
};

static enum StackFrameKind stack_frame_kind(IRFunction *f) {
  /// Always emit a frame if we’re not optimising.
  if (!optimise) return FRAME_FULL;

  /// Emit a frame if we have local variables.
  if (f->locals_total_size) return FRAME_FULL;

  /// We need *some* sort of prologue if we don’t use the stack but
  /// still call other functions.
  if (!f->attr_leaf) return FRAME_MINIMAL;

  /// Otherwise, no frame is required.
  return FRAME_NONE;
}

static void lower(CodegenContext *context) {
  ASSERT(argument_registers, "arch_x86_64 backend can not lower IR when argument registers have not been initialized.");

  FOREACH_INSTRUCTION (context) {
    switch (instruction->kind) {
    default:
      break;

    case IR_RETURN: {
      STATIC_ASSERT(CG_CALL_CONV_COUNT == 2, "Exhaustive handling of calling convention return register during x86_64 lowering");
      switch (context->call_convention) {
      case CG_CALL_CONV_MSWIN: {
        instruction->result = REG_RAX;
        if (instruction->operand)
          instruction->type = instruction->operand->type;
        else instruction->type = t_integer;
      } break;
      case CG_CALL_CONV_SYSV: {
        instruction->result = REG_RAX;
        if (instruction->operand)
          instruction->type = instruction->operand->type;
        else instruction->type = t_integer;
      } break;
      default: UNREACHABLE();
      }
    } break;

    case IR_LOAD: {
      lower_load(context, instruction);
    } break;
    case IR_STORE: {
      lower_store(context, instruction);
    } break;

    case IR_ALLOCA: {
      // Worry about stack alignment
      if (instruction->alloca.size < 8)
        instruction->alloca.size = 8;
    } break;

    case IR_CALL: {
      size_t argcount = instruction->call.arguments.size;

      switch (context->call_convention) {
      default: ICE("Unhandled calling convention in x86_64 lowering of calls");
      case CG_CALL_CONV_SYSV: {
        Type *function_type = NULL;
        if (instruction->call.is_indirect) {
          if (type_is_pointer(instruction->call.callee_instruction->type))
            function_type = instruction->call.callee_instruction->type->pointer.to;
          else function_type = instruction->call.callee_instruction->type;
        } else function_type = instruction->call.callee_function->type;
        ASSERT(function_type->kind == TYPE_FUNCTION, "Expected callee of IR_CALL to be a function, but got %T\n", function_type);

        Vector(usz) sixteen_bytes_that_need_split = {0};
        foreach_index (i, function_type->function.parameters) {
          Parameter *parameter = function_type->function.parameters.data + i;
          SysVArgumentClass class = sysv_classify_argument(parameter->type);
          if (class == SYSV_REGCLASS_INTEGER && type_sizeof(parameter->type) > 8)
            vector_push(sixteen_bytes_that_need_split, i);
        }

        foreach_rev (usz, i, sixteen_bytes_that_need_split) {
          IRInstruction *argument = instruction->call.arguments.data[*i];

          // Load first eightbyte of the parameter.
          INSTRUCTION(first_eightbyte_addr, IR_COPY);
          first_eightbyte_addr->operand = argument;
          first_eightbyte_addr->type = ast_make_type_pointer(context->ast, t_integer->source_location, t_integer);
          insert_instruction_before(first_eightbyte_addr, instruction);

          INSTRUCTION(load1, IR_LOAD);
          load1->operand = first_eightbyte_addr;
          load1->type = t_integer;
          insert_instruction_before(load1, instruction);

          // Load second eightbyte of the parameter.
          // FIXME: Second eightbyte may not be fully eight bytes.
          ASSERT(type_sizeof(argument->type->pointer.to) == 16,
                 "SysV ABI requires alignment of a multiple of 16 for aggregate types from (8 to 16]: %T",
                 argument->type->pointer.to);
          INSTRUCTION(offset, IR_IMMEDIATE);
          offset->type = t_integer;
          offset->imm = 8;
          insert_instruction_before(offset, instruction);

          INSTRUCTION(second_eightbyte_addr, IR_ADD);
          second_eightbyte_addr->type = first_eightbyte_addr->type;
          second_eightbyte_addr->lhs = first_eightbyte_addr;
          mark_used(first_eightbyte_addr, second_eightbyte_addr);
          second_eightbyte_addr->rhs = offset;
          mark_used(offset, second_eightbyte_addr);
          insert_instruction_before(second_eightbyte_addr, instruction);

          INSTRUCTION(load2, IR_LOAD);
          load2->operand = second_eightbyte_addr;
          load2->type = t_integer;
          insert_instruction_before(load2, instruction);

          // Remove argument from call, and replace with two new arguments.
          ir_remove_use(argument, instruction);
          vector_remove_index(instruction->call.arguments, *i);
          vector_insert_after(instruction->call.arguments, load1, instruction->call.arguments.data + *i);
          mark_used(load1, instruction);
          vector_insert_after(instruction->call.arguments, load2, instruction->call.arguments.data + *i);
          mark_used(load2, instruction);

        }
      } break;
      case CG_CALL_CONV_MSWIN: {
        usz idx = 0;

        // Lower aggregates in possible-register arguments by allocating a copy of them on the stack.
        foreach_ptr (IRInstruction *, argument, instruction->call.arguments) {
          if (idx >= argument_register_count) break;
          Type *type = type_canonical(argument->type);
          if ((type->kind == TYPE_STRUCT || type->kind == TYPE_ARRAY) && type_sizeof(type) > 8) {
            instruction->call.arguments.data[idx] = alloca_copy_of(context, argument, instruction);
          }
          ++idx;
        }

        // Lower all arguments not able to go in a register by allocating a copy of them on the stack.
        if (argcount >= argument_register_count) {
          usz i = instruction->call.arguments.size - 1;
          foreach_ptr_rev (IRInstruction *, argument, instruction->call.arguments) {
            if (i < argument_register_count) break;
            instruction->call.arguments.data[i] = alloca_copy_of(context, argument, instruction);
            --i;
          }
        }
      } break;
      }
    } break;

    case IR_PARAMETER: {
      switch (context->call_convention) {

      case CG_CALL_CONV_SYSV: {
        if (parameter_is_in_register_x86_64(context, instruction->parent_block->function, instruction->imm)) {
          // Classify argument into register class.
          // NOTE: This has probably already been done, and we could
          // cache it and use that computed value, if we have somewhere
          // to store it.
          SysVArgumentClass class = SYSV_REGCLASS_INVALID;
          class = sysv_classify_argument(instruction->type);
          ASSERT(class != SYSV_REGCLASS_INVALID, "Could not classify argument according to SYSV ABI, sorry");
          switch(class) {
          case SYSV_REGCLASS_INTEGER: {
            usz size = type_sizeof(instruction->type);
            if (size > 8) {
              ASSERT(size <= 16, "Can only pass things that are two-eightbytes or less in general purpose registers.");

              INSTRUCTION(eightbyte1, IR_REGISTER);
              usz argument_register_index = sysv_argument_register_index_x86_64(context, instruction->parent_block->function->type, instruction->imm);
              ASSERT(argument_register_index + 1 < argument_register_count);
              eightbyte1->result = argument_registers[argument_register_index];
              eightbyte1->type = t_integer;
              insert_instruction_before(eightbyte1, instruction);

              INSTRUCTION(eightbyte2, IR_REGISTER);
              eightbyte2->result = argument_registers[argument_register_index + 1];
              eightbyte2->type = t_integer;
              insert_instruction_before(eightbyte2, instruction);

              INSTRUCTION(alloca, IR_ALLOCA);
              alloca->alloca.size = 16;
              alloca->type = ast_make_type_pointer(context->ast, instruction->type->source_location, instruction->type);
              insert_instruction_before(alloca, instruction);

              // Store first eight bytes from parameter register into
              // newly allocated local variable.
              INSTRUCTION(store1, IR_STORE);
              store1->store.addr = alloca;
              mark_used(alloca, store1);
              store1->store.value = eightbyte1;
              mark_used(eightbyte1, store1);
              insert_instruction_before(store1, instruction);

              // Increment address
              INSTRUCTION(offset, IR_IMMEDIATE);
              offset->type = t_integer;
              offset->imm = 8;
              insert_instruction_before(offset, instruction);

              INSTRUCTION(address, IR_ADD);
              address->lhs = alloca;
              mark_used(alloca, address);
              address->rhs = offset;
              mark_used(offset, address);
              address->type = ast_make_type_pointer(context->ast, t_integer->source_location, t_integer);
              insert_instruction_before(address, instruction);

              // Store second eightbyte.
              INSTRUCTION(store2, IR_STORE);
              store2->kind = IR_STORE;
              store2->store.addr = address;
              mark_used(address, store2);
              store2->store.value = eightbyte2;
              mark_used(eightbyte2, store2);
              insert_instruction_before(store2, instruction);

              instruction->kind = IR_LOAD;
              instruction->operand = alloca;

              lower_load(context, instruction);
            } else {
              instruction->kind = IR_REGISTER;
              instruction->result = argument_registers[instruction->imm];
            }
          } break;
          case SYSV_REGCLASS_MEMORY: {
            INSTRUCTION(rbp, IR_REGISTER);
            rbp->result = REG_RBP;
            rbp->type = t_integer;
            insert_instruction_before(rbp, instruction);

            usz parameter_index = instruction->imm;

            INSTRUCTION(offset, IR_IMMEDIATE);
            offset->type = t_integer;

            // FIXME: Tail calls, leaf functions, etc. may alter the size of the stack frame here.
            // Skip pushed RBP and return addess.
            offset->imm += 16;

            usz i = instruction->parent_block->function->type->function.parameters.size - 1;
            foreach_rev (Parameter, param, instruction->parent_block->function->type->function.parameters) {
              if (i <= parameter_index) break;
              offset->imm += type_sizeof(param->type);
              --i;
            }
            insert_instruction_before(offset, instruction);

            INSTRUCTION(address, IR_ADD);
            address->lhs = rbp;
            mark_used(rbp, address);
            address->rhs = offset;
            mark_used(offset, address);
            address->type = ast_make_type_pointer(context->ast, instruction->type->source_location, instruction->type);
            insert_instruction_before(address, instruction);

            instruction->kind = IR_LOAD;
            instruction->operand = address;
            mark_used(address, instruction);

            lower_load(context, instruction);

          } break;
          default:
            TODO("Handle lowering of SYSV Register Classification: %d\n", class);
          }
        }
      } break;

      case CG_CALL_CONV_MSWIN: {
        Type *type = type_canonical(instruction->type);
        // NOTE: Arrays and strings in Intercept are passed like
        // structs in C, so this doesn't apply to *Intercept*
        // arrays/strings.
        // __m128 types, arrays, and strings are never passed by immediate value.
        // Structs and unions of size 8, 16, 32, or 64 bits, and __m64
        // types, are passed as if they were integers of the same size.
        if (instruction->imm >= argument_register_count) {

          // Calculate offset to caller-allocated stack memory for large parameters.

          // Lower type to a pointer, because that's how the calls have
          // been altered as well.
          INSTRUCTION(rbp, IR_REGISTER);
          rbp->result = REG_RBP;
          rbp->type = t_integer;
          insert_instruction_before(rbp, instruction);

          usz parameter_index = instruction->imm;

          INSTRUCTION(offset, IR_IMMEDIATE);
          offset->type = t_integer;

          // FIXME: Tail calls, leaf functions, etc. may alter the size of the stack frame here.
          // Skip pushed RBP and return addess.
          offset->imm += 16;

          usz i = instruction->parent_block->function->type->function.parameters.size - 1;
          foreach_rev (Parameter, param, instruction->parent_block->function->type->function.parameters) {
            if (i <= parameter_index) break;
            offset->imm += type_sizeof(param->type);
            --i;
          }
          insert_instruction_before(offset, instruction);

          INSTRUCTION(address, IR_ADD);
          address->lhs = rbp;
          mark_used(rbp, address);
          address->rhs = offset;
          mark_used(offset, address);
          if (type_sizeof(type) > 8)
            address->type = ast_make_type_pointer(context->ast, instruction->type->source_location, instruction->type);
          else address->type = instruction->type;
          insert_instruction_before(address, instruction);

          instruction->kind = IR_LOAD;
          instruction->operand = address;
          mark_used(address, instruction);

        } else {
          instruction->kind = IR_REGISTER;
          instruction->result = argument_registers[instruction->imm];
        }
      } break;

      default: ICE("Unhandled call convention for parameter lowering.");
      }
    } break;

    case IR_BITCAST: {
      instruction->kind = IR_COPY;
    } break;
    }
  }
}

static size_t interfering_regs(IRInstruction *instruction) {
  ASSERT(instruction, "Can not get register interference of NULL instruction.");
  size_t mask = 0;

  // FIXME: It'd be really great if we /didn't/ have to loop over every
  // single user of every single instruction here, but I don't see another
  // way of doing this, really.
  // Divisor of div/mod are not allowed to go in RAX/RDX; that's where
  // the dividend must go.
  foreach_ptr (IRInstruction*, inst, instruction->users) {
    if ((inst->kind == IR_DIV || inst->kind == IR_MOD) && inst->rhs == instruction) {
      mask |= ((usz)1 << REG_RAX);
      mask |= ((usz)1 << REG_RDX);
    }
  }

  switch(instruction->kind) {
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

// TODO: This should probably be used by every backend, so it should
// move "up" somewhere.
static void mangle_type_to(string_buffer *buf, Type *t) {
  ASSERT(t);
  switch (t->kind) {
    default: TODO("Handle type kind %d in type mangling!", (int)t->kind);

    case TYPE_STRUCT:
      if (t->structure.decl->struct_decl->name.size)
        format_to(buf, "%Z%S", t->structure.decl->struct_decl->name.size, t->structure.decl->struct_decl->name);
      else {
        static usz struct_count = 0;
        format_to(buf, "%Z%Z", number_width(struct_count), struct_count);
        ++struct_count;
      }
      break;

    case TYPE_PRIMITIVE:
      format_to(buf, "%Z%S", t->primitive.name.size, t->primitive.name);
      break;

    case TYPE_NAMED:
      if (!t->named->val.type) format_to(buf, "%Z%S", t->named->name.size, t->named->name);
      else mangle_type_to(buf, t->named->val.type);
      break;

    case TYPE_POINTER:
      format_to(buf, "P");
      mangle_type_to(buf, t->pointer.to);
      break;

    case TYPE_REFERENCE:
      format_to(buf, "R");
      mangle_type_to(buf, t->reference.to);
      break;

    case TYPE_ARRAY:
      format_to(buf, "A%ZE", t->array.size);
      mangle_type_to(buf, t->array.of);
      break;

    case TYPE_FUNCTION:
      format_to(buf, "F");
      mangle_type_to(buf, t->function.return_type);
      foreach (Parameter, param, t->function.parameters) mangle_type_to(buf, param->type);
      format_to(buf, "E");
      break;
  }
}

void mangle_function_name(IRFunction *function) {
  if (function->is_extern) return;

  string_buffer buf = {0};
  format_to(&buf, "_XF%Z%S", function->name.size, function->name);
  mangle_type_to(&buf, function->type);
  free(function->name.data);
  function->name = (string){buf.data, buf.size};
}

void codegen_lower_x86_64(CodegenContext *context) { lower(context); }

bool parameter_is_in_register_x86_64(CodegenContext *context, IRFunction *function, usz parameter_index) {
  if (parameter_index >= function->type->function.parameters.size)
    ICE("Parameter index out of bounds");

  IRInstruction *parameter = function->parameters.data[parameter_index];

  switch (context->call_convention) {

  case CG_CALL_CONV_MSWIN: {
    if (parameter_index >= 4) return false;
    if (type_sizeof(parameter->type) > 8) return false;
  } return true;

  case CG_CALL_CONV_SYSV: {
    SysVArgumentClass class = SYSV_REGCLASS_INVALID;
    class = sysv_classify_argument(parameter->type);
    ASSERT(class != SYSV_REGCLASS_INVALID, "Could not classify argument according to SYSV ABI, sorry");
    if (class == SYSV_REGCLASS_INTEGER) return true;
    if (class == SYSV_REGCLASS_MEMORY) return false;
    TODO("Handle SYSV Register Classification: %d\n", class);
  }

  default:
    ICE("Unhandled calling convention: %d\n", context->call_convention);
  }

}

static void mir_x86_64_function_entry(enum StackFrameKind frame_kind, MIRFunction *f) {
  ASSERT(f, "Invalid argument");
  STATIC_ASSERT(FRAME_COUNT == 3, "Exhaustive handling of stack frame kinds in function entry MIR lowering");
  ASSERT(frame_kind < FRAME_COUNT, "Invalid stack frame kind!");
  switch (frame_kind) {
  case FRAME_COUNT: UNREACHABLE();
  case FRAME_NONE: break;

  case FRAME_FULL: {
    // PUSH %RBP
    MIRInstruction *save_bp = mir_makenew(MX64_PUSH);
    mir_add_op(save_bp, mir_op_register(REG_RBP, r64, false));
    // MOV %RSP, %RBP
    MIRInstruction *save_sp = mir_makenew(MX64_MOV);
    mir_add_op(save_sp, mir_op_register(REG_RSP, r64, false));
    mir_add_op(save_sp, mir_op_register(REG_RBP, r64, false));
    if (f->origin->locals_total_size) {
      // SUB $f->locals_total_size, %RSP
      MIRInstruction *locals = mir_makenew(MX64_SUB);
      mir_add_op(locals, mir_op_immediate((int64_t)f->origin->locals_total_size));
      mir_add_op(locals, mir_op_register(REG_RSP, r64, false));
      mir_prepend_instruction(f, locals);
    }

    // Function entry (backwards because prepending)
    mir_prepend_instruction(f, save_sp);
    mir_prepend_instruction(f, save_bp);
  } break;

  case FRAME_MINIMAL: {
    // PUSH %RBP
    MIRInstruction *save_bp = mir_makenew(MX64_PUSH);
    mir_add_op(save_bp, mir_op_register(REG_RBP, r64, false));
    mir_prepend_instruction(f, save_bp);
  } break;
  }
}

static void mir_x86_64_function_exit_at(enum StackFrameKind frame_kind, MIRBlock *block, usz *index) {
  STATIC_ASSERT(FRAME_COUNT == 3, "Exhaustive handling of stack frame kinds in function entry MIR lowering");
  ASSERT(frame_kind < FRAME_COUNT, "Invalid stack frame kind!");
  switch (frame_kind) {
  case FRAME_COUNT: UNREACHABLE();
  case FRAME_NONE: break;

  case FRAME_FULL: {
    // MOV %RBP, %RSP
    MIRInstruction *restore_sp = mir_makenew(MX64_MOV);
    mir_add_op(restore_sp, mir_op_register(REG_RBP, r64, false));
    mir_add_op(restore_sp, mir_op_register(REG_RSP, r64, false));
    mir_insert_instruction(block, restore_sp, ++*index);
    // POP %RBP
    MIRInstruction *restore_bp = mir_makenew(MX64_POP);
    mir_add_op(restore_bp, mir_op_register(REG_RBP, r64, false));
    mir_insert_instruction(block, restore_sp, ++*index);
  } break;

  case FRAME_MINIMAL: {
    // POP %RBP
    MIRInstruction *restore_bp = mir_makenew(MX64_POP);
    mir_add_op(restore_bp, mir_op_register(REG_RBP, r64, false));
    mir_insert_instruction(block, restore_bp, ++*index);
  } break;
  }
}

static void mir_x86_64_function_exit(enum StackFrameKind frame_kind, MIRFunction *f) {
  ASSERT(f, "Invalid argument");
  STATIC_ASSERT(FRAME_COUNT == 3, "Exhaustive handling of stack frame kinds in function entry MIR lowering");
  ASSERT(frame_kind < FRAME_COUNT, "Invalid stack frame kind!");
  switch (frame_kind) {
  case FRAME_COUNT: UNREACHABLE();
  case FRAME_NONE: break;

  case FRAME_FULL: {
    // MOV %RBP, %RSP
    MIRInstruction *restore_sp = mir_makenew(MX64_MOV);
    mir_add_op(restore_sp, mir_op_register(REG_RBP, r64, false));
    mir_add_op(restore_sp, mir_op_register(REG_RSP, r64, false));
    mir_append_instruction(f, restore_sp);
    // POP %RBP
    MIRInstruction *restore_bp = mir_makenew(MX64_POP);
    mir_add_op(restore_bp, mir_op_register(REG_RBP, r64, false));
    mir_append_instruction(f, restore_bp);
  } break;

  case FRAME_MINIMAL: {
    // POP %RBP
    MIRInstruction *restore_bp = mir_makenew(MX64_POP);
    mir_add_op(restore_bp, mir_op_register(REG_RBP, r64, false));
    mir_append_instruction(f, restore_bp);
  } break;
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
  foreach_ptr (IRStaticVariable*, var, context->static_vars) {
    /// Do not emit unused variables.
    if (optimise) {
        bool used = false;
        foreach_ptr (IRInstruction*, ref, var->references) {
            if (ref->users.size) {
                used = true;
                break;
            }
        }
        if (!used) continue;
    }

    /// Emit a data section directive if we haven't already.
    if (!have_data_section) {
      have_data_section = true;
      fprint(context->code, ".section .data\n");
    }

    // Do compile-time known static assignment.

    if (var->init) {
      if (var->init->kind == IR_LIT_INTEGER) {
        fprint(context->code, "%S: .byte ", var->name);
        uint8_t *byte_repr = (uint8_t*)(&var->init->imm);

        // TODO: Endianness selection

        // `%u` and the `(unsigned)` cast is because variadic arguments
        // of integral types are always promoted to at least `int` or
        // `unsigned` in C.
        fprint(context->code, "%u", (unsigned) byte_repr[0]);
        for (usz i = 1; i < type_sizeof(var->type); ++i)
          fprint(context->code, ",%u", (unsigned) byte_repr[i]);

        fprint(context->code, "\n");


#ifdef X86_64_GENERATE_MACHINE_CODE
        // Create symbol for var->name at current offset within the .data section
        GObjSymbol sym = {0};
        sym.type = GOBJ_SYMTYPE_STATIC;
        sym.name = strdup(var->name.data);
        sym.section_name = strdup(".data");
        sym.byte_offset = sec_initdata->data.bytes.size;
        vector_push(object.symbols, sym);
        // Write initialised bytes to .data section
        sec_write_n(sec_initdata, byte_repr, type_sizeof(var->type));
#endif // x86_64_GENERATE_MACHINE_CODE

      } else if (var->init->kind == IR_LIT_STRING) {
        {// MANUAL (required because multiline strings)
          fprint(context->code, "%S: .byte ", var->name);
          if (var->init->str.size)
            fprint(context->code, "%u", (unsigned) var->init->str.data[0]);
          for (usz i = 1; i < var->init->str.size; ++i)
            fprint(context->code, ",%u", (unsigned) var->init->str.data[i]);
          fprint(context->code, ",0\n");
        }

#ifdef X86_64_GENERATE_MACHINE_CODE
        // Create symbol for var->name at current offset within the .rodata section
        GObjSymbol sym = {0};
        sym.type = GOBJ_SYMTYPE_STATIC;
        sym.name = strdup(var->name.data);
        sym.section_name = strdup(".rodata");
        sym.byte_offset = sec_initdata->data.bytes.size;
        vector_push(object.symbols, sym);
        // Write string bytes to .rodata section
        sec_write_n(sec_rodata, var->init->str.data, var->init->str.size);
        sec_write_1(sec_rodata, 0);
#endif // x86_64_GENERATE_MACHINE_CODE

      }
      else {
        ir_femit_instruction(stdout, var->init);
        ICE("Unhandled literal IR type for static variable in x86_64 backend, sorry.");
      }
    } else {
      /// Allocate space for the variable.
      usz sz = type_sizeof(var->type);
      fprint(context->code, "%S: .space %zu\n", var->name, sz);

#ifdef X86_64_GENERATE_MACHINE_CODE
      // Create symbol for var->name at current offset within the .bss section
      GObjSymbol sym = {0};
      sym.type = GOBJ_SYMTYPE_STATIC;
      sym.name = strdup(var->name.data);
      sym.section_name = strdup(".bss");
      sym.byte_offset = sec_initdata->data.bytes.size;
      vector_push(object.symbols, sym);
      // Write uninitialised bytes to .data section
      sec_uninitdata->data.fill.amount += sz;
#endif
    }
  }

  if (debug_ir) ir_femit(stdout, context);

  // Mangle function names, and assign block labels.
  usz block_cnt = 0;
  foreach_ptr (IRFunction*, function, context->functions) {
    // Don't mangle external function(s).
    if (!function->attr_nomangle) mangle_function_name(function);

    list_foreach (IRBlock *, block, function->blocks) {
      if (optimise) {
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
                  if (i->cond_br.then == i->parent_block->next) continue;
                  referenced = true;
                  goto done;
                }
                if (i->cond_br.else_ == block) {
                  if (i->cond_br.else_ == i->parent_block->next) continue;
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
      }

      block->name = format(".L%U", block_cnt++);
    }
  }

  /*ir_set_ids(context);
  ir_femit(stdout, context);*/

  MIRFunctionVector machine_instructions_from_ir = mir_from_ir(context);

  // Lowering in code.
  foreach_ptr (MIRFunction*, f, machine_instructions_from_ir) {
    print_mir_function(f);

    if (f->origin && f->origin->is_extern) continue;

    // Restore callee-saved registers used in the function.
    for (Register i = sizeof(f->origin->registers_in_use) * 8 - 1; i > 0; --i) {
      if (f->origin->registers_in_use & ((size_t)1 << i) && is_callee_saved(i)) {
        MIRInstruction *restore = mir_makenew(MX64_POP);
        mir_add_op(restore, mir_op_register(i, r64, false));
        mir_append_instruction(f, restore);
      }
    }

    // Function prologue/epilogue
    enum StackFrameKind frame_kind = stack_frame_kind(f->origin);
    mir_x86_64_function_entry(frame_kind, f);
    mir_x86_64_function_exit(frame_kind, f);

  }

  print("================ ISel ================\n");

  // TODO: Either embed x86_64 isel or somehow make this path knowable (i.e. via install).
  const char isel_filepath[] = "src/codegen/x86_64/arch_x86_64.isel";
  ISelPatterns patterns =  isel_parse_file(isel_filepath);

  //isel_print_patterns(&patterns, mir_x86_64_opcode_mnemonic);

  isel_do_selection(machine_instructions_from_ir, patterns);

  foreach_ptr (MIRFunction*, f, machine_instructions_from_ir) {
    print_mir_function_with_mnemonic(f, mir_x86_64_opcode_mnemonic);
  }

  isel_patterns_delete(&patterns);

  // RA -- Register Allocation
  foreach_ptr (MIRFunction*, f, machine_instructions_from_ir) {
    allocate_registers(f, &desc);
  }

  /// After RA, the last fixups before code emission are applied.
  // Lowering of MIR_CALL
  // Remove register to register moves when value and size are equal.
  foreach_ptr (MIRFunction*, function, machine_instructions_from_ir) {
    if (function->origin && function->origin->is_extern) continue;
    foreach_ptr (MIRBlock*, block, function->blocks) {

      struct MIRInstructionPlusIndex {
        MIRInstruction *inst;
        usz index;
      };
      Vector(struct MIRInstructionPlusIndex) calls_to_fixup = {0};
      MIRInstructionVector instructions_to_remove = {0};

      foreach_index (i, block->instructions) {
        MIRInstruction *instruction = block->instructions.data[i];
        switch (instruction->opcode) {
        default: break;

        case MIR_CALL: {
          struct MIRInstructionPlusIndex val = {0};
          val.inst = instruction;
          val.index = i;
          vector_push(calls_to_fixup, val);
        } break; // case MIR_CALL

        case MX64_MOV: {
          // MOV(REG x, REG x) -> NOP
          if (mir_operand_kinds_match(instruction, 2, MIR_OP_REGISTER, MIR_OP_REGISTER) &&
              mir_get_op(instruction, 0)->value.reg.value == mir_get_op(instruction, 1)->value.reg.value &&
              mir_get_op(instruction, 0)->value.reg.size == mir_get_op(instruction, 1)->value.reg.size) {
            vector_push(instructions_to_remove, instruction);
          }
        } break;
        } // switch (instruction->opcode)

      }

      usz index_difference = 0;
      foreach (struct MIRInstructionPlusIndex, val, calls_to_fixup) {
        // Account for added instructions
        usz inst_index = val->index + index_difference;
        MIRInstruction *inst = val->inst;

        // Tail call.
        if (inst->origin->call.tail_call) {
          // Restore the frame pointer if we have one.
          mir_x86_64_function_exit_at(stack_frame_kind(inst->origin->parent_block->function), inst->block, &inst_index);
          MIRInstruction *jump = mir_makenew(MX64_JMP);
          mir_add_op(jump, *mir_get_op(inst, 0));
          mir_insert_instruction(inst->block, jump, inst_index++);
          // FIXME: I don't think this does anything; what was it for?
          if (inst->origin->parent_block) inst->origin->parent_block->done = true;
          break;
        }

        size_t func_regs = inst->block->function->origin->registers_in_use;
        size_t regs_pushed_count = 0;

        // Save return register if it is not the result of this
        // function call already; if it is, the RA has already asserted
        // that RAX can be clobbered by this instruction.
        if (inst->reg != desc.result_register && func_regs & desc.result_register) {
          MIRInstruction *push = mir_makenew(MX64_PUSH);
          mir_add_op(push, mir_op_register(desc.result_register, r64, false));
          mir_insert_instruction(inst->block, push, inst_index++);
          regs_pushed_count++;
        }

        // Count caller-saved registers used in function, excluding result register (counted above).
        size_t x = func_regs;
        for (size_t i = REG_RAX + 1; i < sizeof(x) * 8; ++i)
          if (x & ((usz)1 << i) && is_caller_saved((MIRRegister)i))
            regs_pushed_count++;

        // Align stack pointer before call, if necessary.
        if (regs_pushed_count & 0b1) {
          MIRInstruction *sub = mir_makenew(MX64_SUB);
          mir_add_op(sub, mir_op_immediate(8));
          mir_add_op(sub, mir_op_register(REG_RSP, r64, false));
          mir_insert_instruction(inst->block, sub, inst_index++);
        }

        // Push caller saved registers
        // TODO: Don't push registers that are used for arguments.
        for (Register i = REG_RAX + 1; i < sizeof(func_regs) * 8; ++i) {
          if (func_regs & ((usz)1 << i) && is_caller_saved(i)) {
            MIRInstruction *push = mir_makenew(MX64_PUSH);
            mir_add_op(push, mir_op_register(i, r64, false));
            mir_insert_instruction(inst->block, push, inst_index++);
          }
        }

        usz bytes_pushed = 0;
        // Shadow stack
        if (context->call_convention == CG_CALL_CONV_MSWIN) {
          MIRInstruction *sub = mir_makenew(MX64_SUB);
          mir_add_op(sub, mir_op_immediate(32));
          mir_add_op(sub, mir_op_register(REG_RSP, r64, false));
          mir_insert_instruction(inst->block, sub, inst_index++);
          bytes_pushed += 32;
        }

        // Push argument addresses, if need be.
        bool first = true;
        FOREACH_MIR_OPERAND(inst, arg) {
          if (first) {
            first = false;
            continue;
          }
          // If argument is passed on stack due to ABI.
          if (arg->kind == MIR_OP_LOCAL_REF) {
            // Push the base pointer.
            MIRInstruction *push = mir_makenew(MX64_PUSH);
            mir_add_op(push, mir_op_register(REG_RBP, r64, false));
            mir_insert_instruction(inst->block, push, inst_index++);
            bytes_pushed += 8;
            // Subtract local's offset from base pointer from the newly pushed base pointer.
            MIRInstruction *sub = mir_makenew(MX64_SUB);
            ASSERT(arg->value.local_ref < function->frame_objects.size, "Referenced frame object does not exist");
            mir_add_op(sub, mir_op_immediate(function->frame_objects.data[arg->value.local_ref].offset));
            mir_add_op(sub, mir_op_register(REG_RSP, r64, false));
            mir_insert_instruction(inst->block, sub, inst_index++);
          }
        }

        MIRInstruction *call = mir_makenew(MX64_CALL);
        call->origin = inst->origin;
        inst->lowered = call;
        mir_add_op(call, *mir_get_op(inst, 0));
        mir_insert_instruction_with_reg(inst->block, call, inst_index++, inst->reg);

        // Restore stack
        if (bytes_pushed) {
          MIRInstruction *add = mir_makenew(MX64_ADD);
          mir_add_op(add, mir_op_immediate((isz)bytes_pushed));
          mir_add_op(add, mir_op_register(REG_RSP, r64, false));
          mir_insert_instruction(inst->block, add, inst_index++);
        }

        // Restore caller saved registers used in called function.
        for (Register i = sizeof(func_regs) * 8 - 1; i > REG_RAX; --i) {
          if (func_regs & ((usz)1 << i) && is_caller_saved(i)) {
            MIRInstruction *pop = mir_makenew(MX64_POP);
            mir_add_op(pop, mir_op_register(i, r64, false));
            mir_insert_instruction(inst->block, pop, inst_index++);
          }
        }

        // Restore stack pointer from stack alignment, if necessary.
        if (regs_pushed_count & 0b1) {
          MIRInstruction *add = mir_makenew(MX64_ADD);
          mir_add_op(add, mir_op_immediate(8));
          mir_add_op(add, mir_op_register(REG_RSP, r64, false));
          mir_insert_instruction(inst->block, add, inst_index++);
        }

        if (inst->reg != desc.result_register) {
          MIRInstruction *move = mir_makenew(MX64_MOV);
          mir_add_op(move, mir_op_register(desc.result_register, r64, false));
          mir_add_op(move, mir_op_register(inst->reg, r64, false));
          mir_insert_instruction(inst->block, move, inst_index++);

          // Restore return register.
          if (func_regs & desc.result_register) {
            MIRInstruction *pop = mir_makenew(MX64_POP);
            mir_add_op(pop, mir_op_register(desc.result_register, r64, false));
            mir_insert_instruction(inst->block, pop, inst_index++);
          }
        }

        vector_push(instructions_to_remove, inst);

        // Record added instructions
        index_difference += inst_index - val->index;
      }
      vector_delete(calls_to_fixup);

      foreach_ptr (MIRInstruction*, instruction, instructions_to_remove) {
        mir_remove_instruction(instruction);
      }
      vector_delete(instructions_to_remove);

    }

    print_mir_function_with_mnemonic(function, mir_x86_64_opcode_mnemonic);
  }

  // CODE EMISSION
  // TODO: Allow for multiple targets here.

  // EMIT ASSEMBLY CODE
  if (context->target == TARGET_GNU_ASM_ATT || context->target == TARGET_GNU_ASM_INTEL)
    emit_x86_64_assembly(context, machine_instructions_from_ir);

#ifdef X86_64_GENERATE_MACHINE_CODE
  // EMIT MACHINE CODE (GENERAL OBJECT FILE)
  if (context->target == TARGET_COFF_OBJECT || context->target == TARGET_ELF_OBJECT)
    emit_x86_64_generic_object(context, machine_instructions_from_ir);
  if (context->target == TARGET_COFF_OBJECT)
    generic_object_as_coff_x86_64(&object, "out.obj");
  if (context->target == TARGET_ELF_OBJECT)
    generic_object_as_elf_x86_64(&object, "out.o");

  generic_object_delete(&object);
#endif // x86_64_GENERATE_MACHINE_CODE
}
