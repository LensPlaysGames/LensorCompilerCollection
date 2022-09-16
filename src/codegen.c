#include <codegen.h>

#include <environment.h>
#include <error.h>
#include <inttypes.h>
#include <parser.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <typechecker.h>

char codegen_verbose = 1;

/// This is used for defining lookup tables etc. and
/// ensures that the registers are always in the correct
/// order
#define FOR_ALL_X86_64_REGISTERS(F)     \
  F(RAX, "rax", "eax", "ax", "al")      \
  F(RBX, "rbx", "ebx", "bx", "bx")      \
  F(RCX, "rcx", "ecx", "cx", "cx")      \
  F(RDX, "rdx", "edx", "dx", "dx")      \
  F(R8,  "r8", "r8d", "r8w", "r8b")     \
  F(R9,  "r9", "r9d", "r9w", "r9b")     \
  F(R10, "r10", "r10d", "r10w", "r10b") \
  F(R11, "r11", "r11d", "r11w", "r11b") \
  F(R12, "r12", "r12d", "r12w", "r12b") \
  F(R13, "r13", "r13d", "r13w", "r13b") \
  F(R14, "r14", "r14d", "r14w", "r14b") \
  F(R15, "r15", "r15d", "r15w", "r15b") \
  F(RSI, "rsi", "esi", "si", "sil")     \
  F(RDI, "rdi", "edi", "di", "dil")     \
  F(RBP, "rbp", "ebp", "bp", "bpl")     \
  F(RSP, "rsp", "esp", "sp", "spl")     \
  F(RIP, "rip", "eip", "ip", "ipl")

#define DEFINE_REGISTER_ENUM(name, ...) REG_X86_64_##name,
#define REGISTER_NAME_64(ident, name, ...) name,
#define REGISTER_NAME_32(ident, name, name_32, ...) name_32,
#define REGISTER_NAME_16(ident, name, name_32, name_16, ...) name_16,
#define REGISTER_NAME_8(ident, name, name_32, name_16, name_8, ...) name_8,

/// Used when initializing Register arrays for RegisterPool.
#define INIT_REGISTER(ident, ...)  \
  ((registers)[REG_X86_64_##ident] = (Register){.in_use = 0, .descriptor = (REG_X86_64_##ident)});

/// Lookup tables for register names.
#define DEFINE_REGISTER_NAME_LOOKUP_FUNCTION(name, bits)                                        \
  const char *name(RegisterDescriptor descriptor) {                                             \
    static const char* register_names[] = { FOR_ALL_X86_64_REGISTERS(REGISTER_NAME_##bits) };   \
    if (descriptor < 0 || descriptor >= REG_X86_64_COUNT) {                                     \
      panic("ERROR::" #name "(): Could not find register with descriptor of %d\n", descriptor); \
    }                                                                                           \
    return register_names[descriptor];                                                          \
  }

enum Registers_x86_64 {
  FOR_ALL_X86_64_REGISTERS(DEFINE_REGISTER_ENUM)
  REG_X86_64_COUNT
};

/// Define register_name and friends.
DEFINE_REGISTER_NAME_LOOKUP_FUNCTION(register_name, 64)
DEFINE_REGISTER_NAME_LOOKUP_FUNCTION(register_name_32, 32)
DEFINE_REGISTER_NAME_LOOKUP_FUNCTION(register_name_16, 16)
DEFINE_REGISTER_NAME_LOOKUP_FUNCTION(register_name_8, 8)

#undef REGISTER_NAME_64
#undef REGISTER_NAME_32
#undef REGISTER_NAME_16
#undef REGISTER_NAME_8

#undef DEFINE_REGISTER_ENUM
#undef DEFINE_REGISTER_NAME_LOOKUP_FUNCTION

// TODO: All instructions we use in x86_64 should be in this enum.
enum Instructions_x86_64 {
  INSTRUCTION_X86_64_ADD,
  INSTRUCTION_X86_64_SUB,
  INSTRUCTION_X86_64_MUL,
  INSTRUCTION_X86_64_IMUL,
  INSTRUCTION_X86_64_DIV,
  INSTRUCTION_X86_64_IDIV,
  INSTRUCTION_X86_64_PUSH,
  INSTRUCTION_X86_64_POP,
  INSTRUCTION_X86_64_XOR,
  INSTRUCTION_X86_64_CMP,
  INSTRUCTION_X86_64_CALL,
  INSTRUCTION_X86_64_RET,
  INSTRUCTION_X86_64_MOV,
  INSTRUCTION_X86_64_LEA,
  INSTRUCTION_X86_64_SETCC,
  INSTRUCTION_X86_64_COUNT
};

enum InstructionOperands_x86_64 {
  IMMEDIATE,
  MEMORY,
  REGISTER,
  NAME,

  IMMEDIATE_TO_REGISTER,
  IMMEDIATE_TO_MEMORY,
  MEMORY_TO_REGISTER,
  NAME_TO_REGISTER,
  REGISTER_TO_REGISTER,
  REGISTER_TO_MEMORY,

};

const char *instruction_mnemonic_x86_64(enum CodegenOutputFormat fmt, enum Instructions_x86_64 instruction) {
  ASSERT(INSTRUCTION_X86_64_COUNT == 15, "ERROR: instruction_mnemonic_x86_64() must exhaustively handle all instructions.");
  // x86_64 instructions that aren't different across syntaxes can go here!
  switch (instruction) {
  default:
    break;
  case INSTRUCTION_X86_64_ADD:
    return "add";
  case INSTRUCTION_X86_64_SUB:
    return "sub";
  case INSTRUCTION_X86_64_MUL:
    return "mul";
  case INSTRUCTION_X86_64_IMUL:
    return "imul";
  case INSTRUCTION_X86_64_DIV:
    return "div";
  case INSTRUCTION_X86_64_IDIV:
    return "idiv";
  case INSTRUCTION_X86_64_PUSH:
    return "push";
  case INSTRUCTION_X86_64_POP:
    return "pop";
  case INSTRUCTION_X86_64_XOR:
    return "xor";
  case INSTRUCTION_X86_64_CMP:
    return "cmp";
  case INSTRUCTION_X86_64_CALL:
    return "call";
  case INSTRUCTION_X86_64_RET:
    return "ret";
  case INSTRUCTION_X86_64_MOV:
    return "mov";
  case INSTRUCTION_X86_64_LEA:
    return "lea";
  case INSTRUCTION_X86_64_SETCC:
    return "set";
  }
  panic("Could not convert instruction into mnemonic for x86_64");
  return NULL; // Unreachable
}

void femit_x86_64_imm_to_reg(CodegenContext *context, const char *mnemonic, va_list args) {
  int64_t immediate                    = va_arg(args, int64_t);
  RegisterDescriptor destination_register  = va_arg(args, RegisterDescriptor);

  const char *destination = register_name(destination_register);

  fprintf(context->code, "%s $%" PRId64 ", %s\n",
          mnemonic, immediate, destination);
}

void femit_x86_64_imm_to_mem(CodegenContext *context, const char *mnemonic, va_list args) {
  int64_t immediate                    = va_arg(args, int64_t);
  RegisterDescriptor address_register  = va_arg(args, RegisterDescriptor);
  int64_t offset                       = va_arg(args, int64_t);

  const char *address = register_name(address_register);

  fprintf(context->code, "%s $%" PRId64 ", %" PRId64 "(%s)\n",
          mnemonic, immediate, offset, address);
}

void femit_x86_64_mem_to_reg(CodegenContext *context, const char *mnemonic, va_list args) {
  int64_t offset                           = va_arg(args, int64_t);
  RegisterDescriptor address_register      = va_arg(args, RegisterDescriptor);
  RegisterDescriptor destination_register  = va_arg(args, RegisterDescriptor);

  const char *address = register_name(address_register);
  const char *destination = register_name(destination_register);

  fprintf(context->code,
          "%s %" PRId64 "(%s), %s\n",
          mnemonic, offset, address, destination);
}

void femit_x86_64_name_to_reg(CodegenContext *context, const char *mnemonic, va_list args) {
  char *name                               = va_arg(args, char *);
  RegisterDescriptor address_register      = va_arg(args, RegisterDescriptor);
  RegisterDescriptor destination_register  = va_arg(args, RegisterDescriptor);

  const char *address = register_name(address_register);
  const char *destination = register_name(destination_register);

  fprintf(context->code,
          "%s %s(%s), %s\n",
          mnemonic, name, address, destination);
}

void femit_x86_64_reg_to_mem(CodegenContext *context, const char *mnemonic, va_list args) {
  RegisterDescriptor source_register   = va_arg(args, RegisterDescriptor);
  RegisterDescriptor address_register  = va_arg(args, RegisterDescriptor);
  int64_t offset                       = va_arg(args, int64_t);

  const char *source = register_name(source_register);
  const char *address = register_name(address_register);

  fprintf(context->code,
          "%s %s, %" PRId64 "(%s)\n",
          mnemonic, source, offset, address);
}

void femit_x86_64_reg_to_reg(CodegenContext *context, const char *mnemonic, va_list args) {
  RegisterDescriptor source_register       = va_arg(args, RegisterDescriptor);
  RegisterDescriptor destination_register  = va_arg(args, RegisterDescriptor);

  const char *source = register_name(source_register);
  const char *destination = register_name(destination_register);

  fprintf(context->code,
          "%s %s, %s\n",
          mnemonic, source, destination);
}


void femit_x86_64
(CodegenContext *context,
 enum Instructions_x86_64 instruction,
 ...
 )
{
  va_list args;
  va_start(args, instruction);

  const char *mnemonic = instruction_mnemonic_x86_64(context->format, instruction);

  ASSERT(context);
  switch (context->format) {
  default:
    break;
  case CG_FMT_x86_64_GAS: {
    ASSERT(INSTRUCTION_X86_64_COUNT == 15, "femit_x86_64() must exhaustively handle all x86_64 instructions for GAS syntax.");
    switch (instruction) {
    default:
      panic("Unhandled instruction in x86_64 GAS code generation.");
    case INSTRUCTION_X86_64_ADD:
    case INSTRUCTION_X86_64_SUB:
    case INSTRUCTION_X86_64_MOV: {
      enum InstructionOperands_x86_64 operands = va_arg(args, enum InstructionOperands_x86_64);
      switch (operands) {
      default:
        panic("Unhandled operand type in x86_64 GAS code generation for MOV.");
      case IMMEDIATE_TO_REGISTER:
        // femit(..., IMMEDIATE_TO_REGISTER, (int64_t)immediate value, (RegisterDescriptor) destination)
        femit_x86_64_imm_to_reg(context, mnemonic, args);
        break;
      case IMMEDIATE_TO_MEMORY:
        // femit(..., IMMEDIATE_TO_MEMORY, (int64_t)immediate value, (RegisterDescriptor) memory_address, (int64_t) memory_offset)
        femit_x86_64_imm_to_mem(context, mnemonic, args);
        break;
      case MEMORY_TO_REGISTER:
        // femit(..., MEMORY_TO_REGISTER, (int64_t)memory_offset, (RegisterDescriptor) memory_address, (RegisterDescriptor) destination)
        femit_x86_64_mem_to_reg(context, mnemonic, args);
        break;
      case REGISTER_TO_MEMORY:
        // femit(..., REGISTER_TO_MEMORY, (RegisterDescriptor) source, (RegisterDescriptor) address, (int64_t) memory_offset)
        femit_x86_64_reg_to_mem(context, mnemonic, args);
        break;
      case REGISTER_TO_REGISTER:
        // femit(..., REGISTER_TO_REGISTER, (RegisterDescriptor) source, (RegisterDescriptor) destination)
        femit_x86_64_reg_to_reg(context, mnemonic, args);
        break;
      }
      break;
    }
    case INSTRUCTION_X86_64_LEA: {
      enum InstructionOperands_x86_64 operands = va_arg(args, enum InstructionOperands_x86_64);
      switch (operands) {
      default:
        panic("femit_x86_64() only accepts MEMORY_TO_REGISTER or NAME_TO_REGISTER operand type with LEA instruction.");
      case MEMORY_TO_REGISTER:
        // femit(..., MEMORY_TO_REGISTER, (int64_t) memory_offset, (RegisterDescriptor) memory_address, (RegisterDescriptor) destination)
        femit_x86_64_mem_to_reg(context, mnemonic, args);
        break;
      case NAME_TO_REGISTER:
        // femit(..., MEMORY_TO_REGISTER, (char *) name, (RegisterDescriptor) memory_address, (RegisterDescriptor) destination)
        femit_x86_64_name_to_reg(context, mnemonic, args);
        break;
      }
      break;
    }
    case INSTRUCTION_X86_64_IMUL: {
      enum InstructionOperands_x86_64 operands = va_arg(args, enum InstructionOperands_x86_64);
      switch (operands) {
      default:
        panic("femit_x86_64() only accepts MEMORY_TO_REGISTER or REGISTER_TO_REGISTER operand type with IMUL instruction.");
        break;
      case MEMORY_TO_REGISTER:
        femit_x86_64_mem_to_reg(context, mnemonic, args);
        break;
      case REGISTER_TO_REGISTER:
        femit_x86_64_reg_to_reg(context, mnemonic, args);
        break;
      }
      break;
    }
    case INSTRUCTION_X86_64_IDIV: {
      enum InstructionOperands_x86_64 operands = va_arg(args, enum InstructionOperands_x86_64);
      switch (operands) {
      default:
        panic("femit_x86_64() only accepts MEMORY or REGISTER operand type with IDIV instruction.");
        break;
      case MEMORY: {
        // femit(..., MEMORY, (int64_t) memory_offset, (RegisterDescriptor) memory_address);
        int64_t offset                           = va_arg(args, int64_t);
        RegisterDescriptor destination_register  = va_arg(args, RegisterDescriptor);

        const char *destination = register_name(destination_register);

        fprintf(context->code, "%s %" PRId64 "(%s)\n",
                mnemonic, offset, destination);
        break;
      }
      case REGISTER: {
        // femit(..., REGISTER, (RegisterDescriptor) source);
        RegisterDescriptor source_register = va_arg(args, RegisterDescriptor);

        const char *source = register_name(source_register);

        fprintf(context->code, "%s %s\n",
                mnemonic, source);
        break;
      }
      }
      break;
    }
    case INSTRUCTION_X86_64_CALL: {
      enum InstructionOperands_x86_64 operand = va_arg(args, enum InstructionOperands_x86_64);

      if (operand == REGISTER) {
        RegisterDescriptor call_register = va_arg(args, RegisterDescriptor);

        const char *call_address = register_name(call_register);

        fprintf(context->code, "%s *%s\n",
                mnemonic, call_address);

      } else if (operand == NAME) {
        char *name = va_arg(args, char *);

        fprintf(context->code, "%s %s\n",
                mnemonic, name);

      } else {
        panic("femit_x86_64() only accepts REGISTER or NAME operand type with CALL instruction.");
      }

      break;
    }
    case INSTRUCTION_X86_64_PUSH: {
      enum InstructionOperands_x86_64 operand = va_arg(args, enum InstructionOperands_x86_64);
      if (operand == REGISTER) {
        // femit(..., REGISTER, (RegisterDescriptor) value_to_push)
        RegisterDescriptor value_register = va_arg(args, RegisterDescriptor);

        const char *value = register_name(value_register);

        fprintf(context->code, "%s %s\n",
                mnemonic, value);
      } else if (operand == MEMORY) {
        // femit(..., MEMORY, (RegisterDescriptor) memory_address, (int64_t) memory_offset)
        RegisterDescriptor address_register  = va_arg(args, RegisterDescriptor);
        int64_t offset                       = va_arg(args, RegisterDescriptor);

        const char *address = register_name(address_register);

        fprintf(context->code, "%s %" PRId64 "(%s)",
                mnemonic, offset, address);

      } else if (operand == IMMEDIATE) {
        // femit(..., IMMEDIATE, (int64_t) immediate)
        int64_t immediate = va_arg(args, RegisterDescriptor);

        fprintf(context->code, "%s $%" PRId64 "\n",
                mnemonic, immediate);

      } else {
        panic("femit_x86_64() only accepts REGISTER, MEMORY, or IMMEDIATE operand type with PUSH instruction.");
      }
      break;
    }
    case INSTRUCTION_X86_64_SETCC: {
      enum InstructionOperands_x86_64 operand = va_arg(args, enum InstructionOperands_x86_64);
      if (operand == REGISTER) {
        // femit(..., ComparisonType, REGISTER)
        enum ComparisonType comparison_type = va_arg(args, enum ComparisonType);
        RegisterDescriptor value_register = va_arg(args, RegisterDescriptor);

        const char *value = register_name_8(value_register);

        fprintf(context->code, "%s%s %s\n",
                mnemonic, comparison_suffixes_x86_64[comparison_type], value);
      } else {
        panic("femit_x86_64() SETcc only accepts a REGISTER operand");
      }
      break;
    }
    }
    break;
  }
  }

  va_end(args);
}

/// Creates a context for the CG_FMT_x86_64_MSWIN architecture.
CodegenContext *codegen_context_x86_64_gas_mswin_create(CodegenContext *parent) {
  RegisterPool pool;

  // If this is the top level context, create the registers.
  // Otherwise, shallow copy register pool to child context.
  if (!parent) {
    Register *registers = calloc(REG_X86_64_COUNT, sizeof(Register));
    FOR_ALL_X86_64_REGISTERS(INIT_REGISTER)

    // Link to MSDN documentation (surely will fall away, but it's been Internet Archive'd).
    // https://docs.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-170#callercallee-saved-registers
    // https://web.archive.org/web/20220916164241/https://docs.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-170
    // "The x64 ABI considers the registers RAX, RCX, RDX, R8, R9, R10, R11, and XMM0-XMM5 volatile."
    // "The x64 ABI considers registers RBX, RBP, RDI, RSI, RSP, R12, R13, R14, R15, and XMM6-XMM15 nonvolatile."
    size_t number_of_scratch_registers = 7;
    Register **scratch_registers = calloc(number_of_scratch_registers, sizeof(Register *));
    scratch_registers[0] = registers + REG_X86_64_RAX;
    scratch_registers[1] = registers + REG_X86_64_RCX;
    scratch_registers[2] = registers + REG_X86_64_RDX;
    scratch_registers[3] = registers + REG_X86_64_R8;
    scratch_registers[4] = registers + REG_X86_64_R9;
    scratch_registers[5] = registers + REG_X86_64_R10;
    scratch_registers[6] = registers + REG_X86_64_R11;

    pool.registers = registers;
    pool.scratch_registers = scratch_registers;
    pool.num_scratch_registers = number_of_scratch_registers;
    pool.num_registers = REG_X86_64_COUNT;
  } else {
    pool = parent->register_pool;
  }

  CodegenContext *cg_ctx = calloc(1,sizeof(CodegenContext));
  if (parent) cg_ctx->code = parent->code;
  cg_ctx->parent = parent;
  cg_ctx->locals = environment_create(NULL);
  cg_ctx->locals_offset = -32;
  cg_ctx->register_pool = pool;
  cg_ctx->format = CG_FMT_x86_64_GAS;
  cg_ctx->call_convention = CG_CALL_CONV_MSWIN;
  return cg_ctx;
}

/// Free a context created by codegen_context_x86_64_mswin_create.
void codegen_context_x86_64_mswin_free(CodegenContext *ctx) {
  // Only free the registers if this is the top-level context.
  if (!ctx->parent) free(ctx->register_pool.registers);
  // TODO(sirraide): Free environment.
  free(ctx);
}


CodegenContext *create_codegen_context(CodegenContext *parent) {
  ASSERT(parent, "create_codegen_context() can only create contexts when a parent is given.");
  ASSERT(CG_FMT_COUNT == 1, "create_codegen_context() must exhaustively handle all codegen output formats.");
  ASSERT(CG_CALL_CONV_COUNT == 2, "create_codegen_context() must exhaustively handle all calling conventions.");
  if (parent->format == CG_FMT_x86_64_GAS) {
    if (parent->call_convention == CG_CALL_CONV_MSWIN) {
      return codegen_context_x86_64_gas_mswin_create(parent);
    } else if (parent->call_convention == CG_CALL_CONV_MSWIN) {
      // return codegen_context_x86_64_gas_linux_create(parent);
    }
  }
  panic("create_codegen_context() could not create a new context from the given parent.");
  return NULL; // Unreachable
}

//================================================================ BEG REGISTER STUFF

void print_registers(CodegenContext *cg_ctx) {
  for (size_t i = 0; i < cg_ctx->register_pool.num_registers; ++i) {
    Register *reg = &cg_ctx->register_pool.registers[i];
    printf("%s:%i\n", register_name(reg->descriptor), reg->in_use);
  }
}

char register_descriptor_is_valid(CodegenContext *cg_ctx, RegisterDescriptor descriptor) {
  return descriptor >= 0 && descriptor < (int)cg_ctx->register_pool.num_registers;
}

RegisterDescriptor register_allocate(CodegenContext *cg_ctx) {
  ASSERT(cg_ctx->register_pool.num_registers > 0 && cg_ctx->register_pool.num_scratch_registers > 0, "Register pool is empty");

  for (size_t i = 0; i < cg_ctx->register_pool.num_scratch_registers; ++i) {
    Register *reg = cg_ctx->register_pool.scratch_registers[i];
    if (reg->in_use == 0) {
      reg->in_use = 1;
      return reg->descriptor;
    }
  }
  panic("ERROR::register_allocate(): Could not allocate register!\n");
  return 0; // Unreachable
}

void register_deallocate
(CodegenContext *cg_ctx, RegisterDescriptor descriptor) {
  if (!register_descriptor_is_valid(cg_ctx, descriptor)) {
    panic("ERROR::register_deallocate(): Invalid register descriptor!\n");
  }
  cg_ctx->register_pool.registers[descriptor].in_use = 0;
}

//================================================================ END REGISTER STUFF

//================================================================ BEG CG_FMT_x86_64_MSWIN

#define label_buffer_size 1024
char label_buffer[label_buffer_size];
size_t label_index = 0;
size_t label_count = 0;
static char *label_generate() {
  char *label = label_buffer + label_index;
  label_index += snprintf(label, label_buffer_size - label_index,
                          ".L%zu", label_count);
  label_index++;
  if (label_index >= label_buffer_size) {
    label_index = 0;
    return label_generate();
  }
  label_count++;
  return label;
}

#define symbol_buffer_size 1024
char symbol_buffer[symbol_buffer_size];
size_t symbol_index = 0;
size_t symbol_count = 0;
// TODO: Return Error and bubble, but makes it annoying.
char *symbol_to_address(CodegenContext *cg_ctx, Node *symbol) {
  if (!cg_ctx) {
    printf("ERROR::symbol_to_address(): Context must not be NULL (pass global).\n");
    return NULL;
  }
  if (!symbol || !symbol->value.symbol) {
    printf("ERROR::symbol_to_address(): A symbol must be passed.\n");
    print_node(symbol,2);
    return NULL;
  }
  char *symbol_string = symbol_buffer + symbol_index;
  if (!cg_ctx->parent) {
    // Global variable access.
    symbol_index += snprintf(symbol_string,
                             symbol_buffer_size - symbol_index,
                             "%s(%%rip)",
                             symbol->value.symbol);
  } else {
    // Local variable access.
    Node *stack_offset = node_allocate();
    if (!environment_get(*cg_ctx->locals, symbol, stack_offset)) {
      putchar('\n');
      print_node(symbol,0);
      environment_print(*cg_ctx->locals, 0);
      printf("ERROR: symbol_to_address() Could not find \"%s\" in locals environment.\n",
             symbol->value.symbol);
      return NULL;
    }
    symbol_index += snprintf(symbol_string,
                             symbol_buffer_size - symbol_index,
                             "%lld(%%rbp)", stack_offset->value.integer);
    free(stack_offset);
  }
  symbol_index++;
  if (symbol_index >= symbol_buffer_size) {
    symbol_index = 0;
    return symbol_to_address(cg_ctx, symbol);
  }
  return symbol_string;
}

const char *comparison_suffixes_x86_64[COMPARE_COUNT] = {
    "e",
    "ne",
    "l",
    "le",
    "g",
    "ge",
};

void codegen_comparison_x86_64
(CodegenContext *cg_context,
 Node *expression,
 enum ComparisonType type) {
  ASSERT(type < COMPARE_COUNT, "Invalid comparison type");

  FILE *code = cg_context->code;

  expression->result_register = register_allocate(cg_context);

  // Zero out result register.
  femit_x86_64(cg_context, INSTRUCTION_X86_64_XOR, REGISTER_TO_REGISTER, expression->result_register, expression->result_register);

  // Perform the comparison.
  femit_x86_64(cg_context, INSTRUCTION_X86_64_CMP, REGISTER_TO_REGISTER,
      expression->children->next_child->result_register,
      expression->children->result_register);
  femit_x86_64(cg_context, INSTRUCTION_X86_64_SETCC, type, expression->result_register);

  // Deallocate LHS and RHS result registers, comparison is complete.
  register_deallocate(cg_context, expression->children->result_register);
  register_deallocate(cg_context, expression->children->next_child->result_register);
}

// Forward declare codegen_function for codegen_expression
Error codegen_function_x86_64_gas
(CodegenContext *cg_context,
 ParsingContext *context,
 ParsingContext **next_child_context,
 char *name,
 Node *function);

Error codegen_expression_x86_64
(CodegenContext *cg_context,
 ParsingContext *context,
 ParsingContext **next_child_context,
 Node *expression
 )
{
  Error err = ok;
  char *result = NULL;
  Node *tmpnode = node_allocate();
  Node *iterator = NULL;
  FILE *code = cg_context->code;
  long long count = 0;

  ParsingContext *original_context = context;
  //expression->result_register = -1;

  ASSERT(NODE_TYPE_MAX == 14, "codegen_expression_x86_64() must exhaustively handle node types!");
  switch (expression->type) {
  default:
    break;
  case NODE_TYPE_INTEGER:
    if (codegen_verbose) {
      fprintf(code, ";;#; INTEGER: %lld\n", expression->value.integer);
    }
    expression->result_register = register_allocate(cg_context);
    femit_x86_64(cg_context, INSTRUCTION_X86_64_MOV,
                 IMMEDIATE_TO_REGISTER,
                 (int64_t)expression->value.integer,
                 expression->result_register);
    break;
  case NODE_TYPE_FUNCTION_CALL:
    if (codegen_verbose) {
      fprintf(code, ";;#; Function Call: \"%s\"\n", expression->children->value.symbol);
    }

    // TODO: Should we technically save all in-use scratch registers?
    // Save RAX because function call will over-write it!
    femit_x86_64(cg_context, INSTRUCTION_X86_64_PUSH, REGISTER, REG_X86_64_RAX);

    // Setup function environment based on calling convention.

    Node *variable_type = node_allocate();
    // Use `typecheck_expression()` to get type of variable.
    // err is ignored purposefully, program already type-checked valid.
    typecheck_expression(context, NULL, expression->children, variable_type);

    iterator = expression->children->next_child->children;
    if (strcmp(variable_type->value.symbol, "external function") == 0) {
      // TODO: Save RCX, RDX, R8, and R9 (they are scratch registers).
      // TODO: Only save scratch registers that are in-use.
      // TODO: Don't generate `mov` if both operands are the same!

      // Put arguments in RCX, RDX, R8, R9, then on the stack in reverse order.
      if (iterator) {
        // Place first argument in RCX or XMM0
        err = codegen_expression_x86_64
          (cg_context, context, next_child_context, iterator);
        if (err.type) { return err; }
        // Ensure iterator result register is moved into RCX.
        femit_x86_64(cg_context, INSTRUCTION_X86_64_MOV, REGISTER_TO_REGISTER,
                     iterator->result_register, REG_X86_64_RCX);
        iterator = iterator->next_child;
      }
      if (iterator) {
        // Place second argument in RDX or XMM1
        err = codegen_expression_x86_64
          (cg_context, context, next_child_context, iterator);
        if (err.type) { return err; }
        femit_x86_64(cg_context, INSTRUCTION_X86_64_MOV, REGISTER_TO_REGISTER,
                     iterator->result_register, REG_X86_64_RDX);
        iterator = iterator->next_child;
      }
      if (iterator) {
        // Place third argument in R8 or XMM2
        err = codegen_expression_x86_64
          (cg_context, context, next_child_context, iterator);
        if (err.type) { return err; }
        femit_x86_64(cg_context, INSTRUCTION_X86_64_MOV, REGISTER_TO_REGISTER,
                     iterator->result_register, REG_X86_64_R8);
        iterator = iterator->next_child;
      }
      if (iterator) {
        // Place third argument in R9 or XMM3
        err = codegen_expression_x86_64
          (cg_context, context, next_child_context, iterator);
        if (err.type) { return err; }
        femit_x86_64(cg_context, INSTRUCTION_X86_64_MOV, REGISTER_TO_REGISTER,
                     iterator->result_register, REG_X86_64_R9);
        iterator = iterator->next_child;
      }

      // TODO: Reverse rest of arguments, push on stack.

      femit_x86_64(cg_context, INSTRUCTION_X86_64_CALL, NAME, expression->children->value.symbol);

    } else {
      // Push arguments on stack in order.
      while (iterator) {
        err = codegen_expression_x86_64
          (cg_context, context, next_child_context, iterator);
        if (err.type) { return err; }
        femit_x86_64(cg_context, INSTRUCTION_X86_64_PUSH, REGISTER, iterator->result_register);
        register_deallocate(cg_context, iterator->result_register);
        iterator = iterator->next_child;
        count += 8;
      }

      err = codegen_expression_x86_64(cg_context, context, next_child_context, expression->children);
      if (err.type) { return err; }

      // Emit call
      femit_x86_64(cg_context, INSTRUCTION_X86_64_CALL, REGISTER, expression->children->result_register);
      register_deallocate(cg_context, expression->children->result_register);
    }

    if (count) {
      femit_x86_64(cg_context, INSTRUCTION_X86_64_ADD, IMMEDIATE_TO_REGISTER, count, REG_X86_64_RSP);
    }

    // Copy return value of function call from RAX to result register
    expression->result_register = register_allocate(cg_context);
    if (expression->result_register != REG_X86_64_RAX) {
      femit_x86_64(cg_context, INSTRUCTION_X86_64_MOV, REGISTER_TO_REGISTER,
                   REG_X86_64_RAX, expression->result_register);
      // Save overwritten in-use registers.
      fprintf(code, "pop %%rax\n");
    } else {
      femit_x86_64(cg_context, INSTRUCTION_X86_64_ADD, IMMEDIATE_TO_REGISTER,
                   (int64_t)8, REG_X86_64_RSP);
    }

    break;
  case NODE_TYPE_FUNCTION:
    if (codegen_verbose) {
      fprintf(code, ";;#; Function\n");
    }
    // TODO/FIXME: Obviously this is not ideal to do backwards lookup,
    // especially for function nodes which contain the body... Oh well!
    ParsingContext *context_it = context;
    while (context_it) {
      if (environment_get_by_value(*context_it->functions, expression, tmpnode)) {
        result = tmpnode->value.symbol;
        break;
      }
      context_it = context_it->parent;
    }
    if (!result) {
      // TODO: Keep track of local lambda label in environment or something.
      // FIXME: Completely memory leaked here, no chance of freeing!
      result = label_generate();
    }
    err = codegen_function_x86_64_gas(cg_context,
                                      context, next_child_context,
                                      result, expression);

    // Function returns beginning of instructions address.
    expression->result_register = register_allocate(cg_context);
    femit_x86_64(cg_context, INSTRUCTION_X86_64_LEA, NAME_TO_REGISTER,
                 result, REG_X86_64_RIP, expression->result_register);
    break;
  case NODE_TYPE_DEREFERENCE:
    if (codegen_verbose) {
      fprintf(code, ";;#; Dereference\n");
    }
    err = codegen_expression_x86_64(cg_context,
                                    context, next_child_context,
                                    expression->children);
    if (err.type) { return err; }
    expression->result_register = expression->children->result_register;
    break;
  case NODE_TYPE_ADDRESSOF:
    if (codegen_verbose) {
      fprintf(code, ";;#; Addressof\n");
    }
    expression->result_register = register_allocate(cg_context);
    fprintf(code, "lea %s, %s\n",
            symbol_to_address(cg_context, expression->children),
            register_name(expression->result_register));
    break;
  case NODE_TYPE_INDEX:
    if (codegen_verbose) {
      fprintf(code, ";;#; Index %lld\n", expression->value.integer);
    }

    // Get type of accessed array.
    err = parse_get_variable(context, expression->children, tmpnode);
    if (err.type) { return err; }

    // Get size of base type of accessed array.
    Node *base_type_info = node_allocate();
    err = parse_get_type(context, tmpnode->children->next_child, base_type_info);
    if (err.type) { return err; }
    long long base_type_size = base_type_info->children->value.integer;
    free(base_type_info);

    long long offset = base_type_size * expression->value.integer;

    // Load memory address of beginning of array.
    expression->result_register = register_allocate(cg_context);
    fprintf(code, "lea %s, %s\n",
            symbol_to_address(cg_context, expression->children),
            register_name(expression->result_register));
    // Offset memory address by index.
    if (offset) {
      femit_x86_64(cg_context, INSTRUCTION_X86_64_ADD, IMMEDIATE_TO_REGISTER,
                   offset, expression->result_register);
    }
    break;
  case NODE_TYPE_IF:
    if (codegen_verbose) {
      fprintf(code, ";;#; If\n");
    }

    // Generate if condition expression code.
    err = codegen_expression_x86_64(cg_context,
                                    context, next_child_context,
                                    expression->children);
    if (err.type) { return err; }

    if (codegen_verbose) {
      fprintf(code, ";;#; If CONDITION\n");
    }

    // Generate code using result register from condition expression.
    char *otherwise_label = label_generate();
    char *after_otherwise_label = label_generate();
    const char *condition_register_name = register_name(expression->children->result_register);
    fprintf(code, "test %s, %s\n", condition_register_name, condition_register_name);
    fprintf(code, "jz %s\n", otherwise_label);
    register_deallocate(cg_context, expression->children->result_register);

    if (codegen_verbose) {
      fprintf(code, ";;#; If THEN\n");
    }

    // Enter if then body context
    ParsingContext *ctx = context;
    ParsingContext *next_child_ctx = *next_child_context;
    // FIXME: Should this NULL check create error rather than silently be allowed?
    if (next_child_context) {
      ctx = *next_child_context;
      next_child_ctx = ctx->children;
      *next_child_context = (*next_child_context)->next_child;

      //printf("Entered if context:\n");
      //parse_context_print(ctx, 0);
    }

    // Generate THEN expression body.
    Node *last_expr = NULL;
    Node *expr = expression->children->next_child->children;
    while (expr) {
      err = codegen_expression_x86_64(cg_context,
                                      ctx, &next_child_ctx,
                                      expr);
      if (err.type) { return err; }
      if (last_expr) {
        register_deallocate(cg_context, last_expr->result_register);
      }
      last_expr = expr;
      expr = expr->next_child;
    }

    // Generate code to copy last expr result register to if result register.
    expression->result_register = register_allocate(cg_context);
    femit_x86_64(cg_context, INSTRUCTION_X86_64_MOV, REGISTER_TO_REGISTER,
                 last_expr->result_register, expression->result_register);
    register_deallocate(cg_context, last_expr->result_register);
    fprintf(code, "jmp %s\n", after_otherwise_label);

    if (codegen_verbose) {
      fprintf(code, ";;#; If OTHERWISE\n");
    }

    // Generate OTHERWISE
    fprintf(code, "%s:\n", otherwise_label);

    last_expr = NULL;
    if (expression->children->next_child->next_child) {

      // Enter if otherwise body context
      ParsingContext *ctx = context;
      ParsingContext *next_child_ctx = *next_child_context;
      // FIXME: Should this NULL check create error rather than silently be allowed?
      if (next_child_context) {
        ctx = *next_child_context;
        next_child_ctx = ctx->children;
        *next_child_context = (*next_child_context)->next_child;

        //printf("Entered if else context:\n");
        //parse_context_print(ctx, 0);
      }

      expr = expression->children->next_child->next_child->children;
      while (expr) {
        err = codegen_expression_x86_64(cg_context,
                                        ctx, &next_child_ctx,
                                        expr);
        if (err.type) { return err; }
        if (last_expr) {
          register_deallocate(cg_context, last_expr->result_register);
        }
        last_expr = expr;
        expr = expr->next_child;
      }
      // Copy last_expr result register to if result register.
      if (last_expr) {
        femit_x86_64(cg_context, INSTRUCTION_X86_64_MOV, REGISTER_TO_REGISTER,
                     last_expr->result_register, expression->result_register);
        register_deallocate(cg_context, last_expr->result_register);
      }
    } else {
      femit_x86_64(cg_context, INSTRUCTION_X86_64_MOV, IMMEDIATE_TO_REGISTER,
                   (int64_t)0, expression->result_register);
    }

    fprintf(code, "%s:\n", after_otherwise_label);

    break;
  case NODE_TYPE_BINARY_OPERATOR:
    if (codegen_verbose) {
      fprintf(code, ";;#; Binary Operator: \"%s\"\n", expression->value.symbol);
    }
    while (context->parent) { context = context->parent; }
    // FIXME: Second argument is memory leaked! :^(
    environment_get(*context->binary_operators, node_symbol(expression->value.symbol), tmpnode);

    //printf("Codegenning binary operator %s\n", expression->value.symbol);
    //print_node(tmpnode,0);

    err = codegen_expression_x86_64(cg_context,
                                    context, next_child_context,
                                    expression->children);
    if (err.type) { return err; }
    err = codegen_expression_x86_64(cg_context,
                                    context, next_child_context,
                                    expression->children->next_child);
    if (err.type) { return err; }

    if (strcmp(expression->value.symbol, ">") == 0) {
      codegen_comparison_x86_64(cg_context, expression, COMPARE_GT);
    } else if (strcmp(expression->value.symbol, "<") == 0) {
      codegen_comparison_x86_64(cg_context, expression, COMPARE_LT);
    } else if (strcmp(expression->value.symbol, "=") == 0) {
      codegen_comparison_x86_64(cg_context, expression, COMPARE_EQ);
    } else if (strcmp(expression->value.symbol, "+") == 0) {
      // Plus/Addition
      // https://www.felixcloutier.com/x86/add

      // Use right hand side result register as our result since ADD is destructive!
      expression->result_register = expression->children->next_child->result_register;

      femit_x86_64
        (cg_context,
         INSTRUCTION_X86_64_ADD,
         expression->children->result_register,
         expression->children->next_child->result_register);

      // Free no-longer-used left hand side result register.
      register_deallocate(cg_context, expression->children->result_register);
    } else if (strcmp(expression->value.symbol, "-") == 0) {
      // Minus/Subtraction
      // https://www.felixcloutier.com/x86/sub

      // Use right hand side result register as our result since SUB is destructive!
      expression->result_register = expression->children->result_register;

      femit_x86_64(cg_context, INSTRUCTION_X86_64_SUB, REGISTER_TO_REGISTER,
                   expression->children->next_child->result_register,
                   expression->children->result_register);

      // Free no-longer-used left hand side result register.
      register_deallocate(cg_context, expression->children->next_child->result_register);
    } else if (strcmp(expression->value.symbol, "*") == 0) {
      // Multiply
      // https://www.felixcloutier.com/x86/mul
      // https://www.felixcloutier.com/x86/imul

      // Use right hand side result register as our result since ADD is destructive!
      expression->result_register = expression->children->next_child->result_register;

      femit_x86_64(cg_context, INSTRUCTION_X86_64_IMUL, REGISTER_TO_REGISTER,
                   expression->children->result_register,
                   expression->children->next_child->result_register);

      // Free no-longer-used left hand side result register.
      register_deallocate(cg_context, expression->children->result_register);
    } else if (strcmp(expression->value.symbol, "/") == 0
               || strcmp(expression->value.symbol, "%") == 0) {
      // Division/Modulo
      // https://www.felixcloutier.com/x86/div
      // https://www.felixcloutier.com/x86/idiv
      // https://www.felixcloutier.com/x86/cwd:cdq:cqo

      char modulo_flag = 0;
      // TODO: Don't compare twice!
      if (strcmp(expression->value.symbol, "%") == 0) {
        modulo_flag = 1;
      }

      // Quotient is in RAX, Remainder in RDX; we must save and
      // restore these registers before and after divide, sadly.

      // TODO: We can optimize the outputted code by testing if LHS
      // result register is RAX, in which case we can use it
      // destructively as our division expression result register.

      femit_x86_64(cg_context, INSTRUCTION_X86_64_PUSH,
                   REGISTER, REG_X86_64_RAX);
      femit_x86_64(cg_context, INSTRUCTION_X86_64_PUSH,
                   REGISTER, REG_X86_64_RDX);

      // Load RAX with left hand side of division operator, if needed.
      femit_x86_64(cg_context, INSTRUCTION_X86_64_MOV, REGISTER_TO_REGISTER,
                   expression->children->result_register, REG_X86_64_RAX);



      // Sign-extend the value in RAX to RDX. RDX is treated as the
      // 8 high bytes of a 16-byte number stored in RDX:RAX.
      fprintf(code, "cqto\n");

      // Call IDIV with right hand side of division operator.
      femit_x86_64(cg_context, INSTRUCTION_X86_64_IDIV,
                   REGISTER, expression->children->next_child->result_register);

      expression->result_register = register_allocate(cg_context);
      if (modulo_flag) {
        // Move return value from RDX into wherever it actually belongs.
        femit_x86_64(cg_context, INSTRUCTION_X86_64_MOV, REGISTER_TO_REGISTER,
                     REG_X86_64_RDX, expression->result_register);
      } else {
        // Move return value from RAX into wherever it actually belongs.
        femit_x86_64(cg_context, INSTRUCTION_X86_64_MOV, REGISTER_TO_REGISTER,
                     REG_X86_64_RAX, expression->result_register);
      }

      fprintf(code,
              "pop %%rdx\n"
              "pop %%rax\n");

    } else if (strcmp(expression->value.symbol, "<<") == 0) {
      // Bitshift Left
      // https://www.felixcloutier.com/x86/sal:sar:shl:shr

      // Use left hand side result register as our result since SHL is destructive!
      expression->result_register = expression->children->result_register;

      fprintf(code,
              "push %%rcx\n"
              "mov %s, %%rcx\n"
              "sal %%cl, %s\n"
              "pop %%rcx\n",
              register_name(expression->children->next_child->result_register),
              register_name(expression->children->result_register));

      // Free no-longer-used right hand side result register.
      register_deallocate(cg_context, expression->children->next_child->result_register);
    } else if (strcmp(expression->value.symbol, ">>") == 0) {
      // Minus/Subtraction
      // https://www.felixcloutier.com/x86/sub

      // Use left hand side result register as our result since SHR is destructive!
      expression->result_register = expression->children->result_register;

      fprintf(code,
              "push %%rcx\n"
              "mov %s, %%rcx\n"
              "sar %%cl, %s\n"
              "pop %%rcx\n",
              register_name(expression->children->next_child->result_register),
              register_name(expression->children->result_register));

      // Free no-longer-used right hand side result register.
      register_deallocate(cg_context, expression->children->next_child->result_register);
    } else {
      fprintf(stderr, "Unrecognized binary operator: \"%s\"\n", expression->value.symbol);
      ERROR_PREP(err, ERROR_GENERIC, "codegen_expression_x86_64() does not recognize binary operator");
      return err;
    }
    break;
  case NODE_TYPE_VARIABLE_ACCESS:
    if (codegen_verbose) {
      fprintf(code, ";;#; Variable Access: \"%s\"\n", expression->value.symbol);
    }
    expression->result_register = register_allocate(cg_context);

    // Find context that local variable resides in.

    CodegenContext *variable_residency = cg_context;
    while (variable_residency) {
      if (environment_get_by_symbol(*variable_residency->locals, expression->value.symbol, tmpnode)) {
        break;
      }
      variable_residency = variable_residency->parent;
    }
    if (!variable_residency) {
      // Global variable
      fprintf(code, "mov %s(%%rip), %s\n",
              expression->value.symbol,
              register_name(expression->result_register));
    } else {
      // TODO: For each context change upwards (base pointer load), emit a call to load caller RBP
      // from current RBP into some register, and use that register as offset for memory access.
      // This will require us to differentiate scopes from stack frames, which is a problem for
      // another time :^). Good luck, future me!
      fprintf(code, "mov %lld(%%rbp), %s\n",
              tmpnode->value.integer,
              register_name(expression->result_register));
    }
    break;
  case NODE_TYPE_VARIABLE_DECLARATION:
    if (!cg_context->parent) { break; }
    if (codegen_verbose) {
      fprintf(code, ";;#; Variable Declaration: \"%s\"\n", expression->children->value.symbol);
    }
    // Allocate space on stack
    //   Get the size in bytes of the type of the variable
    long long size_in_bytes = 0;
    while (context) {
      if (environment_get(*context->variables, expression->children, tmpnode)) {
        break;
      }
      context = context->parent;
    }
    if (!context) {
      printf("Variable Symbol: \"%s\"\n", expression->children->value.symbol);
      ERROR_PREP(err, ERROR_GENERIC, "Invalid AST/context fed to codegen. Could not find variable declaration in environment");
      return err;
    }
    if (strcmp(tmpnode->value.symbol, "external function") == 0) {
      break;
    }
    // Get size in bytes from types environment.
    err = parse_get_type(original_context, tmpnode, tmpnode);
    if (err.type) { return err; }
    size_in_bytes = tmpnode->children->value.integer;

    // TODO: Optimize to subtract all local variable's stack size at
    // beginning of function rather than throughout.
    //   Subtract type size in bytes from stack pointer
    fprintf(code, "sub $%lld, %%rsp\n", size_in_bytes);
    // Keep track of RBP offset.
    cg_context->locals_offset -= size_in_bytes;
    //   Kept in codegen context.
    environment_set(cg_context->locals, expression->children, node_integer(cg_context->locals_offset));
    break;
  case NODE_TYPE_VARIABLE_REASSIGNMENT:
    if (codegen_verbose) {
      fprintf(code, ";;#; Variable Reassignment\n");
    }

    // TODO: This whole section of code is pretty non-radical, dudes.

    // 1. Codegen RHS
    // 2. Recurse LHS into children until LHS is a var. access.

    // Set iterator to the var. access node.
    iterator = expression->children;
    while (iterator && iterator->type != NODE_TYPE_VARIABLE_ACCESS) {
      iterator = iterator->children;
    }
    if (!iterator) {
      // TODO: Error here for invalid or mishapen AST.
    }

    // Codegen RHS
    err = codegen_expression_x86_64(cg_context, context, next_child_context,
                                    expression->children->next_child);
    if (err.type) { break; }

    // When non-zero, de-allocate LHS register and free result string.
    char should_free_result = 0;

    // Set `result` to operand representing LHS that will be written into.
    if (expression->children->type == NODE_TYPE_VARIABLE_ACCESS) {
      result = symbol_to_address(cg_context, expression->children);
    } else {
      should_free_result = 1;
      // Codegen LHS
      err = codegen_expression_x86_64(cg_context, context, next_child_context,
                                      expression->children);
      if (err.type) { break; }
      const char *name = register_name(expression->children->result_register);
      // Put parenthesis around `result`.
      size_t needed_len = strlen(name) + 2;
      char *result_copy = strdup(name);
      result = malloc(needed_len + 1);
      snprintf(result, needed_len + 1, "(%s)", result_copy);
      result[needed_len] = '\0';
      free(result_copy);
    }
    fprintf(code, "mov %s, %s\n",
            register_name( expression->children->next_child->result_register),
            result);
    register_deallocate(cg_context, expression->children->next_child->result_register);

    if (should_free_result) {
      free(result);
      register_deallocate(cg_context, expression->children->result_register);
    }

    break;
  }

  ASSERT(expression->result_register != -1,
         "Result register of expression not set. Likely an internal error during codegen.");

  free(tmpnode);
  return err;
}

const char *function_header_x86_64 =
  "push %rbp\n"
  "mov %rsp, %rbp\n"
  "sub $32, %rsp\n";
const char *function_footer_x86_64 =
  "pop %rbp\n"
  "ret\n";
Error codegen_function_x86_64_gas
(CodegenContext *cg_context,
 ParsingContext *context,
 ParsingContext **next_child_context,
 char *name,
 Node *function
 )
{
  Error err = ok;
  FILE *code = cg_context->code;

  cg_context = create_codegen_context(cg_context);

  // Store base pointer integer offset within locals environment
  // Start at one to make space for pushed RBP in function header.
  size_t param_count = 1;
  Node *parameter = function->children->next_child->children;
  while (parameter) {
    param_count++;
    // Bind parameter name to integer base pointer offset.
    // FIXME: Assume each argument is 8 bytes for now.
    // TODO: This currently doesn't allow for passing arguments in registers, which is much faster.
    //       We need some local binding that refers to a register vs a base pointer offset.
    environment_set(cg_context->locals, parameter->children, node_integer(param_count * 8));
    parameter = parameter->next_child;
  }

  // Nested function execution protection
  fprintf(code, "jmp after%s\n", name);
  // Function beginning label
  fprintf(code, "%s:\n", name);

  // Function header
  fprintf(code, "%s", function_header_x86_64);

  // Function body
  ParsingContext *ctx = context;
  ParsingContext *next_child_ctx = *next_child_context;
  // FIXME: Should this NULL check create error rather than silently be allowed?
  if (next_child_context) {
    ctx = *next_child_context;
    next_child_ctx = ctx->children;
    *next_child_context = (*next_child_context)->next_child;

    //printf("Entered function context:\n");
    //parse_context_print(ctx, 0);
  }

  Node *last_expression = NULL;
  Node *expression = function->children->next_child->next_child->children;
  while (expression) {
    err = codegen_expression_x86_64(cg_context, ctx, &next_child_ctx, expression);
    register_deallocate(cg_context, expression->result_register);
    if (err.type) {
      print_error(err);
      return err;
    }
    last_expression = expression;
    expression = expression->next_child;
  }

  // Copy last expression result register to RAX
  if (last_expression) {
    if (last_expression->result_register != REG_X86_64_RAX) {
      const char *name = register_name(last_expression->result_register);
      fprintf(code, "mov %s, %%rax\n", name);
    }
  }

  // Function footer
  fprintf(code, "add $%lld, %%rsp\n", -cg_context->locals_offset);
  fprintf(code, "%s", function_footer_x86_64);

  // Nested function execution jump label
  fprintf(code, "after%s:\n", name);
  // after<function_label>:

  // Free context;
  codegen_context_x86_64_mswin_free(cg_context);
  return ok;
}

Error codegen_program_x86_64(CodegenContext *cg_context, ParsingContext *context, Node *program) {
  Error err = ok;
  FILE *code = cg_context->code;

  fprintf(code, "%s", ".section .data\n");

  // Generate global variables
  Binding *var_it = context->variables->bind;
  Node *type_info = node_allocate();
  while (var_it) {
    Node *var_id = var_it->id;
    Node *type_id = node_allocate();
    *type_id = *var_it->value;
    // Do not emit "external" typed variables.
    // TODO: Probably should have external attribute rather than this nonsense!
    if (strcmp(type_id->value.symbol, "external function") != 0) {
      err = parse_get_type(context, type_id, type_info);
      if (err.type) {
        print_node(type_id, 0);
        return err;
      }
      fprintf(code, "%s: .space %lld\n", var_id->value.symbol, type_info->children->value.integer);
    }
    var_it = var_it->next;
  }
  free(type_info);

  fprintf(code,
          ".section .text\n"
          ".global main\n"
          "main:\n"
          "%s", function_header_x86_64);

  ParsingContext *next_child_context = context->children;
  Node *last_expression = program->children;
  Node *expression = program->children;
  while (expression) {
    if (nonep(*expression)) {
      expression = expression->next_child;
      continue;
    }
    err = codegen_expression_x86_64(cg_context, context, &next_child_context, expression);
    if (err.type) { return err; }
    register_deallocate(cg_context, expression->result_register);
    last_expression = expression;
    expression = expression->next_child;
  }

  // Copy last expression into RAX register for return value.
  if (last_expression->result_register != REG_X86_64_RAX) {
    const char *name = register_name(last_expression->result_register);
    fprintf(code, "mov %s, %%rax\n", name);
  }

  fprintf(code, "add $%lld, %%rsp\n", -cg_context->locals_offset);
  fprintf(code, "%s", function_footer_x86_64);

  return err;
}

//================================================================ END CG_FMT_x86_64_MSWIN

Error codegen_program
(enum CodegenOutputFormat format,
 enum CodegenCallingConvention call_convention,
 char *filepath,
 ParsingContext *context,
 Node *program
 )
{
  Error err = ok;
  if (!filepath) {
    ERROR_PREP(err, ERROR_ARGUMENTS, "codegen_program(): filepath can not be NULL!");
    return err;
  }
  // Open file for writing.
  FILE *code = fopen(filepath, "w");
  if (!code) {
    printf("Filepath: \"%s\"\n", filepath);
    ERROR_PREP(err, ERROR_GENERIC, "codegen_program(): fopen failed to open file at path.");
    return err;
  }
  if (format == CG_FMT_x86_64_GAS) {
    // TODO: Handle call_convention for creating codegen context!
    CodegenContext *cg_context;
    if (call_convention == CG_CALL_CONV_MSWIN) {
      cg_context = codegen_context_x86_64_gas_mswin_create(NULL);
      cg_context->code = code;
    } else if (call_convention == CG_CALL_CONV_LINUX) {
      // TODO: Create codegen context for GAS linux assembly.
      ERROR_PREP(err, ERROR_TODO, "Create codegen context for GAS linux x86_64 assembly.");
      return err;
    } else {
      ERROR_PREP(err, ERROR_GENERIC, "Unrecognized calling convention!");
      return err;
    }
    err = codegen_program_x86_64(cg_context, context, program);
    codegen_context_x86_64_mswin_free(cg_context);
  } else {
    printf("ERROR: Unrecognized codegen format\n");
  }
  fclose(code);
  return err;
}
