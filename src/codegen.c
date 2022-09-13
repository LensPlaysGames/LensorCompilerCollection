#include <codegen.h>

#include <assert.h>
#include <environment.h>
#include <error.h>
#include <parser.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Each platform must have registers defined, obviously.
// Scratch registers MUST come first in enumeration.
// Values on or between zero and maximum MUST be a valid register
// descriptor.
enum ScratchRegisters_X86_64_MSWIN {
  // Scratch Registers
  REG_X86_64_MSWIN_RAX,
  REG_X86_64_MSWIN_RCX,
  REG_X86_64_MSWIN_RDX,
  REG_X86_64_MSWIN_R8,
  REG_X86_64_MSWIN_R9,
  REG_X86_64_MSWIN_R10,
  REG_X86_64_MSWIN_R11,

  // Non-scratch Registers XD
  REG_X86_64_MSWIN_R12,
  REG_X86_64_MSWIN_R13,
  REG_X86_64_MSWIN_R14,
  REG_X86_64_MSWIN_R15,
  REG_X86_64_MSWIN_RBX,
  REG_X86_64_MSWIN_RSI,
  REG_X86_64_MSWIN_RDI,
  REG_X86_64_MSWIN_RBP,
  REG_X86_64_MSWIN_RSP,
  REG_X86_64_MSWIN_RIP,

  REG_X86_64_MSWIN_SCRATCH = REG_X86_64_MSWIN_R11 + 1,
  REG_X86_64_MSWIN_COUNT = REG_X86_64_MSWIN_RIP + 1
};

#define INIT_REGISTER(registers, desc, reg_name)                        \
  ((registers)[desc] = (Register){.name = (reg_name), .in_use = 0, .descriptor = (desc)})

/// Creates a context for the CG_FMT_x86_64_MSWIN architecture.
CodegenContext *codegen_context_x86_64_mswin_create(CodegenContext *parent) {
  RegisterPool pool;

  // If this is the top level context, create the registers.
  // Otherwise, shallow copy register pool to child context.
  if (!parent) {
    Register *registers = calloc(REG_X86_64_MSWIN_COUNT, sizeof(Register));
    INIT_REGISTER(registers, REG_X86_64_MSWIN_RAX, "%rax");
    INIT_REGISTER(registers, REG_X86_64_MSWIN_RCX, "%rcx");
    INIT_REGISTER(registers, REG_X86_64_MSWIN_RDX, "%rdx");
    INIT_REGISTER(registers, REG_X86_64_MSWIN_R8, "%r8");
    INIT_REGISTER(registers, REG_X86_64_MSWIN_R9, "%r9");
    INIT_REGISTER(registers, REG_X86_64_MSWIN_R10, "%r10");
    INIT_REGISTER(registers, REG_X86_64_MSWIN_R11, "%r11");

    INIT_REGISTER(registers, REG_X86_64_MSWIN_R12, "%r12");
    INIT_REGISTER(registers, REG_X86_64_MSWIN_R13, "%r13");
    INIT_REGISTER(registers, REG_X86_64_MSWIN_R14, "%r14");
    INIT_REGISTER(registers, REG_X86_64_MSWIN_R15, "%r15");
    INIT_REGISTER(registers, REG_X86_64_MSWIN_RBX, "%rbx");
    INIT_REGISTER(registers, REG_X86_64_MSWIN_RSI, "%rsi");
    INIT_REGISTER(registers, REG_X86_64_MSWIN_RDI, "%rdi");
    INIT_REGISTER(registers, REG_X86_64_MSWIN_RBP, "%rbp");
    INIT_REGISTER(registers, REG_X86_64_MSWIN_RSP, "%rsp");
    INIT_REGISTER(registers, REG_X86_64_MSWIN_RIP, "%rip");

    pool = (RegisterPool) {
      .regs = registers,
      .num_scratch_regs = REG_X86_64_MSWIN_SCRATCH,
      .num_regs = REG_X86_64_MSWIN_COUNT,
    };
  } else {
    pool = parent->registers;
  }

  CodegenContext *cg_ctx = calloc(1,sizeof(CodegenContext));
  cg_ctx->parent = parent;
  cg_ctx->registers = pool;
  cg_ctx->locals = environment_create(NULL);
  cg_ctx->locals_offset = -32;
  return cg_ctx;
}

#undef INIT_REGISTER

/// Free a context created by codegen_context_x86_64_mswin_create.
void codegen_context_x86_64_mswin_free(CodegenContext *ctx) {
  // Only free the registers if this is the top-level context.
  if (!ctx->parent) free(ctx->registers.regs);
  // TODO(sirraide): Free environment.
  free(ctx);
}

//================================================================ BEG REGISTER STUFF

void print_registers(CodegenContext *cg_ctx) {
  for (RegisterDescriptor d = 0; d < cg_ctx->registers.num_regs; ++d) {
    Register *reg = &cg_ctx->registers.regs[d];
    printf("%s:%i\n", reg->name, reg->in_use);
  }
}

char register_descriptor_is_valid(CodegenContext *cg_ctx, RegisterDescriptor descriptor) {
  return descriptor >= 0 && descriptor < cg_ctx->registers.num_regs;
}

RegisterDescriptor register_allocate(CodegenContext *cg_ctx) {
  assert(cg_ctx->registers.num_regs > 0 && cg_ctx->registers.num_scratch_regs > 0 && "Register pool is empty");

  for (RegisterDescriptor d = 0; d < cg_ctx->registers.num_scratch_regs; ++d) {
    Register *reg = &cg_ctx->registers.regs[d];
    if (reg->in_use == 0) {
      reg->in_use = 1;
      return reg->descriptor;
    }
  }
  panic("ERROR::register_allocate(): Could not allocate register!\n");
}

void register_deallocate
(CodegenContext *cg_ctx, RegisterDescriptor descriptor) {
  if (!register_descriptor_is_valid(cg_ctx, descriptor)) {
    panic("ERROR::register_deallocate(): Invalid register descriptor!\n");
  }
  cg_ctx->registers.regs[descriptor].in_use = 0;
}

const char *register_name
(CodegenContext *cg_ctx, RegisterDescriptor descriptor) {
  if (!register_descriptor_is_valid(cg_ctx, descriptor)) {
    panic("ERROR::register_name(): Could not find register with descriptor of %d\n", descriptor);
  }
  return cg_ctx->registers.regs[descriptor].name;
}

//================================================================ END REGISTER STUFF

#define label_buffer_size 1024
char label_buffer[label_buffer_size];
size_t label_index = 0;
size_t label_count = 0;
char *label_generate() {
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

//================================================================ BEG CG_FMT_x86_64_MSWIN

// TODO/FIXME: Make this a parameter affectable by command line arguments.
char codegen_verbose = 1;

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

// Forward declare codegen_function for codegen_expression
Error codegen_function_x86_64_att_asm_mswin
(CodegenContext *cg_context,
 ParsingContext *context,
 ParsingContext **next_child_context,
 char *name,
 Node *function,
 FILE *code);

Error codegen_expression_x86_64_mswin
(FILE *code,
 CodegenContext *cg_context,
 ParsingContext *context,
 ParsingContext **next_child_context,
 Node *expression
 )
{
  Error err = ok;
  char *result = NULL;
  Node *tmpnode = node_allocate();
  Node *iterator = NULL;
  long long count = 0;

  ParsingContext *original_context = context;
  //expression->result_register = -1;

  assert(NODE_TYPE_MAX == 13 && "codegen_expression_x86_64_mswin() must exhaustively handle node types!");
  switch (expression->type) {
  default:
    break;
  case NODE_TYPE_INTEGER:
    if (codegen_verbose) {
      fprintf(code, ";;#; INTEGER: %lld\n", expression->value.integer);
    }
    expression->result_register = register_allocate(cg_context);
    fprintf(code, "mov $%lld, %s\n",
            expression->value.integer,
            register_name(cg_context, expression->result_register));
    break;
  case NODE_TYPE_FUNCTION_CALL:
    if (codegen_verbose) {
      fprintf(code, ";;#; Function Call: \"%s\"\n", expression->children->value.symbol);
    }

    // TODO: Should we technically save all in-use scratch registers?
    // Save RAX because function call will over-write it!
    fprintf(code, "pushq %%rax\n");

    // Push arguments *in reverse order* on to the stack.
    // TODO: In reverse order. Or just calculate same when accessing.
    iterator = expression->children->next_child->children;
    while (iterator) {
      err = codegen_expression_x86_64_mswin
        (code, cg_context, context, next_child_context, iterator);
      if (err.type) { return err; }
      fprintf(code, "pushq %s\n", register_name(cg_context, iterator->result_register));
      register_deallocate(cg_context, iterator->result_register);
      iterator = iterator->next_child;
      count++;
    }

    err = codegen_expression_x86_64_mswin(code, cg_context, context, next_child_context, expression->children);
    if (err.type) { return err; }

    // Emit call
    fprintf(code, "call *%s\n", register_name(cg_context, expression->children->result_register));
    register_deallocate(cg_context, expression->children->result_register);
    if (count) {
      fprintf(code, "add $%lld, %%rsp\n", count * 8);
    }

    // Copy return value of function call from RAX to result register
    expression->result_register = register_allocate(cg_context);
    if (expression->result_register != REG_X86_64_MSWIN_RAX) {
      fprintf(code, "mov %%rax, %s\n", register_name(cg_context, expression->result_register));
      // Save overwritten in-use registers.
      fprintf(code, "pop %%rax\n");
    } else {
      fprintf(code, "add $8, %%rsp\n");
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
    err = codegen_function_x86_64_att_asm_mswin(cg_context,
                                                context, next_child_context,
                                                result, expression, code);

    // Function returns beginning of instructions address.
    expression->result_register = register_allocate(cg_context);
    fprintf(code, "lea %s(%%rip), %s\n",
            result,
            register_name(cg_context, expression->result_register));
    break;
  case NODE_TYPE_DEREFERENCE:
    if (codegen_verbose) {
      fprintf(code, ";;#; Dereference\n");
    }
    err = codegen_expression_x86_64_mswin(code, cg_context,
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
            register_name(cg_context, expression->result_register));
    if (err.type) { return err; }
    break;
  case NODE_TYPE_IF:
    if (codegen_verbose) {
      fprintf(code, ";;#; If\n");
    }

    // Generate if condition expression code.
    err = codegen_expression_x86_64_mswin(code, cg_context,
                                          context, next_child_context,
                                          expression->children);
    if (err.type) { return err; }

    if (codegen_verbose) {
      fprintf(code, ";;#; If CONDITION\n");
    }

    // Generate code using result register from condition expression.
    char *otherwise_label = label_generate();
    char *after_otherwise_label = label_generate();
    const char *condition_register_name = register_name(cg_context, expression->children->result_register);
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
      err = codegen_expression_x86_64_mswin(code, cg_context,
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
    fprintf(code, "mov %s, %s\n",
            register_name(cg_context, last_expr->result_register),
            register_name(cg_context, expression->result_register));
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
        err = codegen_expression_x86_64_mswin(code, cg_context,
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
        fprintf(code, "mov %s, %s\n",
                register_name(cg_context, last_expr->result_register),
                register_name(cg_context, expression->result_register));
        register_deallocate(cg_context, last_expr->result_register);
      }
    } else {
      fprintf(code, "mov $0, %s\n", register_name(cg_context, expression->result_register));
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

    err = codegen_expression_x86_64_mswin(code, cg_context,
                                          context, next_child_context,
                                          expression->children);
    if (err.type) { return err; }
    err = codegen_expression_x86_64_mswin(code, cg_context,
                                          context, next_child_context,
                                          expression->children->next_child);
    if (err.type) { return err; }

    if (strcmp(expression->value.symbol, ">") == 0) {
      // Greater than
      // https://www.felixcloutier.com/x86/cmovcc

      expression->result_register = register_allocate(cg_context);
      RegisterDescriptor true_register = register_allocate(cg_context);

      fprintf(code, "mov $0, %s\n", register_name(cg_context, expression->result_register));
      fprintf(code, "mov $1, %s\n", register_name(cg_context, true_register));
      fprintf(code, "cmp %s, %s\n"
              , register_name(cg_context, expression->children->next_child->result_register)
              , register_name(cg_context, expression->children->result_register));
      fprintf(code, "cmovg %s, %s\n",
              register_name(cg_context, true_register),
              register_name(cg_context, expression->result_register));

      // Free no-longer-used left hand side result register.
      register_deallocate(cg_context, true_register);
      register_deallocate(cg_context, expression->children->result_register);
      register_deallocate(cg_context, expression->children->next_child->result_register);
    } else if (strcmp(expression->value.symbol, "<") == 0) {
      // Less than
      // https://www.felixcloutier.com/x86/cmovcc

      expression->result_register = register_allocate(cg_context);
      RegisterDescriptor true_register = register_allocate(cg_context);

      fprintf(code, "mov $0, %s\n", register_name(cg_context, expression->result_register));
      fprintf(code, "mov $1, %s\n", register_name(cg_context, true_register));
      fprintf(code, "cmp %s, %s\n"
              , register_name(cg_context, expression->children->next_child->result_register)
              , register_name(cg_context, expression->children->result_register)
              );
      fprintf(code, "cmovl %s, %s\n",
              register_name(cg_context, true_register),
              register_name(cg_context, expression->result_register));

      // Free no-longer-used left hand side result register.
      register_deallocate(cg_context, true_register);
      register_deallocate(cg_context, expression->children->result_register);
      register_deallocate(cg_context, expression->children->next_child->result_register);
    } else if (strcmp(expression->value.symbol, "=") == 0) {
      // Equality
      // https://www.felixcloutier.com/x86/cmovcc

      expression->result_register = register_allocate(cg_context);
      RegisterDescriptor true_register = register_allocate(cg_context);

      fprintf(code, "mov $0, %s\n", register_name(cg_context, expression->result_register));
      fprintf(code, "mov $1, %s\n", register_name(cg_context, true_register));
      fprintf(code, "cmp %s, %s\n",
              register_name(cg_context, expression->children->result_register),
              register_name(cg_context, expression->children->next_child->result_register));
      fprintf(code, "cmove %s, %s\n",
              register_name(cg_context, true_register),
              register_name(cg_context, expression->result_register));

      // Free no-longer-used left hand side result register.
      register_deallocate(cg_context, true_register);
      register_deallocate(cg_context, expression->children->result_register);
      register_deallocate(cg_context, expression->children->next_child->result_register);
    } else if (strcmp(expression->value.symbol, "+") == 0) {
      // Plus/Addition
      // https://www.felixcloutier.com/x86/add

      // Use right hand side result register as our result since ADD is destructive!
      expression->result_register = expression->children->next_child->result_register;

      fprintf(code, "add %s, %s\n",
              register_name(cg_context, expression->children->result_register),
              register_name(cg_context, expression->children->next_child->result_register));

      // Free no-longer-used left hand side result register.
      register_deallocate(cg_context, expression->children->result_register);
    } else if (strcmp(expression->value.symbol, "-") == 0) {
      // Minus/Subtraction
      // https://www.felixcloutier.com/x86/sub

      // Use right hand side result register as our result since SUB is destructive!
      expression->result_register = expression->children->result_register;

      fprintf(code, "sub %s, %s\n",
              register_name(cg_context, expression->children->next_child->result_register),
              register_name(cg_context, expression->children->result_register));

      // Free no-longer-used left hand side result register.
      register_deallocate(cg_context, expression->children->next_child->result_register);
    } else if (strcmp(expression->value.symbol, "*") == 0) {
      // Multiply
      // https://www.felixcloutier.com/x86/mul
      // https://www.felixcloutier.com/x86/imul

      // Use right hand side result register as our result since ADD is destructive!
      expression->result_register = expression->children->next_child->result_register;

      fprintf(code, "imul %s, %s\n",
              register_name(cg_context, expression->children->result_register),
              register_name(cg_context, expression->children->next_child->result_register));

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

      fprintf(code,
              "push %%rax\n"
              "push %%rdx\n");

      // Load RAX with left hand side of division operator, if needed.
      RegisterDescriptor result_register = expression->children->result_register;
      if (result_register != REG_X86_64_MSWIN_RAX) {
        const char *lhs_register_name = register_name(cg_context, result_register);
        // TODO: If RHS is in RAX, we must save RAX first...
        fprintf(code, "mov %s, %%rax\n", lhs_register_name);
      }

      // Sign-extend the value in RAX to RDX.
      // RDX is treated as the 8 high bytes of a 16-byte
      // number stored in RDX:RAX.
      fprintf(code, "cqto\n");

      // Call IDIV with right hand side of division operator.
      fprintf(code,
              "idiv %s\n",
              register_name(cg_context, expression->children->next_child->result_register));

      expression->result_register = register_allocate(cg_context);
      const char *result_register_name = register_name(cg_context, expression->result_register);
      if (modulo_flag) {
        // Move return value from RDX into wherever it actually belongs.
        fprintf(code, "mov %%rdx, %s\n", result_register_name);
      } else {
        // Move return value from RAX into wherever it actually belongs.
        if (expression->result_register != REG_X86_64_MSWIN_RAX) {
          fprintf(code, "mov %%rax, %s\n", result_register_name);
        }
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
              register_name(cg_context, expression->children->next_child->result_register),
              register_name(cg_context, expression->children->result_register));

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
              register_name(cg_context, expression->children->next_child->result_register),
              register_name(cg_context, expression->children->result_register));

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
              register_name(cg_context, expression->result_register));
    } else {
      // TODO: For each context change upwards (base pointer load), emit a call to load caller RBP
      // from current RBP into some register, and use that register as offset for memory access.
      // This will require us to differentiate scopes from stack frames, which is a problem for
      // another time :^). Good luck, future me!
      fprintf(code, "mov %lld(%%rbp), %s\n",
              tmpnode->value.integer,
              register_name(cg_context, expression->result_register));
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
    // Get size in bytes from types environment.
    err = parse_get_type(original_context, tmpnode, tmpnode);
    if (err.type) { return err; }
    size_in_bytes = tmpnode->children->value.integer;

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
    err = codegen_expression_x86_64_mswin(code, cg_context, context, next_child_context,
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
      err = codegen_expression_x86_64_mswin(code, cg_context, context, next_child_context,
                                            expression->children);
      if (err.type) { break; }
      const char *name = register_name(cg_context, expression->children->result_register);
      // Put parenthesis around `result`.
      size_t needed_len = strlen(name) + 2;
      char *result_copy = strdup(name);
      result = malloc(needed_len + 1);
      snprintf(result, needed_len + 1, "(%s)", result_copy);
      result[needed_len] = '\0';
      free(result_copy);
    }
    fprintf(code, "mov %s, %s\n",
            register_name(cg_context,  expression->children->next_child->result_register),
            result);
    register_deallocate(cg_context, expression->children->next_child->result_register);

    if (should_free_result) {
      free(result);
      register_deallocate(cg_context, expression->children->result_register);
    }

    break;
  }

  if (expression->result_register == -1) {
    printf("Mishandled expression type %i in codegen_expression_x86_64_mswin()\n", expression->type);
  }
  //assert(expression->result_register != -1 &&
  //      "Result register of expression not set. Likely an internal error during codegen.");

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
Error codegen_function_x86_64_att_asm_mswin
(CodegenContext *cg_context,
 ParsingContext *context,
 ParsingContext **next_child_context,
 char *name,
 Node *function,
 FILE *code
 )
{
  Error err = ok;

  cg_context = codegen_context_x86_64_mswin_create(cg_context);

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
    err = codegen_expression_x86_64_mswin(code, cg_context, ctx, &next_child_ctx, expression);
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
    if (last_expression->result_register != REG_X86_64_MSWIN_RAX) {
      const char *name = register_name(cg_context,last_expression->result_register);
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

Error codegen_program_x86_64_mswin(FILE *code, CodegenContext *cg_context, ParsingContext *context, Node *program) {
  Error err = ok;

  fprintf(code, "%s", ".section .data\n");

  // Generate global variables
  Binding *var_it = context->variables->bind;
  Node *type_info = node_allocate();
  while (var_it) {
    Node *var_id = var_it->id;
    Node *type_id = node_allocate();
    *type_id = *var_it->value;
    type_id->children = NULL;
    type_id->next_child = NULL;
    err = parse_get_type(context, type_id, type_info);
    if (err.type) {
      print_node(type_id, 0);
      return err;
    }
    var_it = var_it->next;
    fprintf(code, "%s: .space %lld\n", var_id->value.symbol, type_info->children->value.integer);
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
    err = codegen_expression_x86_64_mswin(code, cg_context, context, &next_child_context, expression);
    if (err.type) { return err; }
    register_deallocate(cg_context, expression->result_register);
    last_expression = expression;
    expression = expression->next_child;
  }

  // Copy last expression into RAX register for return value.
  if (last_expression->result_register != REG_X86_64_MSWIN_RAX) {
    const char *name = register_name(cg_context, last_expression->result_register);
    fprintf(code, "mov %s, %%rax\n", name);
  }

  fprintf(code, "add $%lld, %%rsp\n", -cg_context->locals_offset);
  fprintf(code, "%s", function_footer_x86_64);

  return err;
}

//================================================================ END CG_FMT_x86_64_MSWIN

Error codegen_program
(enum CodegenOutputFormat format,
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
  if (format == CG_FMT_x86_64_MSWIN) {
    CodegenContext *cg_context = codegen_context_x86_64_mswin_create(NULL);
    err = codegen_program_x86_64_mswin(code, cg_context, context, program);
    codegen_context_x86_64_mswin_free(cg_context);
  } else {
    printf("ERROR: Unrecognized codegen format\n");
  }
  fclose(code);
  return err;
}
