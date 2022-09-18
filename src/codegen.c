#include <codegen.h>
#include <codegen/codegen_platforms.h>

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
//================================================================ BEG REGISTER STUFF

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

/// The address of a local or global symbol, or an error
/// indicating why the symbol could not be found.
typedef struct symbol_address {
  enum {
    /// Global variable. The address is in `global`.
    SYMBOL_ADDRESS_MODE_GLOBAL,
    /// Local variable. The address is in `local`.
    SYMBOL_ADDRESS_MODE_LOCAL,
    /// There was an error. The error is in `error`.
    SYMBOL_ADDRESS_MODE_ERROR,
  } mode;
  union {
    Error error;
    const char *global;
    long long int local;
  };
} symbol_address;

symbol_address symbol_to_address(CodegenContext *cg_ctx, Node *symbol) {
  ASSERT(cg_ctx, "symbol_to_address(): Context must not be NULL (pass global).");
  ASSERT(symbol && symbol->value.symbol, "symbol_to_address(): A symbol must be passed.");

  // Global variable access.
  if (!cg_ctx->parent) {
    return (symbol_address) {
        .mode = SYMBOL_ADDRESS_MODE_GLOBAL,
        .global = symbol->value.symbol,
    };
  }

  // Local variable access.
  Node *stack_offset = node_allocate();
  if (!environment_get(*cg_ctx->locals, symbol, stack_offset)) {
    putchar('\n');
    print_node(symbol,0);
    environment_print(*cg_ctx->locals, 0);

    // FIXME(Sirraide): This is ugly. Should this be heap-allocated?
    //   Maybe we should have a way to store a heap allocated string in
    //   an `Error`? I would recommend either adding a flag indicating
    //   that the messages needs to be free()’d or just leaking the
    //   memory since we're probably going to terminate anyway if there
    //   was an error.
    static char err_buf[1024];
    snprintf(err_buf, sizeof err_buf, "symbol_to_address(): Could not find symbol '%s' in environment.", symbol->value.symbol);
    ERROR_CREATE(err, ERROR_GENERIC, err_buf);
    return (symbol_address) {
      .mode = SYMBOL_ADDRESS_MODE_ERROR,
      .error = err,
    };
  }

  long long int address = stack_offset->value.integer;
  free(stack_offset);
  return (symbol_address) {
    .mode = SYMBOL_ADDRESS_MODE_LOCAL,
    .local = address,
  };
}

// Forward declare codegen_function for codegen_expression
Error codegen_function
(CodegenContext *cg_context,
 ParsingContext *context,
 ParsingContext **next_child_context,
 char *name,
 Node *function);

Error codegen_expression
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
    expression->result_register = codegen_load_immediate(cg_context, expression->value.integer);
    break;
  case NODE_TYPE_FUNCTION_CALL:
    if (codegen_verbose) {
      fprintf(code, ";;#; Function Call: \"%s\"\n", expression->children->value.symbol);
    }

    // TODO: Should we technically save all in-use scratch registers?
    // Save RAX because function call will over-write it!
    codegen_prepare_call(cg_context);

    // Setup function environment based on calling convention.

    Node *variable_type = node_allocate();
    // Use `typecheck_expression()` to get type of variable.
    // err is ignored purposefully, program already type-checked valid.
    typecheck_expression(context, NULL, expression->children, variable_type);

    iterator = expression->children->next_child->children;
    if (strcmp(variable_type->value.symbol, "external function") == 0) {
      // TODO: Save RCX, RDX, R8, and R9 (they are scratch registers).
      // TODO: Only save scratch registers that are in-use.

      // Put arguments in RCX, RDX, R8, R9, then on the stack in reverse order.
      while (iterator) {
        // Place first argument in RCX or XMM0
        err = codegen_expression(cg_context, context, next_child_context, iterator);
        if (err.type) { return err; }

        codegen_add_external_function_arg(cg_context, iterator->result_register);
        iterator = iterator->next_child;
      }

      // TODO: Reverse rest of arguments, push on stack.

      expression->result_register = codegen_perform_external_call(cg_context, expression->children->value.symbol);
    } else {
      // Push arguments on stack in order.
      while (iterator) {
        err = codegen_expression(cg_context, context, next_child_context, iterator);
        if (err.type) { return err; }
        codegen_add_internal_function_arg(cg_context, iterator->result_register);
        register_deallocate(cg_context, iterator->result_register);
        iterator = iterator->next_child;
      }

      err = codegen_expression(cg_context, context, next_child_context, expression->children);
      if (err.type) { return err; }

      // Emit call
      expression->result_register = codegen_perform_internal_call(cg_context, expression->children->result_register);
      register_deallocate(cg_context, expression->children->result_register);
    }

    codegen_cleanup_call(cg_context);

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
    err = codegen_function(cg_context,
                                      context, next_child_context,
                                      result, expression);

    // Function returns beginning of instructions address.
    expression->result_register = register_allocate(cg_context);
    codegen_load_global_address_into(cg_context, result, expression->result_register);
    /*femit_x86_64(cg_context, INSTRUCTION_X86_64_LEA, NAME_TO_REGISTER,
        REG_X86_64_RIP, result,
        expression->result_register);*/
    break;
  case NODE_TYPE_DEREFERENCE:
    if (codegen_verbose) {
      fprintf(code, ";;#; Dereference\n");
    }
    err = codegen_expression(cg_context,
                             context, next_child_context,
                             expression->children);
    if (err.type) { return err; }
    expression->result_register = expression->children->result_register;
    break;
  case NODE_TYPE_ADDRESSOF: {
    if (codegen_verbose) {
      fprintf(code, ";;#; Addressof\n");
    }

    symbol_address address = symbol_to_address(cg_context, expression->children);
    switch (address.mode) {
      case SYMBOL_ADDRESS_MODE_ERROR: return address.error;
      case SYMBOL_ADDRESS_MODE_GLOBAL:
        expression->result_register = codegen_load_global_address(cg_context, address.global);
        break;
      case SYMBOL_ADDRESS_MODE_LOCAL:
        expression->result_register = codegen_load_local_address(cg_context, address.local);
        break;
    }
    break;
  }
  case NODE_TYPE_INDEX: {
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
    symbol_address address = symbol_to_address(cg_context, expression->children);
    switch (address.mode) {
      case SYMBOL_ADDRESS_MODE_ERROR: return address.error;
      case SYMBOL_ADDRESS_MODE_GLOBAL:
        expression->result_register = codegen_load_global_address(cg_context, address.global);
        break;
      case SYMBOL_ADDRESS_MODE_LOCAL:
        expression->result_register = codegen_load_local_address(cg_context, address.local);
        break;
    }
    // Offset memory address by index.
    if (offset) {
      codegen_add_immediate(cg_context, expression->result_register, offset);
    }
    break;
  }
  case NODE_TYPE_IF:
    if (codegen_verbose) {
      fprintf(code, ";;#; If\n");
    }

    // Generate if condition expression code.
    err = codegen_expression(cg_context,
                             context, next_child_context,
                             expression->children);
    if (err.type) { return err; }

    if (codegen_verbose) {
      fprintf(code, ";;#; If CONDITION\n");
    }

    // Generate code using result register from condition expression.
    char *otherwise_label = label_generate();
    char *after_otherwise_label = label_generate();
    codegen_branch_if_zero(cg_context, expression->children->result_register, otherwise_label);
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
      err = codegen_expression(cg_context,
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
    codegen_copy_register(cg_context, last_expr->result_register, expression->result_register);
    register_deallocate(cg_context, last_expr->result_register);
    codegen_branch(cg_context, after_otherwise_label);

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
        err = codegen_expression(cg_context,
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
        codegen_copy_register(cg_context, last_expr->result_register, expression->result_register);
        register_deallocate(cg_context, last_expr->result_register);
      }
    } else {
      codegen_zero_register(cg_context, expression->result_register);
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

    err = codegen_expression(cg_context,
                             context, next_child_context,
                             expression->children);
    if (err.type) { return err; }
    err = codegen_expression(cg_context,
                             context, next_child_context,
                             expression->children->next_child);
    if (err.type) { return err; }

    if (strcmp(expression->value.symbol, ">") == 0) {
      expression->result_register = codegen_comparison(cg_context,
          CODEGEN_CLOBBER_OPERANDS, COMPARE_GT,
          expression->children->result_register,
          expression->children->next_child->result_register);
    } else if (strcmp(expression->value.symbol, "<") == 0) {
      expression->result_register = codegen_comparison(cg_context,
          CODEGEN_CLOBBER_OPERANDS, COMPARE_LT,
          expression->children->result_register,
          expression->children->next_child->result_register);
    } else if (strcmp(expression->value.symbol, "=") == 0) {
      expression->result_register = codegen_comparison(cg_context,
          CODEGEN_CLOBBER_OPERANDS, COMPARE_EQ,
          expression->children->result_register,
          expression->children->next_child->result_register);
    } else if (strcmp(expression->value.symbol, "+") == 0) {
      expression->result_register = codegen_add(cg_context,
          CODEGEN_CLOBBER_OPERANDS,
          expression->children->result_register,
          expression->children->next_child->result_register);
    } else if (strcmp(expression->value.symbol, "-") == 0) {
      expression->result_register = codegen_subtract(cg_context,
          CODEGEN_CLOBBER_OPERANDS,
          expression->children->result_register,
          expression->children->next_child->result_register);
    } else if (strcmp(expression->value.symbol, "*") == 0) {
      expression->result_register = codegen_multiply(cg_context,
          CODEGEN_CLOBBER_OPERANDS,
          expression->children->result_register,
          expression->children->next_child->result_register);
    } else if (strcmp(expression->value.symbol, "/") == 0) {
      expression->result_register = codegen_divide(cg_context,
          CODEGEN_CLOBBER_OPERANDS,
          expression->children->result_register,
          expression->children->next_child->result_register);
    } else if (strcmp(expression->value.symbol, "%") == 0) {
      expression->result_register = codegen_modulo(cg_context,
          CODEGEN_CLOBBER_OPERANDS,
          expression->children->result_register,
          expression->children->next_child->result_register);
    } else if (strcmp(expression->value.symbol, "<<") == 0) {
      expression->result_register = codegen_shift_left(cg_context,
          CODEGEN_CLOBBER_OPERANDS,
          expression->children->result_register,
          expression->children->next_child->result_register);
    } else if (strcmp(expression->value.symbol, ">>") == 0) {
      expression->result_register = codegen_shift_right_arithmetic(cg_context,
          CODEGEN_CLOBBER_OPERANDS,
          expression->children->result_register,
          expression->children->next_child->result_register);
    } else {
      fprintf(stderr, "Unrecognized binary operator: \"%s\"\n", expression->value.symbol);
      ERROR_PREP(err, ERROR_GENERIC, "codegen_expression() does not recognize binary operator");
      return err;
    }
    break;
  case NODE_TYPE_VARIABLE_ACCESS:
    if (codegen_verbose) {
      fprintf(code, ";;#; Variable Access: \"%s\"\n", expression->value.symbol);
    }
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
      expression->result_register = codegen_load_global(cg_context, expression->value.symbol);
    } else {
      // TODO: For each context change upwards (base pointer load), emit a call to load caller RBP
      // from current RBP into some register, and use that register as offset for memory access.
      // This will require us to differentiate scopes from stack frames, which is a problem for
      // another time :^). Good luck, future me!
      expression->result_register = codegen_load_local(cg_context, tmpnode->value.integer);
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
    codegen_alloca(cg_context, size_in_bytes);
    // Keep track of RBP offset.
    // FIXME(Sirraide): this is probably no longer necessary since we now reset
    //   RSP to RBP at the end of a function anyway.
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
      // TODO: Error here for invalid or misshapen AST.
    }

    // Codegen RHS
    err = codegen_expression(cg_context, context, next_child_context,
                             expression->children->next_child);
    if (err.type) { break; }

    if (expression->children->type == NODE_TYPE_VARIABLE_ACCESS) {
      symbol_address address = symbol_to_address(cg_context, expression->children);
      switch (address.mode) {
        case SYMBOL_ADDRESS_MODE_ERROR: return address.error;
        case SYMBOL_ADDRESS_MODE_GLOBAL:
          codegen_store_global(cg_context, expression->children->next_child->result_register, address.global);
          break;
        case SYMBOL_ADDRESS_MODE_LOCAL:
          codegen_store_local(cg_context, expression->children->next_child->result_register, address.local);
          break;
      }
    } else {
      // Codegen LHS
      err = codegen_expression(cg_context, context, next_child_context,
                               expression->children);
      if (err.type) { break; }
      codegen_store(cg_context,
                    expression->children->next_child->result_register,
                    expression->children->result_register);
      register_deallocate(cg_context, expression->children->next_child->result_register);
      register_deallocate(cg_context, expression->children->result_register);
    }
    break;
  }

  ASSERT(expression->result_register != -1,
         "Result register of expression not set. Likely an internal error during codegen.");

  free(tmpnode);
  return err;
}

#define LABEL_NAME_BUFFER_SIZE (1024)

Error codegen_function
(CodegenContext *cg_context,
 ParsingContext *context,
 ParsingContext **next_child_context,
 char *name,
 Node *function
 )
{
  Error err = ok;
  FILE *code = cg_context->code;

  cg_context = codegen_context_create(cg_context);

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

  char after_name_buffer[LABEL_NAME_BUFFER_SIZE];
  snprintf(after_name_buffer, sizeof after_name_buffer, "after%s", name);
  after_name_buffer[sizeof after_name_buffer - 1] = 0;
  codegen_branch(cg_context, after_name_buffer);

  // Function beginning label
  fprintf(code, "%s:\n", name);

  // Function header
  codegen_function_prologue(cg_context);

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
    err = codegen_expression(cg_context, ctx, &next_child_ctx, expression);
    register_deallocate(cg_context, expression->result_register);
    if (err.type) {
      print_error(err);
      return err;
    }
    last_expression = expression;
    expression = expression->next_child;
  }

  codegen_set_return_value(cg_context, last_expression->result_register);

  // Function footer
  codegen_function_epilogue(cg_context);

  // Nested function execution jump label
  fprintf(code, "%s:\n", after_name_buffer);
  // after<function_label>:

  // Free context;
  codegen_context_free(cg_context);
  return ok;
}

Error codegen_program(CodegenContext *cg_context, ParsingContext *context, Node *program) {
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

  codegen_entry_point(cg_context);

  ParsingContext *next_child_context = context->children;
  Node *last_expression = program->children;
  Node *expression = program->children;
  while (expression) {
    if (nonep(*expression)) {
      expression = expression->next_child;
      continue;
    }
    err = codegen_expression(cg_context, context, &next_child_context, expression);
    if (err.type) { return err; }
    register_deallocate(cg_context, expression->result_register);
    last_expression = expression;
    expression = expression->next_child;
  }

  // Copy last expression into RAX register for return value.
  // femit() will optimise the move away if the result is already in RAX.
  if (last_expression) {
    codegen_set_return_value(cg_context, last_expression->result_register);
  }

  codegen_function_epilogue(cg_context);

  return err;
}

//================================================================ END CG_FMT_x86_64_MSWIN

Error codegen
(enum CodegenOutputFormat format,
 enum CodegenCallingConvention call_convention,
 enum CodegenAssemblyDialect dialect,
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

  CodegenContext *cg_context = codegen_context_create_top_level(format, call_convention, dialect, code);
  err = codegen_program(cg_context, context, program);
  codegen_context_free(cg_context);

  fclose(code);
  return err;
}
