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

//================================================================ BEG CG_FMT_x86_64_MSWIN
#define node_integer _Static_assert(0); node_integer
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
    Value* local;
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
  Node *variable = node_allocate();
  if (!environment_get(*cg_ctx->locals, symbol, variable)) {
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

  Value* var = variable->value.local_variable;
  free(variable);
  return (symbol_address) {
    .mode = SYMBOL_ADDRESS_MODE_LOCAL,
    .local = var,
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

  ASSERT(NODE_TYPE_MAX == 16, "codegen_expression_x86_64() must exhaustively handle node types!");
  switch (expression->type) {
  default:
    break;
  case NODE_TYPE_LOCAL_VARIABLE:
    PANIC("AST should never contain LOCAL_VARIABLE nodes");
  case NODE_TYPE_INTEGER:
    codegen_comment_verbose(cg_context, "INTEGER: %lld", expression->value.integer);
    expression->result = codegen_load_immediate(cg_context, expression->value.integer);
    break;
  case NODE_TYPE_FUNCTION_CALL:
    codegen_comment_verbose(cg_context, "Function Call: \"%s\"", expression->children->value.symbol);

    // TODO: Should we technically save all in-use scratch registers?
    // Save RAX because function call will over-write it!

    // Setup function environment based on calling convention.

    Node *variable_type = node_allocate();
    // Use `typecheck_expression()` to get type of variable.
    // err is ignored purposefully, program already type-checked valid.
    typecheck_expression(context, NULL, expression->children, variable_type);

    iterator = expression->children->next_child->children;
    if (strcmp(variable_type->value.symbol, "external function") == 0) {
      // TODO: Save RCX, RDX, R8, and R9 (they are scratch registers).
      // TODO: Only save scratch registers that are in-use.
      expression->result = codegen_create_call(cg_context, 1);

      // Put arguments in RCX, RDX, R8, R9, then on the stack in reverse order.
      while (iterator) {
        // Place first argument in RCX or XMM0
        err = codegen_expression(cg_context, context, next_child_context, iterator);
        if (err.type) { return err; }

        codegen_add_function_arg(cg_context, expression->result, iterator->result);
        iterator = iterator->next_child;
      }

      // TODO: Reverse rest of arguments, push on stack.
    } else {
      expression->result = codegen_create_call(cg_context, 0);

      // Push arguments on stack in order.
      while (iterator) {
        err = codegen_expression(cg_context, context, next_child_context, iterator);
        if (err.type) { return err; }
        codegen_add_function_arg(cg_context, expression->result, iterator->result);
        iterator = iterator->next_child;
      }

      err = codegen_expression(cg_context, context, next_child_context, expression->children);
      if (err.type) { return err; }
    }

    break;
  case NODE_TYPE_FUNCTION:
    codegen_comment_verbose(cg_context, "Function");

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
    err = codegen_function(cg_context,
                           context, next_child_context,
                           result, expression);

    // Function returns beginning of instructions address.
    expression->result = codegen_load_global_address(cg_context, result);
    break;
  case NODE_TYPE_DEREFERENCE:
    codegen_comment_verbose(cg_context, "Dereference");

    err = codegen_expression(cg_context,
                             context, next_child_context,
                             expression->children);
    if (err.type) { return err; }
    expression->result = expression->children->result;
    break;
  case NODE_TYPE_ADDRESSOF: {
    codegen_comment_verbose(cg_context, "Addressof");

    symbol_address address = symbol_to_address(cg_context, expression->children);
    switch (address.mode) {
      case SYMBOL_ADDRESS_MODE_ERROR: return address.error;
      case SYMBOL_ADDRESS_MODE_GLOBAL:
        expression->result = codegen_load_global_address(cg_context, address.global);
        break;
      case SYMBOL_ADDRESS_MODE_LOCAL:
        expression->result = codegen_load_local_address(cg_context, address.local);
        break;
    }
    break;
  }
  case NODE_TYPE_INDEX: {
    codegen_comment_verbose(cg_context, "Index %lld", expression->value.integer);

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
        expression->result = codegen_load_global_address(cg_context, address.global);
        break;
      case SYMBOL_ADDRESS_MODE_LOCAL:
        expression->result = codegen_load_local_address(cg_context, address.local);
        break;
    }
    // Offset memory address by index.
    if (offset) {

      expression->result = codegen_add(cg_context, expression->result, codegen_load_immediate(cg_context, offset));
    }
    break;
  }
  case NODE_TYPE_IF:
    codegen_comment_verbose(cg_context, "If");

    // Generate if condition expression code.
    err = codegen_expression(cg_context,
                             context, next_child_context,
                             expression->children);
    if (err.type) { return err; }

    codegen_comment_verbose(cg_context, "If CONDITION");

    // Generate code using result register from condition expression.
    BasicBlock *then_block = codegen_basic_block_create(cg_context);
    BasicBlock *else_block = codegen_basic_block_create_detached(cg_context);
    BasicBlock *end_block = codegen_basic_block_create_detached(cg_context);

    Value* then_result = NULL;
    Value* else_result = NULL;

    codegen_branch_if(cg_context, expression->children->result, then_block, else_block);

    codegen_comment_verbose(cg_context, "If THEN");
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
      last_expr = expr;
      expr = expr->next_child;
    }

    // Save the value for later.
    then_result = last_expr ? last_expr->result : NULL;

    // Skip the else branch.
    codegen_branch(cg_context, end_block);

    codegen_comment_verbose(cg_context, "If OTHERWISE");

    // Generate OTHERWISE
    codegen_basic_block_attach(cg_context, else_block);

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
        last_expr = expr;
        expr = expr->next_child;
      }
      // Copy last_expr result register to if result register.
      if (last_expr) {
        else_result = last_expr ? last_expr->result : NULL;
      }
    } else {
      else_result = codegen_load_immediate(cg_context, 0);
    }

    codegen_basic_block_attach(cg_context, end_block);
    expression->result = codegen_phi_create(cg_context);
    codegen_phi_add(cg_context, expression->result, then_block, then_result);
    codegen_phi_add(cg_context, expression->result, else_block, else_result);

    break;
  case NODE_TYPE_BINARY_OPERATOR:
    codegen_comment_verbose(cg_context, "Binary Operator: \"%s\"", expression->value.symbol);

    while (context->parent) { context = context->parent; }
    // FIXME: Second argument is memory leaked! :^(
    environment_get(*context->binary_operators, node_symbol(expression->value.symbol), tmpnode);

    //codegen_comment_verbose(cg_context, "Codegenning binary operator %s", expression->value.symbol);
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
      expression->result = codegen_comparison(cg_context,
          COMPARE_GT,
          expression->children->result,
          expression->children->next_child->result);
    } else if (strcmp(expression->value.symbol, "<") == 0) {
      expression->result = codegen_comparison(cg_context,
          COMPARE_LT,
          expression->children->result,
          expression->children->next_child->result);
    } else if (strcmp(expression->value.symbol, "=") == 0) {
      expression->result = codegen_comparison(cg_context,
          COMPARE_EQ,
          expression->children->result,
          expression->children->next_child->result);
    } else if (strcmp(expression->value.symbol, "+") == 0) {
      expression->result = codegen_add(cg_context,
          expression->children->result,
          expression->children->next_child->result);
    } else if (strcmp(expression->value.symbol, "-") == 0) {
      expression->result = codegen_subtract(cg_context,
          expression->children->result,
          expression->children->next_child->result);
    } else if (strcmp(expression->value.symbol, "*") == 0) {
      expression->result = codegen_multiply(cg_context,
          expression->children->result,
          expression->children->next_child->result);
    } else if (strcmp(expression->value.symbol, "/") == 0) {
      expression->result = codegen_divide(cg_context,
          expression->children->result,
          expression->children->next_child->result);
    } else if (strcmp(expression->value.symbol, "%") == 0) {
      expression->result = codegen_modulo(cg_context,
          expression->children->result,
          expression->children->next_child->result);
    } else if (strcmp(expression->value.symbol, "<<") == 0) {
      expression->result = codegen_shift_left(cg_context,
          expression->children->result,
          expression->children->next_child->result);
    } else if (strcmp(expression->value.symbol, ">>") == 0) {
      expression->result = codegen_shift_right_arithmetic(cg_context,
          expression->children->result,
          expression->children->next_child->result);
    } else {
      PANIC("Unrecognized binary operator: \"%s\"", expression->value.symbol);
    }
    break;
  case NODE_TYPE_VARIABLE_ACCESS:
    codegen_comment_verbose(cg_context, "Variable Access: \"%s\"", expression->value.symbol);

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
      expression->result = codegen_load_global(cg_context, expression->value.symbol);
    } else {
      // TODO: For each context change upwards (base pointer load), emit a call to load caller RBP
      // from current RBP into some register, and use that register as offset for memory access.
      // This will require us to differentiate scopes from stack frames, which is a problem for
      // another time :^). Good luck, future me!
      expression->result = codegen_load_local(cg_context, tmpnode->value.local_variable);
    }
    break;
  case NODE_TYPE_VARIABLE_DECLARATION:
    if (!cg_context->parent) { break; }
    codegen_comment_verbose(cg_context, "Variable Declaration: \"%s\"", expression->children->value.symbol);
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
      PANIC("Invalid AST/context fed to codegen. Could not find variable declaration in environment\n"
            "Variable Symbol: \"%s\"",
            expression->children->value.symbol);
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
    Value* variable = codegen_alloca(cg_context, size_in_bytes);
    // Keep track of RBP offset.
    // FIXME(Sirraide): this is probably no longer necessary since we now reset
    //   RSP to RBP at the end of a function anyway.
    cg_context->locals_offset -= size_in_bytes;
    //   Kept in codegen context.
    environment_set(cg_context->locals, expression->children, node_local_variable(variable));
    break;
  case NODE_TYPE_VARIABLE_REASSIGNMENT:
    codegen_comment_verbose(cg_context, "Variable Reassignment");

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
          codegen_store_global(cg_context, expression->children->next_child->result, address.global);
          break;
        case SYMBOL_ADDRESS_MODE_LOCAL:
          codegen_store_local(cg_context, expression->children->next_child->result, address.local);
          break;
      }
    } else {
      // Codegen LHS
      err = codegen_expression(cg_context, context, next_child_context,
                               expression->children);
      if (err.type) { break; }
      codegen_store(cg_context,
                    expression->children->next_child->result,
                    expression->children->result);
    }
    break;
  case NODE_TYPE_CAST:
    if (0) {}

    Node *cast_type = expression->children;
    // TODO: Somehow avoid typechecking twice; this is only needed to get result type of expression.
    Node *expression_type = node_allocate();
    err = typecheck_expression(context, next_child_context, expression->children->next_child, expression_type);
    if (err.type) { return err; }

    // Get size of cast_type and expression_type to determine kind of
    // typecast.
    Node *cast_type_info = node_allocate();
    Node *expression_type_info = node_allocate();
    err = parse_get_type(context, cast_type, cast_type_info);
    if (err.type) { return err; }
    err = parse_get_type(context, expression_type, expression_type_info);
    if (err.type) { return err; }
    size_t cast_type_size = cast_type_info->children->value.integer;
    size_t expression_type_size = expression_type_info->children->value.integer;
    free(cast_type_info);
    free(expression_type_info);

    if (cast_type_size > expression_type_size) {
      // TODO: Set `expression_signed` to `1` iff expression_type is of signed type.
      char expression_signed = 0;
      if (expression_signed) {
        TODO("Handle TYPECAST sign extension in codegen_platform.c!");
      } else {
        TODO("Handle TYPECAST zero extension in codegen_platform.c!");
      }
    } else if (cast_type_size < expression_type_size) {
      TODO("Handle TYPECAST truncation in codegen_platform.c!");
    }

    break;
  }

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

  Function *f = codegen_function_create(cg_context, NULL);

  size_t param_count = 0;
  Node *parameter = function->children->next_child->children;
  while (parameter) {
    Value *param = codegen_bind_function_parameter(cg_context, f, param_count++);
    environment_set(cg_context->locals, parameter->children, node_local_variable(param));
    parameter = parameter->next_child;
  }

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
    if (err.type) {
      print_error(err);
      return err;
    }
    last_expression = expression;
    expression = expression->next_child;
  }

  codegen_set_return_value(cg_context, f, last_expression->result);
  function->result = codegen_function_ref(cg_context, f);

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

  // Entry point is always main atm.
  Function *main = codegen_function_create(cg_context, "main");

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
    last_expression = expression;
    expression = expression->next_child;
  }

  // Copy last expression into RAX register for return value.
  // femit() will optimise the move away if the result is already in RAX.
  if (last_expression) {
    codegen_set_return_value(cg_context, main, last_expression->result);
  }

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
    fprintf(stderr, "Filepath: \"%s\"\n", filepath);
    ERROR_PREP(err, ERROR_GENERIC, "codegen_program(): fopen failed to open file at path.");
    return err;
  }

  CodegenContext *cg_context = codegen_context_create_top_level(format, call_convention, dialect, code);
  err = codegen_program(cg_context, context, program);
  codegen_dump_ir(cg_context);
  codegen_context_free(cg_context);

  fclose(code);
  return err;
}
