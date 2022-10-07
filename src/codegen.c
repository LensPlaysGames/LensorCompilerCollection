#include <codegen.h>

#include <codegen/codegen_forward.h>
#include <codegen/intermediate_representation.h>
#include <codegen/x86_64/arch_x86_64.h>
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

CodegenContext *codegen_context_create_top_level
(ParsingContext *parse_context,
 enum CodegenOutputFormat format,
 enum CodegenCallingConvention call_convention,
 enum CodegenAssemblyDialect dialect,
 FILE* code
 )
{
  CodegenContext *context;

  if (format == CG_FMT_x86_64_GAS) {
    // TODO: Handle call_convention for creating codegen context!
    if (call_convention == CG_CALL_CONV_MSWIN) {
      context = codegen_context_x86_64_mswin_create(NULL);
      ASSERT(context);
    } else if (call_convention == CG_CALL_CONV_LINUX) {
      // TODO: Create codegen context for GAS linux assembly.
      panic("Not implemented: Create codegen context for GAS linux x86_64 assembly.");
    } else {
      panic("Unrecognized calling convention!");
    }
  } else {
    panic("Unrecognized codegen format");
  }

  context->parse_context = parse_context;
  context->code = code;
  context->dialect = dialect;
  return context;
}

CodegenContext *codegen_context_create(CodegenContext *parent) {
  ASSERT(parent, "create_codegen_context() can only create contexts when a parent is given.");
  ASSERT(CG_FMT_COUNT == 1, "create_codegen_context() must exhaustively handle all codegen output formats.");
  ASSERT(CG_CALL_CONV_COUNT == 2, "create_codegen_context() must exhaustively handle all calling conventions.");

  CodegenContext *new_context = NULL;

  switch (parent->format) {
  case CG_FMT_x86_64_GAS:
    switch (parent->call_convention) {
    case CG_CALL_CONV_MSWIN:
      new_context = codegen_context_x86_64_mswin_create(parent);
      break;
    default:
      TODO("Handle %d codegen call convention.", parent->call_convention);
      break;
    }
    break;
  default:
    TODO("Handle %d codegen output format.", parent->format);
    break;
  }

  new_context->parse_context    = parent->parse_context;
  new_context->all_functions    = parent->all_functions;
  new_context->function         = parent->function;
  new_context->block            = parent->block;
  new_context->dialect          = parent->dialect;
  new_context->call_convention  = parent->call_convention;
  new_context->format           = parent->format;
  new_context->code             = parent->code;

  return new_context;
}

void codegen_context_free(CodegenContext *context) {
  if (context->format == CG_FMT_x86_64_GAS) {
    if (context->call_convention == CG_CALL_CONV_MSWIN) {
      return codegen_context_x86_64_mswin_free(context);
    } else if (context->call_convention == CG_CALL_CONV_LINUX) {
      // return codegen_context_x86_64_gas_linux_free(parent);
    }
  }
  PANIC("Could not free the given context.");
}

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
typedef struct SymbolAddress {
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
    char *global;
    IRInstruction *local;
  };
} SymbolAddress;

SymbolAddress symbol_to_address(CodegenContext *cg_ctx, Node *symbol) {
  SymbolAddress out;
  out.mode = SYMBOL_ADDRESS_MODE_ERROR;
  out.error = ok;

  ASSERT(cg_ctx, "symbol_to_address(): Context must not be NULL (pass global).");
  ASSERT(symbol && symbol->value.symbol, "symbol_to_address(): A symbol must be passed.");

  // Global variable access.
  if (!cg_ctx->parent) {
    out.mode = SYMBOL_ADDRESS_MODE_GLOBAL;
    out.global = symbol->value.symbol;
    return out;
  }

  // Local variable access.
  // TODO: Make a custom symbol to local instruction hash map/table.
  // That way, we can get rid of IR node type in the AST.
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
    snprintf(err_buf, sizeof(err_buf), "symbol_to_address(): Could not find symbol '%s' in environment.", symbol->value.symbol);
    ERROR_CREATE(err, ERROR_GENERIC, err_buf);
    out.mode = SYMBOL_ADDRESS_MODE_ERROR;
    out.error = err;
    return out;
  }

  IRInstruction *address = stack_offset->value.ir_instruction;
  free(stack_offset);
  out.mode = SYMBOL_ADDRESS_MODE_LOCAL;
  out.local = address;
  return out;
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

  ASSERT(NODE_TYPE_MAX == 15, "codegen_expression_x86_64() must exhaustively handle node types!");
  switch (expression->type) {
  default:
    break;
  case NODE_TYPE_INTEGER:
    expression->result = ir_immediate(cg_context, expression->value.integer);
    break;
  case NODE_TYPE_FUNCTION_CALL:
    if (0) {}
    // Use `typecheck_expression()` to get type of variable.
    Node *variable_type = node_allocate();
    // err is ignored purposefully, program already type-checked valid.
    typecheck_expression(context, NULL, expression->children, variable_type);

    INSTRUCTION(call, IR_CALL);

    if (strcmp(variable_type->value.symbol, "external function") == 0) {
      call->value.call.type = IR_CALLTYPE_DIRECT;
      call->value.call.value.name = expression->children->value.symbol;
    } else {
      err = codegen_expression(cg_context, context, next_child_context, expression->children);
      if (err.type) { return err; }
      call->value.call.type = IR_CALLTYPE_INDIRECT;
      call->value.call.value.callee = expression->children->result;
    }

    for (iterator = expression->children->next_child->children;
         iterator;
         iterator = iterator->next_child
         ) {
      err = codegen_expression(cg_context, context, next_child_context, iterator);
      if (err.type) { return err; }
      ir_add_function_call_argument(cg_context, call, iterator->result);
    }

    ir_insert(cg_context, call);
    expression->result = call;

    break;
  case NODE_TYPE_FUNCTION:
    if (0) {}
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
    err = codegen_function
      (cg_context,
       context, next_child_context,
       result, expression);
    if (err.type) { return err; }
    // Function returns beginning of instructions address.
    expression->result = ir_load_global_address(cg_context, result);
    break;
  case NODE_TYPE_DEREFERENCE:
    err = codegen_expression(cg_context,
                             context, next_child_context,
                             expression->children);
    if (err.type) { return err; }
    expression->result = ir_load(cg_context, expression->children->result);
    break;
  case NODE_TYPE_ADDRESSOF: {
    SymbolAddress address = symbol_to_address(cg_context, expression->children);
    switch (address.mode) {
    case SYMBOL_ADDRESS_MODE_ERROR: return address.error;
    case SYMBOL_ADDRESS_MODE_GLOBAL:
      expression->result = ir_load_global_address(cg_context, address.global);
      break;
    case SYMBOL_ADDRESS_MODE_LOCAL:
      expression->result = ir_load_local_address(cg_context, address.local);
      break;
    }
    break;
  }
  case NODE_TYPE_INDEX: {
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
    SymbolAddress address = symbol_to_address(cg_context, expression->children);
    switch (address.mode) {
      case SYMBOL_ADDRESS_MODE_ERROR:
        return address.error;
      case SYMBOL_ADDRESS_MODE_GLOBAL:
        expression->result = ir_load_global_address(cg_context, address.global);
        break;
      case SYMBOL_ADDRESS_MODE_LOCAL:
        expression->result = ir_load_local_address(cg_context, address.local);
        break;
    }
    // Offset memory address by index.
    if (offset) {
      TODO("Create IR_IMMEDIATE to load offset into temporary, then generate an add between the new temporary and the result of loading the array's address.");
    }
    break;
  }
  case NODE_TYPE_IF:
    // Generate if condition expression code.
    err = codegen_expression(cg_context,
                             context, next_child_context,
                             expression->children);
    if (err.type) { return err; }

    /** Each box is a basic block within intermediate representation,
     *  and edges represent control flow from top to bottom.
     *
     *      +---------+
     *      | current |
     *      +---------+
     *     /           \
     * +------+    +-----------+
     * | then |    | otherwise |
     * +------+    +-----------+
     *         \  /
     *       +------+
     *       | join |
     *       +------+
     */

    IRBlock *then_block = ir_block_create();
    IRBlock *last_then_block = then_block;
    IRBlock *otherwise_block = ir_block_create();
    IRBlock *last_otherwise_block = otherwise_block;
    IRBlock *join_block = ir_block_create();

    // Generate if instruction with then, otherwise blocks
    ir_branch_conditional(cg_context, expression->children->result, then_block, otherwise_block);

    // Attach then_block to current function and make it active as our
    // context block.
    ir_block_attach(cg_context, then_block);

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

    // Generate an unconditional branch to the join_block.
    ir_branch(cg_context, join_block);

    last_then_block = cg_context->block;

    // Generate OTHERWISE

    // Attach otherwise_block to current function and make it active as
    // our context block.
    ir_block_attach(cg_context, otherwise_block);

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

      ir_branch(cg_context, join_block);

      last_otherwise_block = cg_context->block;

    } else {
      ir_immediate(cg_context, 0);
    }

    // This assumes that the last instruction in a block returns a
    // value; if it doesn't, we will simply return zero. This should
    // probably be ensured in the type checker in the future.
    IRInstruction *then_return_value = last_then_block->last_instruction;
    IRInstruction *otherwise_return_value = last_otherwise_block->last_instruction;

    // Attach join_block to function and set it as the active context
    // block.
    ir_block_attach(cg_context, join_block);

    // Insert phi node for result of if expression in join block.
    IRInstruction *phi = ir_phi(cg_context);
    ir_phi_argument(phi, last_otherwise_block, then_return_value);
    ir_phi_argument(phi, last_otherwise_block, otherwise_return_value);

    expression->result = phi;

    break;
  case NODE_TYPE_BINARY_OPERATOR:
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
      expression->result = ir_comparison
        (cg_context,
         COMPARE_GT,
         expression->children->result,
         expression->children->next_child->result);
    } else if (strcmp(expression->value.symbol, "<") == 0) {
      expression->result = ir_comparison
        (cg_context,
         COMPARE_LT,
         expression->children->result,
         expression->children->next_child->result);
    } else if (strcmp(expression->value.symbol, "=") == 0) {
      expression->result = ir_comparison
        (cg_context,
         COMPARE_EQ,
         expression->children->result,
         expression->children->next_child->result);
    } else if (strcmp(expression->value.symbol, "+") == 0) {
      expression->result = ir_add
        (cg_context,
         expression->children->result,
         expression->children->next_child->result);
    } else if (strcmp(expression->value.symbol, "-") == 0) {
      expression->result = ir_subtract
        (cg_context,
         expression->children->result,
         expression->children->next_child->result);
    } else if (strcmp(expression->value.symbol, "*") == 0) {
      expression->result = ir_multiply
        (cg_context,
         expression->children->result,
         expression->children->next_child->result);
    } else if (strcmp(expression->value.symbol, "/") == 0) {
      expression->result = ir_divide
        (cg_context,
         expression->children->result,
         expression->children->next_child->result);
    } else if (strcmp(expression->value.symbol, "%") == 0) {
      expression->result = ir_modulo
        (cg_context,
         expression->children->result,
         expression->children->next_child->result);
    } else if (strcmp(expression->value.symbol, "<<") == 0) {
      expression->result = ir_shift_left
        (cg_context,
         expression->children->result,
         expression->children->next_child->result);
    } else if (strcmp(expression->value.symbol, ">>") == 0) {
      expression->result = ir_shift_right_arithmetic
        (cg_context,
         expression->children->result,
         expression->children->next_child->result);
    } else {
      fprintf(stderr, "Unrecognized binary operator: \"%s\"\n", expression->value.symbol);
      ERROR_PREP(err, ERROR_GENERIC, "codegen_expression() does not recognize binary operator");
      return err;
    }
    break;
  case NODE_TYPE_VARIABLE_ACCESS:
    if (0) {}

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
      expression->result = ir_load_global(cg_context, expression->value.symbol);
    } else {
      // TODO: For each context change upwards (base pointer load), emit a call to load caller RBP
      // from current RBP into some register, and use that register as offset for memory access.
      // This will require us to differentiate scopes from stack frames, which is a problem for
      // another time :^). Good luck, future me!
      expression->result = ir_load_local(cg_context, tmpnode->value.ir_instruction);
    }
    break;
  case NODE_TYPE_VARIABLE_DECLARATION:
    if (!cg_context->parent) { break; }
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

    IRInstruction *local = ir_stack_allocate(cg_context, size_in_bytes);
    Node *local_reference = node_allocate();
    local_reference->value.ir_instruction = local;
    environment_set(cg_context->locals, expression->children, local_reference);
    break;
  case NODE_TYPE_VARIABLE_REASSIGNMENT:
    // Recurse LHS into children until LHS is a var. access.
    // Set iterator to the var. access node.
    iterator = expression->children;
    while (iterator && iterator->type != NODE_TYPE_VARIABLE_ACCESS) {
      iterator = iterator->children;
    }
    ASSERT(iterator, "Invalid or mishapen AST.");

    // Codegen RHS
    err = codegen_expression(cg_context, context, next_child_context,
                             expression->children->next_child);
    if (err.type) { break; }

    if (expression->children->type == NODE_TYPE_VARIABLE_ACCESS) {
      SymbolAddress address = symbol_to_address(cg_context, expression->children);
      switch (address.mode) {
        case SYMBOL_ADDRESS_MODE_ERROR:
          return address.error;
        case SYMBOL_ADDRESS_MODE_GLOBAL:
          expression->result = ir_store_global
            (cg_context,
             expression->children->next_child->result,
             address.global);
          break;
        case SYMBOL_ADDRESS_MODE_LOCAL:
          expression->result = ir_store_local
            (cg_context,
             expression->children->next_child->result,
             address.local);
          break;
      }
    } else {
      // Codegen LHS
      err = codegen_expression(cg_context, context, next_child_context,
                               expression->children);
      if (err.type) { break; }
      expression->result = ir_store(cg_context,
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

  IRFunction *f = ir_function(cg_context);

  // Store base pointer integer offset within locals environment
  // Start at one to make space for pushed RBP in function header.
  size_t param_count = 1;
  Node *parameter = function->children->next_child->children;
  while (parameter) {
    Node *param_node = node_allocate();

    INSTRUCTION(param, IR_PARAMETER_REFERENCE);
    param->value.immediate = param_count++;
    ir_insert(cg_context, param);

    param_node->value.ir_instruction = param;
    environment_set(cg_context->locals, parameter->children, param_node);

    parameter = parameter->next_child;
  }

  // TODO: REMOVE THIS!!!
  // Function beginning label
  fprintf(code, "%s:\n", name);

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

  IRInstruction *branch = ir_return(cg_context);
  f->last->branch = branch;

  f->return_value = last_expression->result;

  // Free context;
  codegen_context_free(cg_context);
  return ok;
}

Error codegen_program(CodegenContext *context, Node *program) {
  Error err = ok;

  IRFunction *main = ir_function(context);

  ParsingContext *next_child_context = context->parse_context->children;
  Node *last_expression = NULL;
  Node *expression = program->children;
  while (expression) {
    if (nonep(*expression)) {
      expression = expression->next_child;
      continue;
    }
    err = codegen_expression(context, context->parse_context, &next_child_context, expression);
    if (err.type) { return err; }
    last_expression = expression;
    expression = expression->next_child;
  }
  if (!main->last->branch) {
    IRInstruction *branch = ir_return(context);
    main->last->branch = branch;
  }
  if (last_expression) {
    main->return_value = last_expression->result;
  }
  return err;
}

//================================================================ END CG_FMT_x86_64_MSWIN

void codegen_emit(CodegenContext *context) {
  switch (context->format) {
  case CG_FMT_x86_64_GAS:
    codegen_emit_x86_64(context);
    break;
  default:
    TODO("Handle %d code generation format.", context->format);
  }
}

Error codegen
(enum CodegenOutputFormat format,
 enum CodegenCallingConvention call_convention,
 enum CodegenAssemblyDialect dialect,
 char *filepath,
 ParsingContext *parse_context,
 Node *program
 )
{
  Error err = ok;
  if (!filepath) {
    ERROR_PREP(err, ERROR_ARGUMENTS, "codegen(): filepath can not be NULL!");
    return err;
  }
  // Open file for writing.
  FILE *code = fopen(filepath, "w");
  if (!code) {
    printf("Filepath: \"%s\"\n", filepath);
    ERROR_PREP(err, ERROR_GENERIC, "codegen(): fopen failed to open file at path.");
    return err;
  }

  CodegenContext *context = codegen_context_create_top_level
    (parse_context, format, call_convention, dialect, code);
  err = codegen_program(context, program);

  ir_set_ids(context);
  ir_femit(stdout, context);

  codegen_emit(context);

  codegen_context_free(context);

  fclose(code);
  return err;
}
