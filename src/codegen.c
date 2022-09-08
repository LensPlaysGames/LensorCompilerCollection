#include <codegen.h>

#include <assert.h>
#include <environment.h>
#include <error.h>
#include <parser.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

CodegenContext *codegen_context_create(CodegenContext *parent) {
  CodegenContext *cg_ctx = calloc(1,sizeof(CodegenContext));
  cg_ctx->parent = parent;
  cg_ctx->locals = environment_create(NULL);
  // TODO/FIXME: This is specific to x86_64 right now [2022-08-18 Thu 10:40]
  cg_ctx->locals_offset = -32;
  return cg_ctx;
}

//================================================================ BEG REGISTER STUFF

void print_registers(Register *base) {
  Register *it = base;
  while (it) {
    printf("%s:%i\n", it->name, it->in_use);
    it = it->next;
  }
}

Register *register_create(char *name) {
  Register *r = calloc(1,sizeof(Register));
  assert(r && "Could not allocate memory for new register");
  r->name = name;
  return r;
}

void register_add(Register *base, char *name) {
  while (base->next) { base = base->next; }
  base->next = register_create(name);
}

// TODO: This is broken :^)
void register_free(Register *base) {
  Register *to_free = NULL;
  while (base) {
    to_free = base;
    if (base->name) {
      free(base->name);
    }
    base = base->next;
    free(to_free);
  }
}

RegisterDescriptor register_allocate(Register *base) {
  RegisterDescriptor register_descriptor = 0;
  while (base) {
    if (base->in_use == 0) {
      base->in_use = 1;
      return register_descriptor;
    }
    base = base->next;
    register_descriptor++;
  }
  printf("ERROR::register_allocate(): Could not allocate register!\n");
  exit(1);
  return -1;
}

void register_deallocate
(Register *base, RegisterDescriptor register_descriptor) {
  while (base) {
    if (register_descriptor == 0) {
      base->in_use = 0;
      return;
    }
    base = base->next;
    register_descriptor--;
  }
  printf("ERROR::register_deallocate(): Could not deallocate register %d\n",
         register_descriptor);
  exit(1);
}

char *register_name
(Register *base, RegisterDescriptor register_descriptor) {
  while (base) {
    if (register_descriptor <= 0) {
      return base->name;
    }
    base = base->next;
    register_descriptor--;
  }
  printf("ERROR::register_name(): Could not find register with descriptor of %d\n",
         register_descriptor);
  return NULL;
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
(Register *r,
 CodegenContext *cg_context,
 ParsingContext *context,
 ParsingContext **next_child_context,
 char *name,
 Node *function,
 FILE *code);

Error codegen_expression_x86_64_mswin
(FILE *code,
 Register *r,
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
    expression->result_register = register_allocate(r);
    fprintf(code, "mov $%lld, %s\n",
            expression->value.integer,
            register_name(r, expression->result_register));
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
        (code, r, cg_context, context, next_child_context, iterator);
      if (err.type) { return err; }
      fprintf(code, "pushq %s\n", register_name(r, iterator->result_register));
      register_deallocate(r, iterator->result_register);
      iterator = iterator->next_child;
      count++;
    }

    err = codegen_expression_x86_64_mswin(code, r, cg_context, context, next_child_context, expression->children);
    if (err.type) { return err; }

    // Emit call
    fprintf(code, "call *%s\n", register_name(r, expression->children->result_register));
    register_deallocate(r, expression->children->result_register);
    if (count) {
      fprintf(code, "add $%lld, %%rsp\n", count * 8);
    }

    // Copy return value of function call from RAX to result register
    expression->result_register = register_allocate(r);
    if (strcmp(register_name(r, expression->result_register), "%rax") != 0) {
      fprintf(code, "mov %%rax, %s\n", register_name(r, expression->result_register));
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
    err = codegen_function_x86_64_att_asm_mswin(r, cg_context,
                                                context, next_child_context,
                                                result, expression, code);

    // Function returns beginning of instructions address.
    expression->result_register = register_allocate(r);
    fprintf(code, "lea %s(%%rip), %s\n",
            result,
            register_name(r, expression->result_register));
    break;
  case NODE_TYPE_DEREFERENCE:
    if (codegen_verbose) {
      fprintf(code, ";;#; Dereference\n");
    }
    err = codegen_expression_x86_64_mswin(code, r, cg_context,
                                          context, next_child_context,
                                          expression->children);
    if (err.type) { return err; }
    expression->result_register = expression->children->result_register;
    break;
  case NODE_TYPE_ADDRESSOF:
    if (codegen_verbose) {
      fprintf(code, ";;#; Addressof\n");
    }
    expression->result_register = register_allocate(r);
    fprintf(code, "lea %s, %s\n",
            symbol_to_address(cg_context, expression->children),
            register_name(r, expression->result_register));
    if (err.type) { return err; }
    break;
  case NODE_TYPE_IF:
    if (codegen_verbose) {
      fprintf(code, ";;#; If\n");
    }

    // Generate if condition expression code.
    err = codegen_expression_x86_64_mswin(code, r, cg_context,
                                          context, next_child_context,
                                          expression->children);
    if (err.type) { return err; }

    if (codegen_verbose) {
      fprintf(code, ";;#; If CONDITION\n");
    }

    // Generate code using result register from condition expression.
    char *otherwise_label = label_generate();
    char *after_otherwise_label = label_generate();
    char *condition_register_name = register_name(r, expression->children->result_register);
    fprintf(code, "test %s, %s\n", condition_register_name, condition_register_name);
    fprintf(code, "jz %s\n", otherwise_label);
    register_deallocate(r,expression->children->result_register);

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
      err = codegen_expression_x86_64_mswin(code, r, cg_context,
                                            ctx, &next_child_ctx,
                                            expr);
      if (err.type) { return err; }
      if (last_expr) {
        register_deallocate(r, last_expr->result_register);
      }
      last_expr = expr;
      expr = expr->next_child;
    }

    // Generate code to copy last expr result register to if result register.
    expression->result_register = register_allocate(r);
    fprintf(code, "mov %s, %s\n",
            register_name(r, last_expr->result_register),
            register_name(r, expression->result_register));
    register_deallocate(r, last_expr->result_register);
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
        err = codegen_expression_x86_64_mswin(code, r, cg_context,
                                              ctx, &next_child_ctx,
                                              expr);
        if (err.type) { return err; }
        if (last_expr) {
          register_deallocate(r, last_expr->result_register);
        }
        last_expr = expr;
        expr = expr->next_child;
      }
      // Copy last_expr result register to if result register.
      if (last_expr) {
        fprintf(code, "mov %s, %s\n",
                register_name(r, last_expr->result_register),
                register_name(r, expression->result_register));
        register_deallocate(r, last_expr->result_register);
      }
    } else {
      fprintf(code, "mov $0, %s\n", register_name(r, expression->result_register));
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

    err = codegen_expression_x86_64_mswin(code, r, cg_context,
                                          context, next_child_context,
                                          expression->children);
    if (err.type) { return err; }
    err = codegen_expression_x86_64_mswin(code, r, cg_context,
                                          context, next_child_context,
                                          expression->children->next_child);
    if (err.type) { return err; }

    if (strcmp(expression->value.symbol, ">") == 0) {
      // Greater than
      // https://www.felixcloutier.com/x86/cmovcc

      expression->result_register = register_allocate(r);
      RegisterDescriptor true_register = register_allocate(r);

      fprintf(code, "mov $0, %s\n", register_name(r, expression->result_register));
      fprintf(code, "mov $1, %s\n", register_name(r, true_register));
      fprintf(code, "cmp %s, %s\n"
              , register_name(r, expression->children->next_child->result_register)
              , register_name(r, expression->children->result_register)
              );
      fprintf(code, "cmovg %s, %s\n",
              register_name(r, true_register),
              register_name(r, expression->result_register));

      // Free no-longer-used left hand side result register.
      register_deallocate(r, true_register);
      register_deallocate(r, expression->children->result_register);
      register_deallocate(r, expression->children->next_child->result_register);
    } else if (strcmp(expression->value.symbol, "<") == 0) {
      // Less than
      // https://www.felixcloutier.com/x86/cmovcc

      expression->result_register = register_allocate(r);
      RegisterDescriptor true_register = register_allocate(r);

      fprintf(code, "mov $0, %s\n", register_name(r, expression->result_register));
      fprintf(code, "mov $1, %s\n", register_name(r, true_register));
      fprintf(code, "cmp %s, %s\n"
              , register_name(r, expression->children->next_child->result_register)
              , register_name(r, expression->children->result_register)
              );
      fprintf(code, "cmovl %s, %s\n",
              register_name(r, true_register),
              register_name(r, expression->result_register));

      // Free no-longer-used left hand side result register.
      register_deallocate(r, true_register);
      register_deallocate(r, expression->children->result_register);
      register_deallocate(r, expression->children->next_child->result_register);
    } else if (strcmp(expression->value.symbol, "=") == 0) {
      // Equality
      // https://www.felixcloutier.com/x86/cmovcc

      expression->result_register = register_allocate(r);
      RegisterDescriptor true_register = register_allocate(r);

      fprintf(code, "mov $0, %s\n", register_name(r, expression->result_register));
      fprintf(code, "mov $1, %s\n", register_name(r, true_register));
      fprintf(code, "cmp %s, %s\n",
              register_name(r, expression->children->result_register),
              register_name(r, expression->children->next_child->result_register));
      fprintf(code, "cmove %s, %s\n",
              register_name(r, true_register),
              register_name(r, expression->result_register));

      // Free no-longer-used left hand side result register.
      register_deallocate(r, true_register);
      register_deallocate(r, expression->children->result_register);
      register_deallocate(r, expression->children->next_child->result_register);
    } else if (strcmp(expression->value.symbol, "+") == 0) {
      // Plus/Addition
      // https://www.felixcloutier.com/x86/add

      // Use right hand side result register as our result since ADD is destructive!
      expression->result_register = expression->children->next_child->result_register;

      fprintf(code, "add %s, %s\n",
              register_name(r, expression->children->result_register),
              register_name(r, expression->children->next_child->result_register));

      // Free no-longer-used left hand side result register.
      register_deallocate(r, expression->children->result_register);
    } else if (strcmp(expression->value.symbol, "-") == 0) {
      // Minus/Subtraction
      // https://www.felixcloutier.com/x86/sub

      // Use right hand side result register as our result since SUB is destructive!
      expression->result_register = expression->children->result_register;

      fprintf(code, "sub %s, %s\n",
              register_name(r, expression->children->next_child->result_register),
              register_name(r, expression->children->result_register));

      // Free no-longer-used left hand side result register.
      register_deallocate(r, expression->children->next_child->result_register);
    } else if (strcmp(expression->value.symbol, "*") == 0) {
      // Multiply
      // https://www.felixcloutier.com/x86/mul
      // https://www.felixcloutier.com/x86/imul

      // Use right hand side result register as our result since ADD is destructive!
      expression->result_register = expression->children->next_child->result_register;

      fprintf(code, "imul %s, %s\n",
              register_name(r, expression->children->result_register),
              register_name(r, expression->children->next_child->result_register));

      // Free no-longer-used left hand side result register.
      register_deallocate(r, expression->children->result_register);
    } else if (strcmp(expression->value.symbol, "/") == 0) {
      // Division
      // https://www.felixcloutier.com/x86/div
      // https://www.felixcloutier.com/x86/idiv

      // Quotient is in RAX, Remainder in RDX; we must save and
      // restore these registers before and after divide, sadly.

      fprintf(code,
              "push %%rax\n"
              "push %%rdx\n");

      // Zero RDX to not interfere with division result.
      // RDX is treated as the 8 high bytes of a 16-byte
      // number stored in RDX:RAX.
      fprintf(code, "xor %%rdx, %%rdx\n");

      // Load RAX with left hand side of division operator.
      // TODO: Check if LHS is already in RAX or not.
      //       If RHS is in RAX, we must save RAX first...
      fprintf(code,
              "mov %s, %%rax\n",
              register_name(r, expression->children->result_register));


      // Call DIV with right hand side of division operator.
      fprintf(code,
              "div %s\n",
              register_name(r, expression->children->next_child->result_register));

      // Move return value from RAX into wherever it actually belongs.
      expression->result_register = register_allocate(r);
      // TODO: Check if result_register is RAX...
      fprintf(code,
              "mov %%rax, %s\n",
              register_name(r, expression->result_register));

      fprintf(code,
              "pop %%rdx\n"
              "pop %%rax\n");
    } else if (strcmp(expression->value.symbol, "[") == 0) { // FIXME: Temp. bitshift operator
      // Bitshift Left
      // https://www.felixcloutier.com/x86/sal:sar:shl:shr

      // Use left hand side result register as our result since SHL is destructive!
      expression->result_register = expression->children->result_register;

      fprintf(code,
              "push %%rcx\n"
              "mov %s, %%rcx\n"
              "shl %%cl, %s\n"
              "pop %%rcx\n",
              register_name(r, expression->children->next_child->result_register),
              register_name(r, expression->children->result_register));

      // Free no-longer-used right hand side result register.
      register_deallocate(r, expression->children->next_child->result_register);
    } else if (strcmp(expression->value.symbol, "]") == 0) {
      // Minus/Subtraction
      // https://www.felixcloutier.com/x86/sub

      // Use left hand side result register as our result since SHR is destructive!
      expression->result_register = expression->children->result_register;

      fprintf(code,
              "push %%rcx\n"
              "mov %s, %%rcx\n"
              "sar %%cl, %s\n"
              "pop %%rcx\n",
              register_name(r, expression->children->next_child->result_register),
              register_name(r, expression->children->result_register));

      // Free no-longer-used right hand side result register.
      register_deallocate(r, expression->children->next_child->result_register);
    }
    break;
  case NODE_TYPE_VARIABLE_ACCESS:
    if (codegen_verbose) {
      fprintf(code, ";;#; Variable Access: \"%s\"\n", expression->value.symbol);
    }
    expression->result_register = register_allocate(r);

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
              register_name(r, expression->result_register));
    } else {
      // TODO: For each context change upwards (base pointer load), emit a call to load caller RBP
      // from current RBP into some register, and use that register as offset for memory access.
      // This will require us to differentiate scopes from stack frames, which is a problem for
      // another time :^). Good luck, future me!
      fprintf(code, "mov %lld(%%rbp), %s\n",
              tmpnode->value.integer,
              register_name(r, expression->result_register));
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
    err = codegen_expression_x86_64_mswin(code, r, cg_context, context, next_child_context,
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
      err = codegen_expression_x86_64_mswin(code, r, cg_context, context, next_child_context,
                                            expression->children);
      if (err.type) { break; }
      result = register_name(r, expression->children->result_register);
      // Put parenthesis around `result`.
      size_t needed_len = strlen(result) + 2;
      char *result_copy = strdup(result);
      result = malloc(needed_len + 1);
      snprintf(result, needed_len + 1, "(%s)", result_copy);
      result[needed_len] = '\0';
      free(result_copy);
    }
    fprintf(code, "mov %s, %s\n",
            register_name(r, expression->children->next_child->result_register),
            result);
    register_deallocate(r, expression->children->next_child->result_register);

    if (should_free_result) {
      free(result);
      register_deallocate(r, expression->children->result_register);
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
(Register *r,
 CodegenContext *cg_context,
 ParsingContext *context,
 ParsingContext **next_child_context,
 char *name,
 Node *function,
 FILE *code
 )
{
  Error err = ok;

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
  fprintf(code, "jmp after%s\n", name);
  // Function beginning label
  fprintf(code, "%s:\n", name);

  // Function header
  fprintf(code, "%s", function_header_x86_64);

  // TODO/FIXME: Do not save/restore these registers unless they are used in function body.
  fprintf(code,
          "push %%rbx\n"
          "push %%rsi\n"
          "push %%rdi\n");

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
    err = codegen_expression_x86_64_mswin(code, r, cg_context, ctx, &next_child_ctx, expression);
    register_deallocate(r, expression->result_register);
    if (err.type) {
      print_error(err);
      return err;
    }
    last_expression = expression;
    expression = expression->next_child;
  }

  // Copy last expression result register to RAX
  if (last_expression) {
    char *name = register_name(r,last_expression->result_register);
    if (strcmp(name, "%rax")) {
      fprintf(code, "mov %s, %%rax\n", name);
    }
  }

  // TODO/FIXME: Only save/restore when register used in function.
  fprintf(code,
          "pop %%rbx\n"
          "pop %%rsi\n"
          "pop %%rdi\n");

  // Function footer
  fprintf(code, "add $%lld, %%rsp\n", -cg_context->locals_offset);
  fprintf(code, "%s", function_footer_x86_64);

  // Nested function execution jump label
  fprintf(code, "after%s:\n", name);
  // after<function_label>:

  // TODO: Free context;
  cg_context = cg_context->parent;

  return ok;
}

Error codegen_program_x86_64_mswin(FILE *code, CodegenContext* cg_context, ParsingContext *context, Node *program) {
  Error err = ok;

  Register *r = register_create("%rax");
  register_add(r, "%r10");
  register_add(r, "%r11");
  register_add(r, "%rbx");
  register_add(r, "%rdi");
  register_add(r, "%rsi");

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
    err = codegen_expression_x86_64_mswin(code, r, cg_context, context, &next_child_context, expression);
    if (err.type) { return err; }
    register_deallocate(r, expression->result_register);
    last_expression = expression;
    expression = expression->next_child;
  }

  // TODO: Copy this code to the generic function for return value!
  char *name = register_name(r,last_expression->result_register);
  if (strcmp(name, "%rax") != 0) {
    fprintf(code, "mov %s, %%rax\n", name);
  }

  fprintf(code, "add $%lld, %%rsp\n", -cg_context->locals_offset);
  fprintf(code, "%s", function_footer_x86_64);

  // TODO: This breaks things, but we should do it.
  //register_free(r);

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
  CodegenContext *cg_context = codegen_context_create(NULL);
  // Open file for writing.
  FILE *code = fopen(filepath, "w");
  if (!code) {
    printf("Filepath: \"%s\"\n", filepath);
    ERROR_PREP(err, ERROR_GENERIC, "codegen_program(): fopen failed to open file at path.");
    return err;
  }
  if (format == CG_FMT_DEFAULT || format == CG_FMT_x86_64_MSWIN) {
    err = codegen_program_x86_64_mswin(code, cg_context, context, program);
  } else {
    printf("ERROR: Unrecognized codegen format\n");
  }
  fclose(code);
  return err;
}
