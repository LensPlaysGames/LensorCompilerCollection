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
                          ".L%zu:\n", label_count);
  label_index++;
  if (label_index >= label_buffer_size) {
    label_index = 0;
    return label_generate();
  }
  label_count++;
  return label;
}

//================================================================ BEG CG_FMT_x86_64_MSWIN

#define symbol_buffer_size 1024
char symbol_buffer[label_buffer_size];
size_t symbol_index = 0;
size_t symbol_count = 0;
char *symbol_to_address(CodegenContext *cg_ctx, Node *symbol) {
  char *symbol_string = symbol_buffer + symbol_index;
  if (!cg_ctx->parent) {
    // Global variable access.
    symbol_index += snprintf(symbol_string,
                             symbol_buffer_size - symbol_index,
                             "%s(%%rip)", symbol->value.symbol);
  } else {
    // Local variable access.
    Node *stack_offset = node_allocate();
    if (!environment_get(*cg_ctx->locals, symbol, stack_offset)) {
      printf("ERROR: Internal compiler error :^(\n");
      return NULL;
    }
    symbol_index += snprintf(symbol_string,
                             symbol_buffer_size - symbol_index,
                             "%lld(%%rbp)", stack_offset->value.integer);
    free(stack_offset);
  }
  symbol_index++;
  if (symbol_index >= label_buffer_size) {
    symbol_index = 0;
    return label_generate();
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
  switch (expression->type) {
  default:
    break;
  case NODE_TYPE_INTEGER:
    expression->result_register = register_allocate(r);
    fprintf(code, "mov $%lld, %s\n",
            expression->value.integer,
            register_name(r, expression->result_register));
    break;
  case NODE_TYPE_FUNCTION_CALL:
    // Push arguments *in reverse order* on to the stack.
    // TODO: In reverse order. Or just calculate same when accessing.
    iterator = expression->children->next_child->children;
    while (iterator) {
      err = codegen_expression_x86_64_mswin
        (code, r, cg_context, context, next_child_context, iterator);
      fprintf(code, "pushq %s\n", register_name(r, iterator->result_register));
      register_deallocate(r, iterator->result_register);
      iterator = iterator->next_child;
      count++;
    }
    // Emit call
    fprintf(code, "call %s\n", expression->children->value.symbol);
    fprintf(code, "add $%lld, %%rsp\n", count * 8);
    break;
  case NODE_TYPE_FUNCTION:
    if (!cg_context->parent) { break; }
    // TODO: Keep track of local lambda label in environment or something.
    result = label_generate();
    err = codegen_function_x86_64_att_asm_mswin(r, cg_context, context, next_child_context,
                                                result, expression, code);
    if (err.type) { break; }
    break;
  case NODE_TYPE_BINARY_OPERATOR:
    while (context->parent) { context = context->parent; }
    // FIXME: Second argument is memory leaked! :^(
    environment_get(*context->binary_operators, node_symbol(expression->value.symbol), tmpnode);

    //printf("Codegenning binary operator %s\n", expression->value.symbol);
    //print_node(tmpnode,0);

    err = codegen_expression_x86_64_mswin(code, r, cg_context,
                                          context, next_child_context,
                                          expression->children);
    err = codegen_expression_x86_64_mswin(code, r, cg_context,
                                          context, next_child_context,
                                          expression->children->next_child);

    if (strcmp(expression->value.symbol, "+") == 0) {
      // https://www.felixcloutier.com/x86/add

      // Use right hand side result register as our result since ADD is destructive!
      expression->result_register = expression->children->next_child->result_register;

      fprintf(code, "add %s, %s\n",
              register_name(r, expression->children->result_register),
              register_name(r, expression->children->next_child->result_register));

      // Free no-longer-used left hand side result register.
      register_deallocate(r, expression->children->result_register);
    } else if (strcmp(expression->value.symbol, "-") == 0) {
      // https://www.felixcloutier.com/x86/sub

      // Use right hand side result register as our result since ADD is destructive!
      expression->result_register = expression->children->next_child->result_register;

      fprintf(code, "sub %s, %s\n",
              register_name(r, expression->children->result_register),
              register_name(r, expression->children->next_child->result_register));

      // Free no-longer-used left hand side result register.
      register_deallocate(r, expression->children->result_register);
    } else if (strcmp(expression->value.symbol, "*") == 0) {
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
      // TODO: Division codegen!

      // https://www.felixcloutier.com/x86/div
      // https://www.felixcloutier.com/x86/idiv

      // Quotient is in RAX, Remainder in RDX; we must save and
      // restore these registers before and after divide, sadly.
    }

    break;
  case NODE_TYPE_VARIABLE_ACCESS:
    expression->result_register = register_allocate(r);
    if (!cg_context->parent) {
      // Global variable
      fprintf(code, "mov %s(%%rip), %s\n",
              expression->value.symbol,
              register_name(r, expression->result_register));
    } else {
      // Local variable
      // TODO: Store base pointer offset from parent base pointer offset,
      //       if possible. This would allow for us to access any local
      //       variable in any parent scope.
      //while (cg_context) {
      //  if (environment_get(*cg_context->locals, expression, tmpnode)) {
      //    break;
      //  }
      //  cg_context = cg_context->parent;
      //}
      //if (!cg_context) {
      //  printf("Variable: \"%s\"\n", expression->value.symbol);
      //  ERROR_PREP(err, ERROR_GENERIC, "Could not get local base pointer offset of variable");
      //  return err;
      //}
      if (!environment_get_by_symbol(*cg_context->locals, expression->value.symbol, tmpnode)) {
        printf("Variable: \"%s\"\n", expression->value.symbol);
        ERROR_PREP(err, ERROR_GENERIC, "Could not get local base pointer offset of variable");
        return err;
      }
      fprintf(code, "mov %lld(%%rbp), %s\n",
              tmpnode->value.integer,
              register_name(r, expression->result_register));
    }
    break;
  case NODE_TYPE_VARIABLE_DECLARATION:
    if (!cg_context->parent) { break; }
    // Allocate space on stack
    //   Get the size in bytes of the type of the variable
    while (context) {
      if (environment_get(*context->variables, expression->children, tmpnode)) { break; }
      context = context->parent;
    }
    err = parse_get_type(context, tmpnode, tmpnode);
    if (err.type) { return err; }
    //   Subtract type size in bytes from stack pointer
    fprintf(code, "sub $%lld, %%rsp\n", tmpnode->children->value.integer);
    // Keep track of RBP offset.
    cg_context->locals_offset -= tmpnode->children->value.integer;
    //   Kept in codegen context.
    environment_set(cg_context->locals, expression->children, node_integer(cg_context->locals_offset));
    break;
  case NODE_TYPE_VARIABLE_REASSIGNMENT:
    if (cg_context->parent) {
      err = codegen_expression_x86_64_mswin(code, r, cg_context, context, next_child_context,
                                            expression->children->next_child);
      if (err.type) { break; }
      result = register_name(r, expression->children->next_child->result_register);
      fprintf(code, "mov %s, %s\n", result, symbol_to_address(cg_context, expression->children));
      register_deallocate(r, expression->children->next_child->result_register);
    } else {
      // Global variable reassignment
      // Very simple optimization to handle plain integer node assignment.
      if (expression->children->next_child->type == NODE_TYPE_INTEGER) {
        result = malloc(64);
        if (!result) {
          ERROR_PREP(err, ERROR_GENERIC, "Could not allocate integer result string buffer :^(");
          break;
        }
        snprintf(result, 64, "$%lld", expression->children->next_child->value.integer);
        fprintf(code, "movq %s, %s\n", result, symbol_to_address(cg_context, expression->children));
        free(result);
      } else {
        err = codegen_expression_x86_64_mswin(code, r, cg_context, context, next_child_context,
                                              expression->children->next_child);
        if (err.type) { break; }
        result = register_name(r, expression->children->next_child->result_register);
        fprintf(code, "mov %s, %s\n", result, symbol_to_address(cg_context, expression->children));
        register_deallocate(r, expression->children->next_child->result_register);
      }
    }
    break;
  }
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
  Node *parameter = function->children->children;
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
  ParsingContext *ctx = next_child_context ? *next_child_context : context;
  Node *last_expression = NULL;
  Node *expression = function->children->next_child->next_child->children;
  while (expression) {
    err = codegen_expression_x86_64_mswin(code, r, cg_context, ctx, next_child_context, expression);
    register_deallocate(r, expression->result_register);
    if (err.type) {
      print_error(err);
      return err;
    }
    last_expression = expression;
    expression = expression->next_child;
  }
  if (next_child_context) {
    *next_child_context = (*next_child_context)->next_child;
  }

  // Copy last expression result register to RAX
  if (last_expression) {
    char *name = register_name(r,last_expression->result_register);
    if (strcmp(name, "%rax")) {
      fprintf(code, "mov %s, %%rax\n", name);
    }
  }

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

  fprintf(code, "%s", ".section .data\n");

  // Generate global variables
  Binding *var_it = context->variables->bind;
  while (var_it) {
    Node *var_id = var_it->id;
    Node *type = var_it->value;
    Node *type_info = node_allocate();
    if (!environment_get(*context->types, type, type_info)) {
      printf("Type: \"%s\"\n", type->value.symbol);
      ERROR_PREP(err, ERROR_GENERIC,
                 "Could not get type info from types environment!");
      // TODO/FIXME: Should I return error here?
    }
    var_it = var_it->next;
    fprintf(code, "%s: .space %lld\n", var_id->value.symbol, type_info->children->value.integer);
    free(type_info);
  }

  fprintf(code, ".section .text\n");

  // Generate global functions
  ParsingContext *next_child_context = context->children;
  Binding *function_it = context->functions->bind;
  while (function_it) {
    Node *function_id = function_it->id;
    Node *function = function_it->value;
    function_it = function_it->next;
    err = codegen_function_x86_64_att_asm_mswin(r, cg_context, context, &next_child_context, function_id->value.symbol, function, code);
  }

  fprintf(code,
          ".global main\n"
          "main:\n"
          "%s", function_header_x86_64);

  Node *last_expression = program->children;
  Node *expression = program->children;
  while (expression) {
    codegen_expression_x86_64_mswin(code, r, cg_context, context, &next_child_context, expression);
    register_deallocate(r, expression->result_register);
    last_expression = expression;
    expression = expression->next_child;
  }

  // TODO: Copy this code to the generic function for return value!
  char *name = register_name(r,last_expression->result_register);
  if (strcmp(name, "%rax")) {
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
  }
  fclose(code);
  return err;
}
