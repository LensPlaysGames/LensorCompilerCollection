#include <typechecker.h>

#include <error.h>
#include <environment.h>
#include <parser.h>

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

Error expression_return_type(ParsingContext *context, Node *expression, int *type) {
  Error err = ok;
  ParsingContext *original_context = context;
  Node *tmpnode = node_allocate();
  Node *result = node_allocate();
  result->type = -1;
  *type = result->type;
  switch (expression->type) {
  default:
    result->type = expression->type;
    break;
  case NODE_TYPE_BINARY_OPERATOR:
    // Typecheck this operator as well!
    err = typecheck_expression(context, expression);
    if (err.type) { break; }
    while (context->parent) { context = context->parent; }
    environment_get_by_symbol(*context->binary_operators,
                              expression->value.symbol,
                              result);
    err = parse_get_type(original_context,
                         result->children->next_child,
                         result);
    break;
  case NODE_TYPE_FUNCTION_CALL:
    while (context) {
      if (environment_get(*context->functions, expression->children, result)) {
        break;
      }
      context = context->parent;
    }
    // RESULT contains a function node.
    err = parse_get_type(original_context, result->children->next_child, result);
    break;
  case NODE_TYPE_VARIABLE_ACCESS:
    parse_context_print(context,0);
    // Get type symbol from variables environment using variable symbol.
    while (context) {
      if (environment_get_by_symbol(*context->variables, expression->value.symbol, tmpnode)) {
        break;
      }
      context = context->parent;
    }
    if (!context) {
      printf("Variable: \"%s\"\n", expression->value.symbol);
      ERROR_PREP(err, ERROR_GENERIC,
                 "Could not get variable within context for variable access return type");
      break;
    }
    // Get type integer from types environment using type symbol.
    context = original_context;
    while (context) {
      if (environment_get(*context->types, tmpnode, result)) {
        break;
      }
      context = context->parent;
    }
    if (!context) {
      printf("Type: \"%s\"\n", result->value.symbol);
      ERROR_PREP(err, ERROR_GENERIC,
                 "Could not get type within context for variable access return type");
      break;
    }
    break;
  }
  free(tmpnode);
  *type = result->type;
  free(result);
  return err;
}

Error typecheck_expression(ParsingContext *context, Node *expression) {
  Error err = ok;
  if (!context || !expression) {
    ERROR_PREP(err, ERROR_ARGUMENTS, "typecheck_expression(): Arguments must not be NULL!");
    return err;
  }
  ParsingContext *original_context = context;
  Node *value = node_allocate();
  Node *tmpnode = node_allocate();
  Node *iterator = NULL;
  Node *result = node_allocate();
  int type = NODE_TYPE_NONE;

  // TODO: I feel like children should only be checked when parent type is handled.
  // Typecheck all the children of node before typechecking node.
  //Node *child_it = expression->children;
  //while (child_it) {
  //  err = typecheck_expression(context, child_it);
  //  if (err.type) { return err; }
  //  child_it = child_it->next_child;
  //}

  switch (expression->type) {
  default:
    break;
  case NODE_TYPE_BINARY_OPERATOR:
    while (context->parent) { context = context->parent; }
    environment_get_by_symbol(*context->binary_operators,
                              expression->value.symbol,
                              value);
    // Get return type of LHS in type integer.
    err = expression_return_type(original_context, expression->children, &type);
    if (err.type) { return err; }
    // Get expected return type of LHS in tmpnode->type.
    err = parse_get_type(original_context,
                         value->children->next_child->next_child,
                         tmpnode);
    if (err.type) { return err; }
    if (type != tmpnode->type) {
      print_node(expression,0);
      ERROR_PREP(err, ERROR_TYPE,
                 "Return type of left hand side expression of binary operator does not match declared left hand side return type");
      return err;
    }
    // Get return type of RHS in type integer.
    err = expression_return_type(original_context, expression->children->next_child, &type);
    if (err.type) { return err; }
    // Get expected return type of RHS in tmpnode->type.
    err = parse_get_type(original_context,
                         value->children->next_child->next_child,
                         tmpnode);
    if (err.type) { return err; }
    if (type != tmpnode->type) {
      print_node(expression,0);
      ERROR_PREP(err, ERROR_TYPE,
                 "Return type of right hand side expression of binary operator does not match declared right hand side return type");
      return err;
    }
    break;
  case NODE_TYPE_FUNCTION_CALL:
    // TODO: Ensure function call arguments are of correct type.
    //       Get function info from functions environment.
    while (context) {
      if (environment_get(*context->functions, expression->children, value)) {
        break;
      }
      context = context->parent;
    }
    //print_node(value,0);
    iterator = expression->children->next_child->children;
    tmpnode = value->children->children;
    while (iterator && tmpnode) {
      err = parse_get_type(original_context, tmpnode->children->next_child, result);
      if (err.type) { break; }
      err = expression_return_type(original_context, iterator, &type);
      if (err.type) { return err; }
      if (type != result->type) {
        printf("Function:%s\n", expression->children->value.symbol);
        ERROR_PREP(err, ERROR_TYPE, "Argument type does not match declared parameter type");
        break;
      }

      iterator = iterator->next_child;
      tmpnode = tmpnode->next_child;
    }
    if (tmpnode != NULL) {
      printf("Function:%s\n", expression->children->value.symbol);
      ERROR_PREP(err, ERROR_ARGUMENTS, "Not enough arguments passed to function!");
      break;
    }
    if (iterator != NULL) {
      printf("Function:%s\n", expression->children->value.symbol);
      ERROR_PREP(err, ERROR_ARGUMENTS, "Too many arguments passed to function!");
      break;
    }
    break;
  }
  free(result);
  free(value);
  free(tmpnode);
  return err;
}

Error typecheck_program(ParsingContext *context, Node *program) {
  Error err = ok;

  Node *expression = program->children;
  while (expression) {
    err = typecheck_expression(context, expression);
    if (err.type) { return err; }
    expression = expression->next_child;
  }

  return err;
}
