#include <typechecker.h>

#include <error.h>
#include <environment.h>
#include <parser.h>

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

int expression_return_type(ParsingContext *context, Node *expression) {
  Node *result;
  switch (expression->type) {
  default:
    return expression->type;
  case NODE_TYPE_FUNCTION_CALL:
    while (context) {
      if (environment_get(*context->functions, expression->children, result)) {
        break;
      }
      context = context->parent;
    }
    // RESULT contains a function node.

    print_node(result,0);
    break;
  }
  return result->type;
}

Error typecheck_expression(ParsingContext *context, Node *expression) {
  Error err = ok;
  Node *value = node_allocate();
  Node *tmpnode = node_allocate();
  Node *iterator = node_allocate();
  Node *result = node_allocate();
  int type = NODE_TYPE_NONE;
  switch (expression->type) {
  default:
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
      err = parse_get_type(context, tmpnode->children->next_child, result);
      if (err.type) { break; }
      if (expression_return_type(context, iterator) != result->type) {
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
      ERROR_CREATE(err, ERROR_ARGUMENTS, "Too many arguments passed to function!");
      break;
    }
    break;
  }
  free(result);
  free(value);
  free(tmpnode);
  free(iterator);
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
