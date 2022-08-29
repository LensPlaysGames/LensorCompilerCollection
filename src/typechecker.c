#include <typechecker.h>

#include <error.h>
#include <environment.h>
#include <parser.h>

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int type_compare(Node *a, Node *b) {
  if (a->type != b->type) { return 0; }
  Node *child_it_a = a->children;
  Node *child_it_b = b->children;
  while (child_it_a && child_it_b) {
    if (type_compare(child_it_a, child_it_b) == 0) {
      return 0;
    }
    child_it_a = child_it_a->next_child;
    child_it_b = child_it_b->next_child;
  }
  if (child_it_a == child_it_b) {
    return 1;
  }
  return 0;
}

Error typecheck_expression
(ParsingContext *context,
 ParsingContext **context_to_enter,
 Node *expression,
 Node *result_type
 )
{
  Error err = ok;
  if (!context || !expression) {
    ERROR_PREP(err, ERROR_ARGUMENTS, "typecheck_expression(): Arguments must not be NULL!");
    return err;
  }
  ParsingContext *context_it = context;
  Node *value = node_allocate();
  Node *tmpnode = node_allocate();
  Node *iterator = NULL;
  Node *result = node_allocate();
  Node *type = node_allocate();

  // TODO: I feel like children should only be checked when parent type is handled.
  // Typecheck all the children of node before typechecking node.
  //Node *child_it = expression->children;
  //while (child_it) {
  //  err = typecheck_expression(context, child_it);
  //  if (err.type) { return err; }
  //  child_it = child_it->next_child;
  //}

  ParsingContext *to_enter;

  switch (expression->type) {
  default:
    break;
  case NODE_TYPE_INTEGER:
    result_type->type = NODE_TYPE_INTEGER;
    Node *sizeof_integer = node_integer(8);
    result_type->children = sizeof_integer;
    break;
  case NODE_TYPE_VARIABLE_ACCESS:
    // Get type symbol from variables environment using variable symbol.
    while (context_it) {
      if (environment_get_by_symbol(*context_it->variables, expression->value.symbol, result_type)) {
        break;
      }
      context_it = context_it->parent;
    }
    if (!context_it) {
      printf("Variable: \"%s\"\n", expression->value.symbol);
      ERROR_PREP(err, ERROR_GENERIC,
                 "Could not get variable within context for variable access return type");
      break;
    }

    // TYPE -> "integer"
    // TYPE -> POINTER
    //         `-- "integer"

    // TYPE -> INT:0
    // TYPE -> POINTER
    //         `-- INT:0

    node_copy(result_type, type);
    iterator = type;
    while (iterator->children) {
      iterator = iterator->children;
    }
    // Get type node from types environment using type symbol.
    err = parse_get_type(context, iterator, tmpnode);
    if (err.type) {
      printf("Variable: \"%s\"\n", expression->value.symbol);
      ERROR_PREP(err, ERROR_GENERIC,
                 "Could not get type definition in current context for variable access");
      return err;
    }
    *iterator = *tmpnode;
    *result_type = *type;
    break;
  case NODE_TYPE_ADDRESSOF:
    // Ensure child is a variable access.
    // TODO: Addressof a Dereference should cancel out.
    if (!expression->children || expression->children->type != NODE_TYPE_VARIABLE_ACCESS) {
      ERROR_PREP(err, ERROR_TYPE,
                 "Addressof operator requires valid variable access following it.");
      return err;
    }
    Node *variable_access_return_type = node_allocate();
    typecheck_expression(context, context_to_enter,
                         expression->children,
                         variable_access_return_type);

    result_type->type = NODE_TYPE_POINTER;
    result_type->children = variable_access_return_type;
    break;
  case NODE_TYPE_DEREFERENCE:
    // Ensure child return type is at least one level of pointer indirection.
    err = typecheck_expression(context, context_to_enter, expression->children, result_type);
    if (err.type) { return err; }
    if (result_type->type != NODE_TYPE_POINTER) {
      print_node(result_type,0);
      ERROR_PREP(err, ERROR_TYPE, "Dereference may only operate on pointers!");
      return err;
    }
    *result_type = *result_type->children;
    break;
  case NODE_TYPE_FUNCTION:
    // Typecheck body of function in proper context.

    // TODO: Handle functions with empty body.

    to_enter = (*context_to_enter)->children;
    Node *body_expression = expression->children->next_child->next_child->children;
    Node *expr_return_type = node_allocate();
    while (body_expression) {
      err = typecheck_expression(*context_to_enter, &to_enter, body_expression, expr_return_type);
      if (err.type) { return err; }
      body_expression = body_expression->next_child;
    }

    // Compare return type of function to return type of last
    // expression in the body.
    Node *function_return_type = node_allocate();
    node_copy(expression->children->next_child, function_return_type);
    Node *function_return_type_it = function_return_type;
    while (function_return_type_it->children) {
      function_return_type_it = function_return_type_it->children;
    }
    parse_get_type(*context_to_enter, function_return_type_it, result);
    if (type_compare(result, expr_return_type) == 0) {
      ERROR_PREP(err, ERROR_TYPE, "Return type of last expression in function does not match function return type.");
      return err;
    }

    result_type->type = NODE_TYPE_FUNCTION;

    *context_to_enter = (*context_to_enter)->next_child;

    free(expr_return_type);
    break;
  case NODE_TYPE_VARIABLE_REASSIGNMENT:
    // Get return type of left hand side variable, dereference adjusted.
    err = typecheck_expression(context, context_to_enter,
                               expression->children, result_type);
    if (err.type) { return err; }

    //printf("\n");
    //printf("LHS\n");
    //print_node(expression->children,0);
    //printf("LHS return type (dereference adjusted pointer type)\n");
    //print_node(result_type,0);

    // Get return type of right hand side expression.
    Node *rhs_return_value = node_allocate();
    err = typecheck_expression(context, context_to_enter,
                               expression->children->next_child,
                               rhs_return_value);
    if (err.type) { return err; }

    //printf("RHS\n");
    //print_node(expression->children->next_child,0);
    //printf("RHS return type\n");
    //print_node(rhs_return_value,0);

    if (type_compare(result_type, rhs_return_value) == 0) {
      printf("Expression:\n");
      print_node(expression,0);
      printf("LHS TYPE:\n");
      print_node(result_type,2);
      printf("RHS TYPE:\n");
      print_node(rhs_return_value,2);
      ERROR_PREP(err, ERROR_TYPE, "Type of LHS of variable reassignment must match RHS return type.");

      free(rhs_return_value);
      return err;
    }
    free(rhs_return_value);
    break;
  case NODE_TYPE_BINARY_OPERATOR:
    // Get global context.
    while (context_it->parent) { context_it = context_it->parent; }
    // Get binary operator definition from global context into `value`.
    environment_get_by_symbol(*context_it->binary_operators,
                              expression->value.symbol,
                              value);
    // Get return type of LHS into `type`.
    err = typecheck_expression(context, context_to_enter, expression->children, type);
    if (err.type) { return err; }
    // Get expected return type of LHS into `tmpnode`.
    err = parse_get_type(context,
                         value->children->next_child->next_child,
                         tmpnode);
    if (err.type) { return err; }
    if (type_compare(type, tmpnode) == 0) {
      print_node(expression,0);
      ERROR_PREP(err, ERROR_TYPE,
                 "Return type of left hand side expression of binary operator does not match declared left hand side return type");
      return err;
    }
    // Get return type of RHS into `type`.
    err = typecheck_expression(context, context_to_enter, expression->children->next_child, type);
    if (err.type) { return err; }
    // Get expected return type of RHS into `tmpnode`.
    err = parse_get_type(context,
                         value->children->next_child->next_child->next_child,
                         tmpnode);
    if (err.type) { return err; }
    if (type_compare(type, tmpnode) == 0) {
      print_node(expression,0);
      ERROR_PREP(err, ERROR_TYPE,
                 "Return type of right hand side expression of binary operator does not match declared right hand side return type");
      return err;
    }
    *result_type = *value->children->next_child;
    break;
  case NODE_TYPE_FUNCTION_CALL:
    // Ensure function call arguments are of correct type.
    // Get function info from functions environment.
    while (context_it) {
      if (environment_get(*context_it->functions, expression->children, value)) {
        break;
      }
      context_it = context_it->parent;
    }
    //print_node(value,0);
    iterator = expression->children->next_child->children;
    Node *expected_parameter = value->children->children;

    //printf("Iterator:\n");
    //print_node(iterator,2);
    //printf("Expected parameter:\n");
    //print_node(expected_parameter,2);

    while (iterator && expected_parameter) {
      // Get return type of given parameter.
      err = typecheck_expression(context, context_to_enter, iterator, type);
      if (err.type) { return err; }

      // Expected type symbol of parameter found in expected_parameter->children->next_child.
      Node *expected_type_symbol = expected_parameter->children->next_child;
      Node *type_result_it = result;
      while (expected_type_symbol->children && expected_type_symbol->type != NODE_TYPE_SYMBOL) {
        result->type = NODE_TYPE_POINTER;

        Node *one_more_level_of_pointer_indirection = node_allocate();
        one_more_level_of_pointer_indirection->type = NODE_TYPE_POINTER;
        type_result_it->children = one_more_level_of_pointer_indirection;
        type_result_it = one_more_level_of_pointer_indirection;

        expected_type_symbol = expected_type_symbol->children;
      }
      err = parse_get_type(context, expected_type_symbol, type_result_it);
      if (err.type) { return err; }

      //print_node(type,2);
      //printf("\n");
      //print_node(result,2);
      //printf("\n");
      //print_node(expected_type_symbol,2);

      if (type_compare(result, type) == 0) {
        printf("Function:%s\n", expression->children->value.symbol);
        printf("Invalid argument:\n");
        print_node(iterator, 2);
        printf("Expected argument:\n");
        print_node(expected_parameter, 2);
        ERROR_PREP(err, ERROR_TYPE, "Argument type does not match declared parameter type");
        return err;
      }

      iterator = iterator->next_child;
      expected_parameter = expected_parameter->next_child;
    }
    if (expected_parameter != NULL) {
      printf("Function:%s\n", expression->children->value.symbol);
      printf("Expected argument:\n");
      print_node(expected_parameter, 2);
      ERROR_PREP(err, ERROR_ARGUMENTS, "Not enough arguments passed to function!");
      break;
    }
    if (iterator != NULL) {
      printf("Function:%s\n", expression->children->value.symbol);
      ERROR_PREP(err, ERROR_ARGUMENTS, "Too many arguments passed to function!");
      break;
    }

    // Set `func_return_type` to return type symbol of called function.
    Node *func_return_type = node_allocate();
    node_copy(value->children->next_child, func_return_type);
    Node *func_return_type_it = func_return_type;
    while (func_return_type_it->children) {
      func_return_type_it = func_return_type_it->children;
    }

    // TODO/FIXME:
    // Is context_to_enter correct, like, at all? NO!
    // Do we need some link between function definition and function context? Maybe!
    // It's like we need to know which context function was defined within
    // to properly check that return type is valid or get it at all, really.

    err = parse_get_type(context, func_return_type_it, result);
    if (err.type) { return err; }
    *func_return_type_it = *result;
    node_copy(func_return_type, result_type);

    //printf("RESULT:\n");
    //print_node(result,0);
    //printf("resolved func return type:\n");
    //print_node(func_return_type,0);

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
  Node *type = node_allocate();
  ParsingContext *to_enter = context->children;
  while (expression) {
    err = typecheck_expression(context, &to_enter, expression, type);
    if (err.type) { return err; }
    expression = expression->next_child;
  }
  return err;
}
