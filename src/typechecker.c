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

Error expression_return_type
(ParsingContext *context,
 ParsingContext **context_to_enter,
 Node *expression,
 Node *type
 )
{
  Error err = ok;
  ParsingContext *original_context = context;
  Node *tmpnode = node_allocate();
  Node *iterator = NULL;
  type->type = -1;
  switch (expression->type) {
  default:
    type->type = expression->type;
    break;
    case NODE_TYPE_INTEGER:
      // FIXME: Lookup in types env. instead!
      type->type = NODE_TYPE_INTEGER;
      type->children = node_allocate();
      type->children->type = NODE_TYPE_INTEGER;
      //type->children->value.integer = 8;
      //  iterator = node_symbol("integer");
      //  err = parse_get_type(context, iterator, type);
      //  if (err.type) { return err; }
      //  node_free(iterator);
      break;
  case NODE_TYPE_BINARY_OPERATOR:
    // Typecheck this operator as well!
    err = typecheck_expression(context, context_to_enter, expression);
    if (err.type) { break; }
    while (context->parent) { context = context->parent; }
    // FIXME: Handle return value!
    environment_get_by_symbol(*context->binary_operators,
                              expression->value.symbol,
                              tmpnode);
    err = parse_get_type(original_context,
                         tmpnode->children->next_child,
                         type);
    break;
  case NODE_TYPE_FUNCTION_CALL:
    while (context) {
      if (environment_get(*context->functions, expression->children, tmpnode)) {
        break;
      }
      context = context->parent;
    }
    // TMPNODE contains a function node.
    err = parse_get_type(original_context, tmpnode->children->next_child, type);
    if (err.type) { return err; }
    break;
  case NODE_TYPE_ADDRESSOF:
    err = typecheck_expression(context, context_to_enter, expression);
    if (err.type) { return err; }
    // tmpnode contains return type of variable access.
    err = expression_return_type(context, context_to_enter, expression->children, tmpnode);
    if (err.type) { return err; }
    type->type = NODE_TYPE_POINTER;
    type->children = node_allocate();
    node_copy(tmpnode, type->children);
    node_add_child(type, iterator);
    break;
  case NODE_TYPE_DEREFERENCE:
    err = typecheck_expression(context, context_to_enter, expression);
    if (err.type) { return err; }
    err = typecheck_expression(context, context_to_enter, expression->children);
    if (err.type) { return err; }
    err = expression_return_type(context, context_to_enter, expression->children, type);
    if (err.type) { return err; }
    memcpy(type, type->children, sizeof(Node));
    break;
  case NODE_TYPE_VARIABLE_ACCESS:
    //parse_context_print(context,0);
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

    node_copy(tmpnode, type);
    // TYPE -> "integer"
    // TYPE -> POINTER
    //         `-- "integer"

    // TYPE -> POINTER
    //         `-- INT:0

    iterator = type;
    while (iterator->children) {
      iterator = iterator->children;
    }
    // Get type node from types environment using type symbol.
    context = original_context;
    while (context) {
      if (environment_get(*context->types, iterator, tmpnode)) {
        break;
      }
      context = context->parent;
    }
    if (!context) {
      printf("Variable: \"%s\"\n", expression->value.symbol);
      printf("Type: \"%s\"\n", iterator->value.symbol);
      ERROR_PREP(err, ERROR_GENERIC,
                 "Could not get type within context for variable access return type");
      break;
    }
    *iterator = *tmpnode;
    break;
  }
  free(tmpnode);
  return err;
}

Error typecheck_expression
(ParsingContext *context,
 ParsingContext **context_to_enter,
 Node *expression
 )
{
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

  // @a

  // DEREFERENCE
  // `-- VAR. ACCESS ("a")

  switch (expression->type) {
  default:
    break;
  case NODE_TYPE_ADDRESSOF:
    // Ensure child is a variable access.
    // TODO: Addressof a Dereference should cancel out.
    if (expression->children->type != NODE_TYPE_VARIABLE_ACCESS) {
      ERROR_PREP(err, ERROR_TYPE, "Addressof operator requires valid variable access following it.");
      return err;
    }
    break;
  case NODE_TYPE_DEREFERENCE:
    // Ensure child return type is at least one level of pointer indirection.
    err = expression_return_type(context, context_to_enter, expression->children, type);
    if (type->type != NODE_TYPE_POINTER) {
      print_node(type,0);
      ERROR_PREP(err, ERROR_TYPE, "Dereference may only operate on pointers!");
      return err;
    }
    break;
  case NODE_TYPE_FUNCTION:
    // Typecheck body of function in proper context.

    // TODO: Handle functions with empty body.

    to_enter = (*context_to_enter)->children;
    Node *body_expression = expression->children->next_child->next_child->children;
    Node *last_expression = body_expression;
    while (body_expression) {
      err = typecheck_expression(*context_to_enter, &to_enter, body_expression);
      if (err.type) { return err; }
      last_expression = body_expression;
      body_expression = body_expression->next_child;
    }

    // TODO: Compare return type of function to return type of last
    // expression in the body.
    if (last_expression) {
      Node *return_type_id = expression->children->next_child;
      while (return_type_id->type != NODE_TYPE_SYMBOL) {
        return_type_id = return_type_id->children;
      }
      Node *return_type = node_allocate();
      parse_get_type(*context_to_enter, return_type_id, return_type);

      Node *last_type = node_allocate();
      err = expression_return_type(*context_to_enter, &to_enter, last_expression, last_type);
      if (err.type) { return err; }

      if (type_compare(return_type, last_type) == 0) {
        ERROR_PREP(err, ERROR_TYPE, "Return type of last expression in function does not match function return type.");
        return err;
      }

    }

    *context_to_enter = (*context_to_enter)->next_child;
    break;
  case NODE_TYPE_VARIABLE_REASSIGNMENT:
    // Get type of left hand side variable, dereference adjusted.
    err = expression_return_type(context, context_to_enter,
                                 expression->children, tmpnode);
    if (err.type) { return err; }

    // tmpnode now contains expected type of RHS.

    //printf("\n");
    //printf("LHS\n");
    //print_node(expression->children,0);
    //printf("LHS return type (dereference adjusted pointer type)\n");
    //print_node(tmpnode,0);

    // Get return type of right hand side expression.
    err = expression_return_type(context, context_to_enter,
                                 expression->children->next_child,
                                 result);
    if (err.type) { return err; }

    if (type_compare(tmpnode, result) == 0) {
      printf("Expression:\n");
      print_node(expression,0);
      printf("LHS TYPE:\n");
      print_node(tmpnode,2);
      printf("RHS TYPE:\n");
      print_node(result,2);
      ERROR_PREP(err, ERROR_TYPE, "Type of LHS of variable reassignment must match RHS return type.");
      return err;
    }

    break;
  case NODE_TYPE_BINARY_OPERATOR:
    while (context->parent) { context = context->parent; }
    environment_get_by_symbol(*context->binary_operators,
                              expression->value.symbol,
                              value);
    // Get return type of LHS.
    err = expression_return_type(original_context, context_to_enter, expression->children, type);
    if (err.type) { return err; }
    // Get expected return type of LHS in tmpnode->type.
    err = parse_get_type(original_context,
                         value->children->next_child->next_child,
                         tmpnode);
    if (err.type) { return err; }
    if (type_compare(type, tmpnode) == 0) {
      print_node(expression,0);
      ERROR_PREP(err, ERROR_TYPE,
                 "Return type of left hand side expression of binary operator does not match declared left hand side return type");
      return err;
    }
    // Get return type of RHS in type integer.
    err = expression_return_type(original_context, context_to_enter, expression->children->next_child, type);
    if (err.type) { return err; }
    // Get expected return type of RHS in tmpnode->type.
    err = parse_get_type(original_context,
                         value->children->next_child->next_child,
                         tmpnode);
    if (err.type) { return err; }
    if (type_compare(type, tmpnode) == 0) {
      print_node(expression,0);
      ERROR_PREP(err, ERROR_TYPE,
                 "Return type of right hand side expression of binary operator does not match declared right hand side return type");
      return err;
    }
    break;
  case NODE_TYPE_FUNCTION_CALL:
    // Ensure function call arguments are of correct type.
    // Get function info from functions environment.
    while (context) {
      if (environment_get(*context->functions, expression->children, value)) {
        break;
      }
      context = context->parent;
    }
    //print_node(value,0);
    iterator = expression->children->next_child->children;
    tmpnode = value->children->children;

    //printf("Iterator:\n");
    //print_node(iterator,2);
    //printf("Tmpnode:\n");
    //print_node(tmpnode,2);

    while (iterator && tmpnode) {
      // Get return type of given parameter.
      err = expression_return_type(original_context, context_to_enter, iterator, type);
      if (err.type) { return err; }
      // Expected type symbol of parameter found in tmpnode->children->next_child.
      err = parse_get_type(context, tmpnode->children->next_child, result);
      if (err.type) { return err; }
      if (type_compare(result, type) == 0) {
        printf("Function:%s\n", expression->children->value.symbol);
        printf("Invalid argument:\n");
        print_node(iterator, 2);
        printf("Expected argument:\n");
        print_node(tmpnode, 2);
        ERROR_PREP(err, ERROR_TYPE, "Argument type does not match declared parameter type");
        return err;
      }
      iterator = iterator->next_child;
      tmpnode = tmpnode->next_child;
    }
    if (tmpnode != NULL) {
      printf("Expected argument:\n");
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
  ParsingContext *to_enter = context->children;
  while (expression) {
    err = typecheck_expression(context, &to_enter, expression);
    if (err.type) { return err; }
    expression = expression->next_child;
  }

  return err;
}
