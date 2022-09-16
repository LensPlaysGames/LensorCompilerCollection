#include <typechecker.h>

#include <error.h>
#include <environment.h>
#include <parser.h>

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TYPE_NODE_TEXT_BUFFER_SIZE 1024
char type_node_text_buffer[TYPE_NODE_TEXT_BUFFER_SIZE];
char *type_node_text(Node *type) {
  static size_t offset = 0;
  char *outstring = type_node_text_buffer + offset;
  for (unsigned i = 0; i < type->pointer_indirection; ++i) {
    offset += snprintf(type_node_text_buffer + offset,
                       TYPE_NODE_TEXT_BUFFER_SIZE - offset,
                       "@");
  }
  offset += snprintf(type_node_text_buffer + offset,
                     TYPE_NODE_TEXT_BUFFER_SIZE - offset,
                     "%s", type->value.symbol);
  offset += 1;
  if (offset >= TYPE_NODE_TEXT_BUFFER_SIZE) {
    offset = 0;
  }
  return outstring;
}

void print_type_node(Node *type, size_t indent) {
  size_t indent_it = indent;
  while (indent_it--) {
    putchar(' ');
  }
  printf("%s\n", type_node_text(type));
}

int type_compare(Node *a, Node *b) {
  if (a->type != b->type
      || a->pointer_indirection != b->pointer_indirection) {
    return 0;
  }
  Node *child_it_a = a->children;
  Node *child_it_b = b->children;
  while (child_it_a && child_it_b) {
    if (type_compare(child_it_a, child_it_b) == 0) {
      return 0;
    }
    child_it_a = child_it_a->next_child;
    child_it_b = child_it_b->next_child;
  }
  if (child_it_a == NULL && child_it_b == NULL) {
    return 1;
  }
  return 0;
}

char type_compare_symbol(Node *a, Node *b) {
  if (!a || !b) {
    printf("DEVELOPER WARNING: type_compare_symbol() called with NULL args!\n");
    return 0;
  }
  if (a->type != NODE_TYPE_SYMBOL || b->type != NODE_TYPE_SYMBOL) {
    printf("DEVELOPER WARNING: type_compare_symbol() called on non-symbol nodes!\n");
    return 0;
  }
  if (a->pointer_indirection != b->pointer_indirection
      || strcmp(a->value.symbol, b->value.symbol) != 0) {
    return 0;
  }
  Node *a_child = a->children;
  Node *b_child = b->children;
  while (a_child && b_child) {
    if (type_compare_symbol(a_child, b_child) == 0) {
      return 0;
    }
    a_child = a_child->next_child;
    b_child = b_child->next_child;
  }
  if (a_child != b_child) {
    return 0;
  }
  return 1;
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
    printf("DEVELOPER WARNING: Unhandled expression type in typecheck_expression()\n");
    print_node(expression,2);
    break;
  case NODE_TYPE_NONE:
    break;
  case NODE_TYPE_VARIABLE_DECLARATION:
    break;
  case NODE_TYPE_INTEGER:
    if (0) { ; }
    Node *integer_type = node_symbol("integer");
    *result_type = *integer_type;
    free(integer_type);
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
    break;
  case NODE_TYPE_INDEX:
    // Ensure child is a variable access
    if (!expression->children || expression->children->type != NODE_TYPE_VARIABLE_ACCESS) {
      ERROR_PREP(err, ERROR_TYPE, "Index node may only operate on a valid variable access.");
      return err;
    }
    // Ensure variable being accessed is of an array type.
    err = typecheck_expression(context, context_to_enter, expression->children, tmpnode);
    if (strcmp(tmpnode->value.symbol, "array") != 0) {
      ERROR_PREP(err, ERROR_TYPE, "Array index may only operate on variables of array type.");
      return err;
    }
    // Ensure integer value is less than array size.
    // TODO: Expand support for variable access array size.
    if (expression->value.integer < 0 || expression->value.integer >= tmpnode->children->value.integer) {
      ERROR_PREP(err, ERROR_TYPE, "Array index may only operate within bounds of given array.");
      return err;
    }
    *result_type = *tmpnode->children->next_child;
    result_type->pointer_indirection += 1;
    break;
  case NODE_TYPE_ADDRESSOF:
    // Ensure child is a variable access.
    // TODO: Addressof a Dereference should cancel out. Maybe do this during parsing?
    // Or just remove this pattern nodes during optimization.
    if (!expression->children || expression->children->type != NODE_TYPE_VARIABLE_ACCESS) {
      ERROR_PREP(err, ERROR_TYPE,
                 "Addressof operator requires valid variable access following it.");
      return err;
    }
    err = typecheck_expression(context, context_to_enter,
                               expression->children,
                               result_type);
    if (err.type) { return err; }
    result_type->pointer_indirection += 1;
    break;
  case NODE_TYPE_DEREFERENCE:
    // Ensure child return type is at least one level of pointer indirection.

    //printf("\n\n");
    //print_node(expression,0);

    err = typecheck_expression(context, context_to_enter, expression->children, result_type);
    if (err.type) { return err; }
    if (result_type->pointer_indirection == 0) {
      print_node(result_type,0);
      ERROR_PREP(err, ERROR_TYPE, "Dereference may only operate on pointers!");
      return err;
    }
    result_type->pointer_indirection -= 1;
    break;
  case NODE_TYPE_IF:
    if (0) { ; }

    // Enter `if` THEN context.
    to_enter = (*context_to_enter)->children;
    Node *then_expression = expression->children->next_child->children;
    while (then_expression) {
      err = typecheck_expression(*context_to_enter, &to_enter, then_expression, result_type);
      if (err.type) { return err; }
      then_expression = then_expression->next_child;
    }
    // Eat `if` THEN context.
    *context_to_enter = (*context_to_enter)->next_child;

    // When `if` has OTHERWISE...
    if (expression->children->next_child->next_child) {
      // Enter `if` OTHERWISE context.
      to_enter = (*context_to_enter)->children;
      Node *otherwise_expression = expression->children->next_child->next_child->children;
      Node *otherwise_type = node_allocate();
      while (otherwise_expression) {
        err = typecheck_expression(*context_to_enter, &to_enter, otherwise_expression, otherwise_type);
        if (err.type) { return err; }
        otherwise_expression = otherwise_expression->next_child;
      }
      // Eat `if` OTHERWISE context.
      *context_to_enter = (*context_to_enter)->next_child;
      // Enforce that `if` expressions with else bodies must return same type.
      if(type_compare_symbol(result_type, otherwise_type) == 0) {
        printf("THEN type:\n");
        print_node(result_type,2);
        printf("OTHERWISE type:\n");
        print_node(otherwise_type,2);
        ERROR_PREP(err, ERROR_TYPE, "All branches of `if` expression must return same type.");
        return err;
      }
      free(otherwise_type);
    }
    break;
  case NODE_TYPE_FUNCTION:
    // Only handle function body when it exists.
    if (expression->children->next_child->next_child->children) {
      // Typecheck body of function in proper context.
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
      if (type_compare_symbol(expression->children, expr_return_type) == 0) {
        printf("Expected type:\n");
        print_node(expression->children,2);
        printf("Return type of last expression:\n");
        print_node(expr_return_type,2);
        ERROR_PREP(err, ERROR_TYPE, "Return type of last expression in function does not match function return type.");
        return err;
      }

      free(expr_return_type);
    }

    // TODO: Copy result type from function, don't just make it up!
    Node *function_type_symbol = node_symbol("function");
    *result_type = *function_type_symbol;

    // Return type of function as first child.
    result_type->children = node_allocate();
    node_copy(expression->children, result_type->children);
    result_type->children->children = NULL;
    result_type->children->next_child = NULL;

    Node *parameter = expression->children->next_child->children;
    if (parameter) {
      do {
        Node *parameter_type = node_allocate();
        if (parameter->type != NODE_TYPE_VARIABLE_DECLARATION) {
          ERROR_PREP(err, ERROR_TYPE, "Function parameter declaration must be a valid variable declaration!");
          return err;
        }

        err = parse_get_variable(*context_to_enter, parameter->children, parameter_type);
        if (err.type) { return err; }

        node_add_child(result_type, parameter_type);

        parameter = parameter->next_child;
      } while (parameter);
    }

    *context_to_enter = (*context_to_enter)->next_child;

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

    if (type_compare_symbol(result_type, rhs_return_value) == 0) {
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
    // Expected return type of LHS is third child of binary operator definition.
    if (type_compare_symbol(type, value->children->next_child->next_child) == 0) {
      print_node(expression,0);
      ERROR_PREP(err, ERROR_TYPE,
                 "Return type of left hand side expression of binary operator does not match declared left hand side return type");
      return err;
    }
    // Get return type of RHS into `type`.
    err = typecheck_expression(context, context_to_enter, expression->children->next_child, type);
    if (err.type) { return err; }
    // Expected return type of RHS is fourth child of binary operator definition.
    if (type_compare_symbol(type, value->children->next_child->next_child->next_child) == 0) {
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
      if (environment_get(*context_it->variables, expression->children, value)) {
        break;
      }
      context_it = context_it->parent;
    }

    // Ensure variable that is being accessed is of function type.
    if (strcmp(value->value.symbol, "function") != 0 && strcmp(value->value.symbol, "external function") != 0) {
      print_node(expression,0);
      ERROR_PREP(err, ERROR_TYPE, "A called variable must have a function type!");
      return err;
    }

    // Only typecheck parameters when they exist.
    if (value->children->next_child) {
      iterator = expression->children->next_child->children;
      Node *expected_parameter = value->children->next_child;

      //printf("Iterator:\n");
      //print_node(iterator,2);
      //printf("Expected parameter:\n");
      //print_node(expected_parameter,2);

      while (iterator && expected_parameter) {
        // Get return type of given parameter.
        err = typecheck_expression(context, context_to_enter, iterator, type);
        if (err.type) { return err; }
        if (type_compare_symbol(expected_parameter, type) == 0) {
          printf("Function:%s\n", expression->children->value.symbol);
          printf("Invalid argument:\n");
          print_node(iterator, 2);
          printf("Argument return type is `%s` but was expected to be `%s`\n",
                 type_node_text(type),
                 type_node_text(expected_parameter->children->next_child));
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
    }
    *result_type = *value->children;
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
