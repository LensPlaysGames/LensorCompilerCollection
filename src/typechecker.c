#include <typechecker.h>

#include <error.h>
#include <parser.h>

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DIAG(diag, loc, ...)                                                                  \
  do {                                                                                        \
    issue_diagnostic(diag, (ast)->filename.data, as_span((ast)->source), (loc), __VA_ARGS__); \
    return false;                                                                             \
  } while (0)
#define ERR(loc, ...)   DIAG(DIAG_ERR, loc, __VA_ARGS__)
#define SORRY(loc, ...) DIAG(DIAG_SORRY, loc, __VA_ARGS__)

#define ERR_DO(code, loc, ...)                                                                    \
  do {                                                                                            \
    issue_diagnostic(DIAG_ERR, (ast)->filename.data, as_span((ast)->source), (loc), __VA_ARGS__); \
    code;                                                                                         \
    return false;                                                                                 \
  } while (0)

#define ERR_NOT_CONVERTIBLE(to, from)                                                                                     \
  do {                                                                                                                    \
    string to_str = ast_typename(to, false);                                                                              \
    string from_str = ast_typename(from, false);                                                                          \
    ERR_DO(free(to_str.data); free(from_str.data), from->source_location, "Type \"%.*s\" is not convertible to \"%.*s\"", \
                              (int) from_str.size, from_str.data, (int) to_str.size, to_str.data);                        \
  } while (0)

/// Check if from is convertible to to.
NODISCARD static bool convertible(Node *to, Node *from) {
  TODO();
}

/// Get the common type of two types.
NODISCARD static Node *common_type(Node *a, Node *b) {
  TODO();
}

/// Check if a type is a pointer type.
NODISCARD static bool is_pointer(Node *type) { return type->kind == NODE_TYPE_POINTER; }

/// Check if a type is an array type.
NODISCARD static bool is_array(Node *type) { return type->kind == NODE_TYPE_ARRAY; }

/// Check if a type is an integer type.
NODISCARD static bool is_integer(Node *type) {
  /// Currently, all primitive types are integers.
  return type->kind == NODE_TYPE_PRIMITIVE;
}

NODISCARD bool typecheck_expression(AST *ast, Node *expr) {
  /// Don’t typecheck the same expression twice.
  if (expr->type_checked) return true;

  /// Typecheck the expression.
  switch (expr->kind) {
    /// Typecheck each child of the root.
    case NODE_ROOT: {
      VECTOR_FOREACH_PTR (Node *, node, expr->root.children) {
        if (!typecheck_expression(ast, node))
          return false;
        return true;
      }

      /// Typecheck the function body.
    case NODE_FUNCTION:
      if (!typecheck_expression(ast, expr->function.body)) return false;

      /// Make sure the return type of the body is convertible to that of the function.
      if (!convertible(expr->type->type_function.return_type, expr->function.body->type)) {
        string ret = ast_typename(expr->type->type_function.return_type, false);
        string body = ast_typename(expr->function.body->type, false);
        ERR_DO(free(ret.data); free(body.data), expr->source_location,
          "Type \"%.*s\" of function body is not convertible to return type \"%.*s\".",
            (int) body.size, body.data, (int) ret.size, ret.data);
      }

      return true;

    /// Typecheck declarations.
    case NODE_DECLARATION:
      /// If there is an intialiser, then its type must match the type of the variable.
      if (expr->declaration.init) {
        if (!typecheck_expression(ast, expr->declaration.init)) return false;
        if (!convertible(expr->type, expr->declaration.init->type))
          ERR_NOT_CONVERTIBLE(expr, expr->declaration.init);
      }
      return true;

    /// If expression.
    case NODE_IF:
      if (!typecheck_expression(ast, expr->if_.condition)) return false;
      if (!typecheck_expression(ast, expr->if_.then)) return false;

      /// If the then and else branch of an if expression both exist and have
      /// the a common type, then the type of the if expression is that type.
      if (expr->if_.else_) {
        if (!typecheck_expression(ast, expr->if_.else_)) return false;
        Node *common = common_type(expr->if_.then->type, expr->if_.else_->type);
        if (common) expr->type = common;
        else expr->type = ast->t_void;
      }

      /// Otherwise, the type of the if expression is void.
      else { expr->type = ast->t_void; }
      return true;

    /// A while expression has type void.
    case NODE_WHILE:
      if (!typecheck_expression(ast, expr->while_.condition)) return false;
      if (!typecheck_expression(ast, expr->while_.body)) return false;
      expr->type = ast->t_void;
      return true;

    /// Typecheck all children and set the type of the block
    /// to the type of the last child. TODO: noreturn?
    case NODE_BLOCK: {
      VECTOR_FOREACH_PTR (Node *, node, expr->block.children)
        if (!typecheck_expression(ast, node))
          return false;
      expr->type = VECTOR_BACK_OR(expr->block.children, ast->t_void)->type;
      return true;
    }

    /// First, resolve the function. Then, typecheck all parameters
    /// and set the type to the return type of the callee.
    case NODE_CALL: {
      /// Resolve the function if applicable.
      Node *callee = expr->call.callee;
      if (callee->kind == NODE_FUNCTION_REFERENCE && !callee->funcref->value) {
        Symbol *sym = scope_find_symbol(callee->funcref->scope, as_span(callee->funcref->name), false);
        if (!sym || !sym->value) ERR(callee->source_location, "No such function \"%.*s\".",
          (int) callee->funcref->name.size, callee->funcref->name.data);
        callee->funcref->value = sym->value;
      }

      /// Typecheck the callee.
      if (!typecheck_expression(ast, expr->call.callee)) return false;

      /// Typecheck all arguments.
      VECTOR_FOREACH_PTR (Node *, param, expr->call.arguments)
        if (!typecheck_expression(ast, param))
          return false;

      /// Make sure we have the right number of arguments.
      if (expr->call.arguments.size != callee->type->type_function.parameters.size)
        ERR(callee->source_location, "Expected %zu arguments, got %zu.",
          callee->type->type_function.parameters.size, expr->call.arguments.size);

      /// Make sure all arguments are convertible to the parameter types.
      VECTOR_FOREACH_INDEX (i, expr->call.arguments) {
        Node *param = expr->call.arguments.data[i];
        Node *arg = callee->type->type_function.parameters.data[i];
        if (!convertible(param->type, arg->type)) ERR_NOT_CONVERTIBLE(param, arg);
      }

      /// Set the type of the call to the return type of the callee.
      expr->type = callee->type->type_function.return_type;
      return true;
    }

    /// Make sure a cast is even possible.
    case NODE_CAST: TODO();

    /// Binary expression. This is a complicated one.
    case NODE_BINARY: {
      /// Get this out of the way early.
      Node *const lhs = expr->binary.lhs, *const rhs = expr->binary.rhs;
      if (!typecheck_expression(ast, lhs)) return false;
      if (!typecheck_expression(ast, rhs)) return false;
      
      /// Typecheck the operator.
      switch (expr->binary.op) {
        default: ICE("Invalid binary operator \"%s\".", token_type_to_string(expr->binary.op));

        /// The subscript operator is basically pointer arithmetic.
        case TK_LBRACK:
          /// We can only subscript pointers and arrays.
          if (!is_pointer(lhs->type) && !is_array(lhs->type)) {
            string name = ast_typename(lhs->type, false);
            ERR_DO(free(name.data), lhs->source_location, "Cannot subscript non-pointer, non-array type \"%.*s\".",
                (int) name.size, name.data);
          }

          /// The RHS has to be an integer.
          if (!is_integer(rhs->type)) {
            string name = ast_typename(rhs->type, false);
            ERR_DO(free(name.data), rhs->source_location, "Cannot subscript with non-integer type \"%.*s\".",
                (int) name.size, name.data);
          }

          /// The type of the subscript expression is the type of the pointer/array.
          expr->type = lhs->type;
          return true;

        /// All of these are basically the same when it comes to types.
        case TK_GT:
        case TK_LT:
        case TK_GE:
        case TK_LE:
        case TK_EQ:
        case TK_NE:
          if (!is_integer(lhs->type)) {
            string name = ast_typename(lhs->type, false);
            ERR_DO(free(name.data), lhs->source_location, "Cannot compare non-integer type \"%.*s\".",
                (int) name.size, name.data);
          }

          if (!is_integer(rhs->type)) {
            string name = ast_typename(rhs->type, false);
            ERR_DO(free(name.data), rhs->source_location, "Cannot compare non-integer type \"%.*s\".",
                (int) name.size, name.data);
          }

          /// TODO: Change this to bool if we ever add a bool type.
          expr->type = ast->t_integer;
          return true;

        /// Since pointer arithmetic is handled by the subscript operator,
        /// type checking for these is basically all the same.
        /// TODO: Implicit conversions when we add the `byte` type.
        case TK_PLUS:
        case TK_MINUS:
        case TK_STAR:
        case TK_SLASH:
        case TK_PERCENT:
        case TK_SHL:
        case TK_SHR:
        case TK_AMPERSAND:
        case TK_PIPE:
        case TK_CARET:
          if (!is_integer(lhs->type)) {
            string name = ast_typename(lhs->type, false);
            ERR_DO(free(name.data), lhs->source_location, "Cannot perform arithmetic on non-integer type \"%.*s\".",
                (int) name.size, name.data);
          }

          if (!is_integer(rhs->type)) {
            string name = ast_typename(rhs->type, false);
            ERR_DO(free(name.data), rhs->source_location, "Cannot perform arithmetic on non-integer type \"%.*s\".",
                (int) name.size, name.data);
          }

          expr->type = lhs->type;
          return true;
      }
    }

    /// Here be dragons.
    case NODE_UNARY:
      break;

    case NODE_LITERAL: break;
    case NODE_VARIABLE_REFERENCE: break;
    case NODE_FUNCTION_REFERENCE: break;
    case NODE_TYPE_PRIMITIVE: break;
    case NODE_TYPE_NAMED: break;
    case NODE_TYPE_POINTER: break;
    case NODE_TYPE_ARRAY: break;
    case NODE_TYPE_FUNCTION: break;
  }
}

bool typecheck_expression
(ParsingContext *const context,
 ParsingContext **context_to_enter,
 Node *expression,
 Node *result_type
 )
{
  if (!context || !expression) ICE("typecheck_expression(): Arguments must not be NULL!");
  ParsingContext *context_it = context;
  Node *value    = node_allocate();
  Node *tmpnode  = node_allocate();
  Node *iterator = NULL;
  Node *result   = node_allocate();
  Node *type     = node_allocate();

  ParsingContext *to_enter;

  switch (expression->type) {
  default:
    printf("DEVELOPER WARNING: Unhandled expression type in typecheck_expression()\n");
    print_node(expression,2);
    break;

  case NODE_TYPE_NONE:
  case NODE_TYPE_VARIABLE_DECLARATION: break;

  case NODE_TYPE_INTEGER: {
    Node *integer_type = node_symbol("integer");
    *result_type = *integer_type;
    free(integer_type);
    } break;

  case NODE_TYPE_NOT: {
    Node *not_child_type = node_allocate();
    if (!typecheck_expression(context, context_to_enter, expression->children, not_child_type)) return false;

    Node *integer_type = node_symbol("integer");
    *result_type = *integer_type;
    free(integer_type);

    if (!type_compare_symbol(result_type, not_child_type))
      ERR(expression->source_location, "NOT operator may only operate on integers!");
  } break;

  case NODE_TYPE_VARIABLE_ACCESS:
    // Get type symbol from variables environment using variable symbol.
    while (context_it) {
      if (environment_get_by_symbol(*context_it->variables, expression->value.symbol, result_type)) {
        break;
      }
      context_it = context_it->parent;
    }
    if (!context_it) ICE("Could not get variable within context for variable access return type: \"%s\"\n",
      expression->value.symbol);
    break;

  case NODE_TYPE_INDEX:
    // Ensure child is a variable access
    if (!expression->children || expression->children->type != NODE_TYPE_VARIABLE_ACCESS)
      ERR(expression->source_location, "Index node may only operate on a valid variable access.");

    // Ensure variable being accessed is of an array type.
    if (!typecheck_expression(context, context_to_enter, expression->children, tmpnode)) return false;
    if (strcmp(tmpnode->value.symbol, "array") != 0)
      ERR(expression->source_location, "Array index may only operate on variables of array type.");

    // Ensure integer value is less than array size.
    // TODO: Expand support for variable access array size.
    if (expression->value.integer < 0 || expression->value.integer >= tmpnode->children->value.integer)
      ERR(expression->source_location, "Array index may only operate within bounds of given array.");

    *result_type = *tmpnode->children->next_child;
    result_type->pointer_indirection += 1;
    break;

  case NODE_TYPE_ADDRESSOF:
    // Ensure child is a variable access.
    // TODO: Addressof a Dereference should cancel out. Maybe do this during parsing?
    // Or just remove this pattern nodes during optimization.
    if (!expression->children || expression->children->type != NODE_TYPE_VARIABLE_ACCESS)
      ERR(expression->source_location, "Addressof operator requires valid variable access following it.");

    if (!typecheck_expression(context, context_to_enter,
         expression->children,
         result_type)) return false;
    result_type->pointer_indirection += 1;
    break;

  case NODE_TYPE_DEREFERENCE:
    // Ensure child return type is at least one level of pointer indirection.

    //printf("\n\n");
    //print_node(expression,0);

    if (!typecheck_expression(context, context_to_enter, expression->children, result_type)) return false;
    if (result_type->pointer_indirection == 0)
      ERR(expression->source_location, "Dereference may only operate on pointers!");
    result_type->pointer_indirection -= 1;
    break;

  case NODE_TYPE_IF: {
    // TODO: Typecheck the condition maybe?

    // Enter `if` THEN context.
    to_enter = (*context_to_enter)->children;
    Node *last_then_expression = NULL;
    Node *then_expression = expression->children->next_child->children;
    while (then_expression) {
      if (!typecheck_expression(*context_to_enter, &to_enter, then_expression, result_type)) return false;
      last_then_expression = then_expression;
      then_expression = then_expression->next_child;
    }
    if (!last_then_expression) {
      TODO("Compiler doesn't yet handle empty if expression bodies. Put a zero or something.");
    }

    // Ensure last expression of IF THEN body returns a value.
    if (!node_returns_value(last_then_expression))
      ERR(expression->source_location, "Last expression of if body must return a value.");

    // Eat `if` THEN context.
    *context_to_enter = (*context_to_enter)->next_child;

    // When `if` has OTHERWISE...
    if (expression->children->next_child->next_child) {
      // Enter `if` OTHERWISE context.
      to_enter = (*context_to_enter)->children;
      Node *last_otherwise_expression = NULL;
      Node *otherwise_expression = expression->children->next_child->next_child->children;
      Node *otherwise_type = node_allocate();
      while (otherwise_expression) {
        if (!typecheck_expression(*context_to_enter, &to_enter, otherwise_expression, otherwise_type)) return false;
        last_otherwise_expression = otherwise_expression;
        otherwise_expression = otherwise_expression->next_child;
      }

      if (!last_otherwise_expression)
        SORRY(expression->source_location, "Compiler doesn't yet handle empty if expression bodies.");

      if (!node_returns_value(last_otherwise_expression))
        ERR(otherwise_expression->source_location, "Last expression of else body must return a value.");
      // Eat `if` OTHERWISE context.
      *context_to_enter = (*context_to_enter)->next_child;
      // Enforce that `if` expressions with else bodies must return same type.
      if(type_compare_symbol(result_type, otherwise_type) == 0) {
        printf("THEN type:\n");
        print_node(result_type,2);
        printf("OTHERWISE type:\n");
        print_node(otherwise_type,2);
        ERR(expression->source_location, "All branches of `if` expression must return same type.");
      }
      free(otherwise_type);
    }
  } break;

  case NODE_TYPE_WHILE: {
    // TODO: Typecheck condition, maybe?

    // Enter `while` context.
    to_enter = (*context_to_enter)->children;
    Node *last_while_expression = NULL;
    Node *body_expression = expression->children->next_child->children;
    while (body_expression) {
      if (!typecheck_expression(*context_to_enter, &to_enter, body_expression, result_type)) return false;
      last_while_expression = body_expression;
      body_expression = body_expression->next_child;
    }

    if (!last_while_expression)
      SORRY(expression->source_location, "Compiler doesn't yet handle empty while expression bodies.");

    // Ensure last expression of IF THEN body returns a value.
    if (!node_returns_value(last_while_expression))
      ERR(expression->source_location, "Last expression of while body must return a value.");

    // Eat `while` context.
    *context_to_enter = (*context_to_enter)->next_child;
  } break;

  case NODE_TYPE_FUNCTION:
    // Only handle function body when it exists.
    if (expression->children->next_child->next_child->children) {
      // Typecheck body of function in proper context.
      to_enter = (*context_to_enter)->children;
      Node *body_expression = expression->children->next_child->next_child->children;
      Node *expr_return_type = node_allocate();
      while (body_expression) {
        if (!typecheck_expression(*context_to_enter, &to_enter, body_expression, expr_return_type)) return false;
        body_expression = body_expression->next_child;
      }

      // Compare return type of function to return type of last
      // expression in the body.
      if (type_compare_symbol(expression->children, expr_return_type) == 0) {
        printf("Expected type:\n");
        print_node(expression->children,2);
        printf("Return type of last expression:\n");
        print_node(expr_return_type,2);
        ERR(expression->source_location, "Return type of last expression in function does not match function return type");
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
        if (parameter->type != NODE_TYPE_VARIABLE_DECLARATION)
          ERR(expression->source_location, "Function parameter declaration must be a valid variable declaration");

        if (!parse_get_variable(*context_to_enter, parameter->children, parameter_type, false)) return false;

        Node *param_type_copy = node_allocate();
        node_copy(parameter_type, param_type_copy);
        node_add_child(result_type, param_type_copy);

        parameter = parameter->next_child;
      } while (parameter);
    }

    *context_to_enter = (*context_to_enter)->next_child;
    break;

  case NODE_TYPE_VARIABLE_REASSIGNMENT: {
    // Get return type of left hand side variable, dereference adjusted.
    if (!typecheck_expression(context, context_to_enter,
         expression->children,
         result_type)) return false;

    // TODO: Ensure type of LHS is actually assignable, like not an
    // external function lol.

    //printf("\n");
    //printf("LHS\n");
    //print_node(expression->children,0);
    //printf("LHS return type (dereference adjusted pointer type)\n");
    //print_node(result_type,0);

    // Get return type of right hand side expression.
    Node *rhs_return_value = node_allocate();
    if (!typecheck_expression(context, context_to_enter,
         expression->children->next_child,
         rhs_return_value)) return false;

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
      ERR(expression->source_location, "Type of LHS of variable reassignment must match RHS return type.");
    }
    free(rhs_return_value);

    } break;

  case NODE_TYPE_BINARY_OPERATOR:
    // Get global context.
    while (context_it->parent) { context_it = context_it->parent; }
    // Get binary operator definition from global context into `value`.
    environment_get_by_symbol(*context_it->binary_operators,
                              expression->value.symbol,
                              value);

    // Get return type of LHS into `type`.
    if (!typecheck_expression(context, context_to_enter, expression->children, type)) return false;

    // Expected return type of LHS is third child of binary operator definition.
    if (type_compare_symbol(type, value->children->next_child->next_child) == 0) {
      ERR(value->children->next_child->next_child->source_location,
          "Return type of left hand side expression of binary operator does not match declared left hand side return type");
    }

    // Get return type of RHS into `type`.
    if (!typecheck_expression(context, context_to_enter, expression->children->next_child, type)) return false;

    // Expected return type of RHS is fourth child of binary operator definition.
    if (type_compare_symbol(type, value->children->next_child->next_child->next_child) == 0)
      ERR(value->children->next_child->next_child->next_child->source_location,
          "Return type of right hand side expression of binary operator does not match declared right hand side return type");

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
    if (strcmp(value->value.symbol, "function") != 0 && strcmp(value->value.symbol, "external function") != 0)
      ERR(expression->source_location, "A called variable must have a function type!");

    // Only typecheck parameters when they exist.
    if (value->children->next_child) {
      iterator = expression->children->next_child->children;
      Node *expected_parameter = value->children->next_child;

      while (iterator && expected_parameter) {
        // Get return type of given parameter.
        if (!typecheck_expression(context, context_to_enter, iterator, type)) return false;
        if (type_compare_symbol(expected_parameter, type) == 0) {
          printf("Function:%s\n", expression->children->value.symbol);
          printf("Invalid argument:\n");
          print_node(iterator, 2);
          printf("Argument return type is `%s` but was expected to be `%s`\n",
                 type_node_text(type),
                 type_node_text(expected_parameter->children->next_child));
          ERR(expression->source_location, "Argument type does not match declared parameter type");
        }

        iterator = iterator->next_child;
        expected_parameter = expected_parameter->next_child;
      }

      if (expected_parameter != NULL) {
        printf("Function:%s\n", expression->children->value.symbol);
        printf("Expected argument(s):\n");
        while (expected_parameter) {
          print_node(expected_parameter, 2);
          expected_parameter = expected_parameter->next_child;
        }

        ERR(expression->source_location, "Not enough arguments passed to function!");
      }

      if (iterator != NULL)
        ERR(expression->source_location, "Too many arguments passed to function: %s", expression->children->value.symbol);
    }

    *result_type = *value->children;
    break;

  case NODE_TYPE_CAST: {
    Node *cast_type = expression->children;

    // Result of a cast expression will always be the casted-to type.
    *result_type = *cast_type;

    Node *expression_type = node_allocate();
    if (!typecheck_expression(context, context_to_enter, expression->children->next_child, expression_type)) return false;

    // We could/should remove this cast node if the cast type and
    // return type of expression are the same. Turns out removing nodes
    // is actually impossible without a parent node pointer member.
    // So, for now, this type of cast is just passed through.
    if (type_compare(cast_type, expression_type)) break;

    // TODO: We should probably have some way to assert the amount of
    // base types or something. Otherwise we'll have silent errors when
    // adding new base types.

    // Check for reinterpret kind of typecast (pointer to pointer)
    if (cast_type->pointer_indirection > 0) {
      if (cast_type->pointer_indirection != expression_type->pointer_indirection)
        ERR(expression->source_location,
            "Pointer to pointer cast must maintain pointer indirection level.");

      // Get size of cast_type and expression_type to determine kind of
      // typecast.
      size_t cast_type_size;
      size_t expression_type_size;
      Node *cast_type_info = node_allocate();
      Node *expression_type_info = node_allocate();

      // Use type copy without pointer indirection to get size of base
      // type!
      Node *cast_type_no_pointer = node_allocate();
      *cast_type_no_pointer = *cast_type;
      cast_type_no_pointer->pointer_indirection = 0;

      Node *expression_type_no_pointer = node_allocate();
      *expression_type_no_pointer = *expression_type;
      expression_type_no_pointer->pointer_indirection = 0;

      if (!parse_get_type(context, cast_type_no_pointer, cast_type_info, false)) return false;
      if (!parse_get_type(context, expression_type_no_pointer, expression_type_info, false)) return false;
      cast_type_size = (size_t) cast_type_info->children->value.integer;
      expression_type_size = (size_t) expression_type_info->children->value.integer;
      free(cast_type_info);
      free(expression_type_info);

      // a : integer = 69
      // ;; a is an 8 byte integer
      // b : byte = 42
      // ;; b is a one byte integer
      // ptr_a : @integer = &a
      // ptr_b : @byte    = &b
      // ;; The following should trigger a type error, as ptr_b does
      // ;; not contain enough memory to store an 8 byte integer.
      // ptr_a := [@integer]ptr_b
      if (cast_type_size > expression_type_size) {
        ERR(expression->source_location,
            "Invalid Reinterpret Typecast: Base type of cast type must be smaller or equal to expression_type");
      }
      break;
    } else if (expression_type->pointer_indirection > 0) {
      ERR(expression->source_location, "Invalid Typecast: Can not cast pointer to non-pointer type.");
    }

    // If it's not a pointer reinterpret, ensure both expression and
    // cast type are base types.
    Node int_type = (Node){
        .type = NODE_TYPE_SYMBOL,
        .value.symbol = "integer",
    };

    Node byte_type = (Node){
        .type = NODE_TYPE_SYMBOL,
        .value.symbol = "byte",
    };

    // TODO: Extract is_base_type() helper function.
    if (!(type_compare(cast_type, &int_type) || type_compare(cast_type, &byte_type)))
      ERR(expression->source_location, "Invalid typecast: Cast type must be a base type.");
    if (!(type_compare(expression_type, &int_type) || type_compare(expression_type, &byte_type)))
      ERR(expression->source_location, "Invalid typecast: Expression type must be a base type.");
  } break;
  }

  free(result);
  free(value);
  free(tmpnode);
  return true;
}

