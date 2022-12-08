#ifndef TYPECHECKER_H
#define TYPECHECKER_H

#include <error.h>
#include <parser.h>

void print_type_node(Node *type, size_t indent);

/// If types are equal, return true, otherwise false.
NODISCARD bool type_compare(Node *a, Node *b);
/// If type symbols are equivalent, return true, otherwise false.
NODISCARD bool type_compare_symbol(Node *a, Node *b);

NODISCARD bool expression_return_type(ParsingContext *context,
  ParsingContext **context_to_enter,
  Node *expression,
  Node *type);
NODISCARD bool typecheck_expression(ParsingContext *context,
  ParsingContext **context_to_enter,
  Node *expression,
  Node *type);
NODISCARD bool typecheck_program(ParsingContext *context, Node *program);

#endif /* TYPECHECKER_H */
