#ifndef TYPECHECKER_H
#define TYPECHECKER_H

#include <error.h>
#include <parser.h>

void print_type_node(Node *type, size_t indent);

/// If types are equal, return 1, otherwise 0.
int type_compare(Node *a, Node *b);
/// If type symbols are equivalent, return 1, otherwise 0.
char type_compare_symbol(Node *a, Node *b);

Error expression_return_type(ParsingContext *context,
                             ParsingContext **context_to_enter,
                             Node *expression,
                             Node *type);
Error typecheck_expression(ParsingContext *context,
                           ParsingContext **context_to_enter,
                           Node *expression,
                           Node *type);
Error typecheck_program(ParsingContext *context, Node *program);

#endif /* TYPECHECKER_H */
