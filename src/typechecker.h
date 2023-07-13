#ifndef TYPECHECKER_H
#define TYPECHECKER_H

#include <ast.h>
#include <error.h>
#include <parser.h>

/// Typecheck an expression.
///
/// This also resolves function references and determines
/// the type of the expression and stores it in the node.
///
/// \param ast The AST of the expression.
/// \param expr The expression to typecheck.
/// \return Whether the expression is well-formed.
NODISCARD bool typecheck_expression(Module *ast, Node *expr);

#endif /* TYPECHECKER_H */
