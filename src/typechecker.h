#ifndef TYPECHECKER_H
#define TYPECHECKER_H

#include <error.h>
#include <parser.h>

Error typecheck_program(ParsingContext *context, Node *program);

#endif /* TYPECHECKER_H */
