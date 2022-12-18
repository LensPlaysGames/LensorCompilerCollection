#ifndef COMPILER_PARSER_H
#define COMPILER_PARSER_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <error.h>
#include <codegen/codegen_forward.h>

/// Parse a program.
///
/// \param source The source code to parse.
/// \param filename The name of the file that we’re parsing.
/// \return An AST representing the source code on success, NULL on failure.
NODISCARD AST *parse(span source, const char* filename);

#endif /* COMPILER_PARSER_H */
