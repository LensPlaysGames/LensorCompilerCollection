#ifndef FUNCOMPILER_IR_PARSER_H
#define FUNCOMPILER_IR_PARSER_H

#include <codegen/codegen_forward.h>
#include <stddef.h>

/// Parse IR.
///
/// \param context The context into which to parse the IR.
/// \param filename The name of the file to parse. May be NULL.
/// \param ir The IR to parse.
/// \return true on success, false on failure.
bool ir_parse(CodegenContext *context, const char *filename, string ir);

#endif // FUNCOMPILER_IR_PARSER_H
