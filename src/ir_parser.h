#ifndef FUNCOMPILER_IR_PARSER_H
#define FUNCOMPILER_IR_PARSER_H

#include <codegen/codegen_forward.h>
#include <stddef.h>

/// Parse IR.
///
/// \param context The context into which to parse the IR.
/// \param filename The name of the file to parse. May be NULL.
/// \param ir The IR to parse.
/// \param sz The size of the IR to parse.
/// \return true on success, false on failure.
bool ir_parse(CodegenContext *context, const char *filename, const char* ir, size_t sz);

#endif // FUNCOMPILER_IR_PARSER_H
