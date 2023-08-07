#ifndef FUNCOMPILER_IR_PARSER_H
#define FUNCOMPILER_IR_PARSER_H

#include <codegen/codegen_forward.h>
#include <stddef.h>
#include <error.h>

/// Parse IR.
///
/// \param context The context into which to parse the IR.
/// \param filename The name of the file to parse. May be NULL.
/// \param ir The IR to parse.
/// \return true on success, false on failure.
static inline bool ir_parse(CodegenContext *context, const char *filename, string ir) {
    /// TODO: remove this body once this is working again.
    (void) context;
    (void) filename;
    (void) ir;
    ASSERT(false, "Sorry, IR parser is currently not supported");
}

#endif // FUNCOMPILER_IR_PARSER_H
