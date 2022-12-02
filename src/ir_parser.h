#ifndef FUNCOMPILER_IR_PARSER_H
#define FUNCOMPILER_IR_PARSER_H

#include <codegen/codegen_forward.h>
#include <stddef.h>

/// Parse IR.
/// Returns false on error.
bool ir_parse(CodegenContext *context, const char* ir, size_t sz);

#endif // FUNCOMPILER_IR_PARSER_H
