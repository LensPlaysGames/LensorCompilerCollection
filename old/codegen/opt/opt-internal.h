#ifndef INTERCEPT_CODEGEN_OPT_OPT_INTERNAL_H
#define INTERCEPT_CODEGEN_OPT_OPT_INTERNAL_H

/// This header is only supposed to be included by .c files that deal
/// with optimisation.

#include <codegen.h>
#include <codegen/opt/opt.h>
#include <ir/dom.h>
#include <ir/ir.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <vector.h>

/// Inlining pass during optimisation.
bool opt_inline(CodegenContext *ctx, isz threshold);

/// Convert a call to a tail call if possible.
///
/// \param i The call to convert.
/// \return Whether this was successful.
bool opt_try_convert_to_tail_call(IRInstruction *i);

#endif // INTERCEPT_CODEGEN_OPT_OPT_INTERNAL_H
