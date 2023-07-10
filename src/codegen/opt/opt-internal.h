#ifndef INTERCEPT_CODEGEN_OPT_OPT_INTERNAL_H
#define INTERCEPT_CODEGEN_OPT_OPT_INTERNAL_H

/// This header is only supposed to be included by .c files that deal
/// with optimisation.

#include <codegen.h>
#include <codegen/dom.h>
#include <codegen/intermediate_representation.h>
#include <codegen/opt/opt.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <vector.h>

/// Inlining pass during optimisation.
bool opt_inline(CodegenContext *ctx, isz threshold);

#endif // INTERCEPT_CODEGEN_OPT_OPT_INTERNAL_H
