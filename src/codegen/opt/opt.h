#ifndef FUNCOMPILER_OPT_H
#define FUNCOMPILER_OPT_H

#include <codegen/codegen_forward.h>

extern int optimise;

/// Currently, we don’t have optimisation levels, so this
/// will simply perform all available optimisations.
void codegen_optimise(CodegenContext *ctx);

/// This will reorder and optimise blocks but not change any instructions.
void codegen_optimise_blocks(CodegenContext *ctx);

/// Perform mandatory inlining.
/// \return False if there was an error.
bool codegen_process_inline_calls(CodegenContext *ctx);

#endif // FUNCOMPILER_OPT_H
