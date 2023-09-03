#ifndef LAYE_IRGEN_H
#define LAYE_IRGEN_H

#ifdef __cplusplus
extern "C" {
#endif

#include <lcc/lcc-c.h>

#include <layec/laye/ast.h>

LccModuleRef laye_generate_ir(LccContextRef context, layec_laye_module* laye_module);

#ifdef __cplusplus
}
#endif

#endif // LAYE_IRGEN_H
