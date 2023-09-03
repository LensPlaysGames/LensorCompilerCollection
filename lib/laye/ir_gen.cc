#include <lcc/lcc-c.h>

#include <laye/ir_gen.h>

extern "C" {

LccModuleRef laye_generate_ir(LccContextRef context, layec_laye_module* laye_module)
{
    LccModuleRef lcc_module = lcc_module_create(context);

    return lcc_module;
}

}
