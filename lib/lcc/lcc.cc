#include <lcc/context.hh>
#include <lcc/ir/ir.hh>
#include <lcc/ir/module.hh>
#include <lcc/ir/printer.hh>
#include <lcc/ir/type.hh>
#include <lcc/lcc.h>

extern "C" {

LccModuleRef lcc_module_create(LccContextRef context)
{
    auto lcc_context = static_cast<lcc::Context*>(context);
    return static_cast<LccModuleRef>(new lcc::Module(lcc_context));
}

}
