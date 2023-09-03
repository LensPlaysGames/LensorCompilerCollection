#include <lcc/context.hh>
#include <lcc/ir/ir.hh>
#include <lcc/ir/module.hh>
#include <lcc/ir/printer.hh>
#include <lcc/ir/type.hh>
#include <lcc/lcc-c.h>

extern "C" {

/// Gets the target info for 64 bit Linux.
LccTargetRef lcc_target_x86_64_linux()
{
    return reinterpret_cast<LccTargetRef>(Target::x86_64_linux);
}

/// Gets the target info for 64 bit Windows.
LccTargetRef lcc_target_x86_64_windows()
{
    return reinterpret_cast<LccTargetRef>(Target::x86_64_windows);
}

/// Create an LCC context.
LccContextRef lcc_context_create(LccTargetRef target)
{
    return new lcc::Context{reinterpret_cast<const lcc::Target*>(target)}
}

LccModuleRef lcc_module_create(LccContextRef context)
{
    auto lcc_context = reinterpret_cast<lcc::Context*>(context);
    return reinterpret_cast<LccModuleRef>(new lcc::Module(lcc_context));
}

}
