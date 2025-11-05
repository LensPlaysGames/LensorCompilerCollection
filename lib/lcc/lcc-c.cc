#include <lcc/context.hh>
#include <lcc/format.hh>
#include <lcc/ir/ir.hh>
#include <lcc/ir/module.hh>
#include <lcc/ir/type.hh>
#include <lcc/lcc-c.h>
#include <lcc/target.hh>

extern "C" {

/// Gets the target info for 64 bit Linux.
LccTargetRef lcc_target_x86_64_linux() {
    return reinterpret_cast<LccTargetRef>(lcc::Target::x86_64_linux);
}

/// Gets the target info for 64 bit Windows.
LccTargetRef lcc_target_x86_64_windows() {
    return reinterpret_cast<LccTargetRef>(lcc::Target::x86_64_windows);
}

/// Gets the target info for 64 bit Linux.
LccFormatRef lcc_format_llvm_textual_ir() {
    return reinterpret_cast<LccFormatRef>(lcc::Format::llvm_textual_ir);
}

/// Gets the target info for 64 bit Windows.
LccFormatRef lcc_format_gnu_as_att_assembly() {
    return reinterpret_cast<LccFormatRef>(lcc::Format::gnu_as_att_assembly);
}

/// Create an LCC context.
LccContextRef lcc_context_create(LccTargetRef target, LccFormatRef format) {
    return reinterpret_cast<LccContextRef>(new lcc::Context{
        reinterpret_cast<const lcc::Target*>(target),
        reinterpret_cast<const lcc::Format*>(format),
        lcc::Context::Options{
            lcc::Context::DoNotUseColour,
            lcc::Context::DoNotPrintStats,
            lcc::Context::DoNotPrintAST,
            lcc::Context::DoNotStopatLex,
            lcc::Context::DoNotStopatSyntax,
            lcc::Context::DoNotStopatSema,
            lcc::Context::DoNotPrintMIR,
            lcc::Context::DoNotStopatMIR //
        }
    });
}

LccModuleRef lcc_module_create(LccContextRef context) {
    auto* lcc_context = reinterpret_cast<lcc::Context*>(context);
    return reinterpret_cast<LccModuleRef>(new lcc::Module(lcc_context));
}
}
