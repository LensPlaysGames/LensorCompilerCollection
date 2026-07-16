#ifndef LCC_DEFAULTS_HH
#define LCC_DEFAULTS_HH

#include <lcc/format.hh>
#include <lcc/target.hh>

#include <lccbase/context.hh>

#include <hdronly/lcc/platform.hh>

namespace lcc {

/// Default target.
const Target* default_target =
#if defined(LCC_WINDOWS)
    lcc::Target::x86_64_windows;
#elif defined(LCC_APPLE) || defined(LCC_LINUX)
    lcc::Target::x86_64_linux;
#else
    lcc::Target::x86_64_linux;
#    warning "Unknown target, setting default target to x86_64_linux"
#endif

/// Default format
const lcc::Format* default_format
    = lcc::Format::gnu_as_att_assembly;

constexpr lcc::Context default_context() {
    return lcc::Context{
        lcc::default_target,
        lcc::default_format,
        lcc::Context::Options{
            lcc::Context::DoNotUseColour,
            lcc::Context::DoNotPrintStats,
            lcc::Context::DoNotDiagBacktrace,
            lcc::Context::DoNotPrintAST,
            lcc::Context::DoNotStopatLex,
            lcc::Context::DoNotStopatSyntax,
            lcc::Context::DoNotStopatSema,
            lcc::Context::DoNotPrintMachineIR,
            lcc::Context::DoNotStopatMIR
        }
    };
};

} // namespace lcc

#endif /* LCC_DEFAULTS_HH */
