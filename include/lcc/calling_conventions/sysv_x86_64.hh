#ifndef LCC_CALLING_CONVENTION_SYSV_X86_64_HH
#define LCC_CALLING_CONVENTION_SYSV_X86_64_HH

#include <lcc/codegen/x86_64/x86_64.hh>

namespace lcc::cconv::sysv {

/// If you don't like the long signature, use like
///     `constexpr auto arg_regs = lcc::cconv::args_regs;`
constexpr const std::array<usz, 6> arg_regs = {
    +x86_64::RegisterId::RDI,
    +x86_64::RegisterId::RSI,
    +x86_64::RegisterId::RDX,
    +x86_64::RegisterId::RCX,
    +x86_64::RegisterId::R8,
    +x86_64::RegisterId::R9 //
};

} // namespace lcc::cconv::sysv

#endif /* LCC_CALLING_CONVENTION_SYSV_X86_64_HH */
