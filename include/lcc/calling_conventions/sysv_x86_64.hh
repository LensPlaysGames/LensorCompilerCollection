#ifndef LCC_CALLING_CONVENTION_SYSV_X86_64_HH
#define LCC_CALLING_CONVENTION_SYSV_X86_64_HH

#include <lcc/codegen/x86_64/x86_64.hh>
#include <lcc/ir/ir.hh>

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

enum class ParameterClass {
    INVALID,

    REGISTER,
    MEMORY,

    COUNT
};

struct ParameterDescription {
    struct Parameter {
        ParameterClass kind{ParameterClass::INVALID};
        /// The amount of argument registers, total, taken up by parameters BEFORE
        /// (and NOT by) this parameter.
        usz arg_regs_used{};
        /// The amount of argument registers taken up by this parameter.
        usz arg_regs{};
        usz stack_slot_index{};
        usz stack_byte_offset{};
    };
    std::vector<Parameter> info;
};

auto parameter_description(Function* function) -> ParameterDescription {
    // If we super-cared or measured this function as being really slow or
    // called over and over (which won't happen), we could implement a cache
    // on Function* here, or a name-based one.

    ParameterDescription out{};
    ParameterDescription::Parameter working_param{};

    out.info.reserve(function->params().size());

    usz next_stack_slot_index{};
    for (const auto* param : function->params()) {
        working_param.arg_regs_used += working_param.arg_regs;
        working_param.stack_slot_index = next_stack_slot_index;

        working_param.kind = ParameterClass::REGISTER;
        working_param.arg_regs = 0;

        if (
            param->type()->bytes() <= x86_64::GeneralPurposeBytewidth
            and working_param.arg_regs_used < arg_regs.size()
        ) ++working_param.arg_regs;

        else if (
            param->type()->bytes() <= 2 * x86_64::GeneralPurposeBytewidth
            and working_param.arg_regs_used < arg_regs.size() - 1
        ) working_param.arg_regs += 2;

        else {
            working_param.kind = ParameterClass::MEMORY;
            ++next_stack_slot_index;
            working_param.stack_byte_offset += param->type()->bytes();
        }

        out.info.emplace_back(working_param);
    }

    return out;
};

} // namespace lcc::cconv::sysv

#endif /* LCC_CALLING_CONVENTION_SYSV_X86_64_HH */
