#include <lcc/calling_conventions/sysv_x86_64.hh>
#include <lcc/ir/ir.hh>

#include <algorithm>
#include <ranges>

namespace lcc {

auto cconv::sysv::parameter_description(std::vector<Type*> parameter_types) -> ParameterDescription {
    // If we super-cared or measured this function as being really slow or
    // called over and over (which won't happen), we could implement a cache
    // on Function* here, or a name-based one.

    ParameterDescription out{};
    ParameterDescription::Parameter working_param{};

    out.info.reserve(parameter_types.size());

    usz next_stack_slot_index{};
    for (const auto* t : parameter_types) {
        working_param.arg_regs_used += working_param.arg_regs;
        working_param.stack_slot_index = next_stack_slot_index;

        working_param.location = ParameterClass::REGISTER;
        working_param.arg_regs = 0;

        if (
            t->bytes() <= x86_64::GeneralPurposeBytewidth
            and working_param.arg_regs_used < arg_regs.size()
        ) ++working_param.arg_regs;

        else if (
            t->bytes() <= 2 * x86_64::GeneralPurposeBytewidth
            and working_param.arg_regs_used < arg_regs.size() - 1
        ) working_param.arg_regs += 2;

        else {
            working_param.location = ParameterClass::MEMORY;
            ++next_stack_slot_index;
            // TODO: Take alignment into consideration, I'd think.
            working_param.stack_byte_offset_used = working_param.stack_byte_offset;
            working_param.stack_byte_offset += t->bytes();
        }

        out.info.emplace_back(working_param);
    }

    return out;
};

auto cconv::sysv::parameter_description(Function* function) -> ParameterDescription {
    std::vector<Type*> parameter_types{};
    rgs::transform(
        function->params(),
        std::back_inserter(parameter_types),
        [](auto p) { return p->type(); }
    );
    return parameter_description(parameter_types);
};

} // namespace lcc
