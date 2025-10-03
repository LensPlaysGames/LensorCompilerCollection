#include <lcc/calling_conventions/ms_x64.hh>
#include <lcc/codegen/x86_64/x86_64.hh>

namespace lcc::cconv::msx64 {

// Return a description as if the given list of types were the types of
// the parameters of a function.
auto parameter_description(std::vector<Type*> parameter_types)
    -> ParameterDescription {
    ParameterDescription param_desc{};
    param_desc.info.reserve(parameter_types.size());

    ParameterDescription::Parameter working_param{};
    usz next_stack_slot_index{};
    for (auto [param_i, arg] : vws::enumerate(parameter_types)) {
        working_param.arg_regs_used += working_param.arg_regs;
        working_param.arg_regs = 0;
        working_param.is_overlarge = arg->bits() > x86_64::GeneralPurposeBitwidth;

        // TODO: First four registers are passed in arguments, but, for types
        // larger than the register size, a pointer is passed in that register.
        // Basically, if an argument is over-large, we allocate a copy on the
        // stack (that way the caller can modify without doing bad bad), and then
        // pass the pointer to the stack address in the regular place the argument
        // would have gone (either the register the argument would have fit in, or
        // on the stack).
        // "Structs or unions (larger than 64 bits) are passed as a pointer to
        // memory allocated by the caller. For these aggregate types passed as a
        // pointer, including __m128, the caller-allocated temporary memory must
        // be 16-byte aligned."
        if (usz(param_i) < arg_regs.size()) {
            working_param.location = ParameterClass::REGISTER;
            working_param.arg_regs = 1;
        } else {
            working_param.location = ParameterClass::MEMORY;
            working_param.stack_slot_index = next_stack_slot_index;
            working_param.stack_byte_offset_used = working_param.stack_byte_offset;
            working_param.stack_byte_offset += arg->bytes();
        }

        param_desc.info.emplace_back(working_param);
    }
    return param_desc;
}

// Given an LCC IR function, return a description of how the parameters
// would be passed in the SysV convention.
auto parameter_description(Function* function)
    -> ParameterDescription {
    std::vector<Type*> parameter_types{};
    rgs::transform(
        function->params(),
        std::back_inserter(parameter_types),
        [](auto p) { return p->type(); }
    );
    return parameter_description(parameter_types);
}

} // namespace lcc::cconv::msx64
