#include <lcc/calling_conventions/ms_x64.hh>

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
        }

        param_desc.info.emplace_back(working_param);
    }
    return {};
}

// Given an LCC IR function, return a description of how the parameters
// would be passed in the SysV convention.
auto parameter_description(Function* function)
    -> ParameterDescription {
    return {};
}

} // namespace lcc::cconv::msx64
