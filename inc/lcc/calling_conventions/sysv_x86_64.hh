#ifndef LCC_CALLING_CONVENTION_SYSV_X86_64_HH
#define LCC_CALLING_CONVENTION_SYSV_X86_64_HH

#include <lcc/codegen/x86_64/x86_64.hh>

#include <array>
#include <vector>

namespace lcc::cconv::sysv {

constexpr x86_64::RegisterId return_register = x86_64::RegisterId::RAX;

/// If you don't like the long signature, use like
///     `constexpr auto arg_regs = lcc::cconv::args_regs;`
constexpr const std::array<x86_64::RegisterId, 6> arg_regs = {
    x86_64::RegisterId::RDI,
    x86_64::RegisterId::RSI,
    x86_64::RegisterId::RDX,
    x86_64::RegisterId::RCX,
    x86_64::RegisterId::R8,
    x86_64::RegisterId::R9
};

constexpr const std::array<x86_64::RegisterId, 9> volatile_regs = {
    x86_64::RegisterId::RAX,
    x86_64::RegisterId::RCX,
    x86_64::RegisterId::RDX,
    x86_64::RegisterId::RSI,
    x86_64::RegisterId::RDI,
    x86_64::RegisterId::R8,
    x86_64::RegisterId::R9,
    x86_64::RegisterId::R10,
    x86_64::RegisterId::R11
};

// Both the argument and volatile scalar registers
constexpr const std::array<x86_64::RegisterId, 8> scalar_regs = {
    x86_64::RegisterId::XMM0,
    x86_64::RegisterId::XMM1,
    x86_64::RegisterId::XMM2,
    x86_64::RegisterId::XMM3,
    x86_64::RegisterId::XMM4,
    x86_64::RegisterId::XMM5,
    x86_64::RegisterId::XMM6,
    x86_64::RegisterId::XMM7
};

enum class ParameterClass {
    INVALID,

    REGISTER,
    MEMORY,

    COUNT
};

struct ParameterDescription {
    struct Parameter {
        ParameterClass location{ParameterClass::INVALID};
        /// The amount of argument registers, total, taken up by parameters BEFORE
        /// (and NOT by) this parameter. This is the first index into the argument
        /// registers that is valid. The first index into the argument registers
        /// that is /invalid/ is `arg_regs_used + arg_regs`
        usz arg_regs_used{};
        /// The amount of argument registers taken up by this parameter.
        usz arg_regs{};

        /// The index of the "stack slot" this parameter is stored within.
        /// Only valid for memory parameters.
        usz stack_slot_index{};
        /// The offset, in bytes, of this parameter from the base of the stack.
        /// That is, the first memory parameter will have it's own size in bytes as
        /// it's offset.
        /// Only valid for memory parameters.
        usz stack_byte_offset{};
        /// The offset, in bytes, that the stack was already at due to previous
        /// parameters.
        usz stack_byte_offset_used{};

        /// The amount of scalar argument registers, total, taken up by parameters
        /// BEFORE (and NOT by) this parameter. This is the first index into the
        /// scalar argument registers that is valid. The first index into the scalar
        /// argument registers that is /invalid/ is `arg_scalars_used + arg_scalars`
        usz arg_scalars_used{};
        /// The amount of scalar argument registers taken up by this parameter.
        usz arg_scalars{};

        bool is_memory() { return location == ParameterClass::MEMORY; }
        bool is_register() { return location == ParameterClass::REGISTER; }

        bool is_single_register() { return is_register() and arg_regs == 1; }
        bool is_double_register() { return is_register() and arg_regs == 2; }

        bool is_scalar() { return is_register() and arg_scalars == 1; }

        enum class Kinds {
            SingleRegister,
            DoubleRegister,
            Memory,
            Scalar,
        };

        [[nodiscard]]
        Kinds kind() {
            if (arg_regs == 1) return Kinds::SingleRegister;
            if (arg_regs == 2) return Kinds::DoubleRegister;
            LCC_ASSERT(
                arg_regs == 0,
                "Invalid number of argument registers used by single parameter"
            );

            if (arg_scalars == 1) return Kinds::Scalar;
            LCC_ASSERT(
                arg_scalars == 0,
                "Invalid number of scalar registers used by parameter"
            );

            return Kinds::Memory;
        }
    };
    std::vector<Parameter> info{};
};

// Return a description as if the given list of types were the types of
// the parameters of a function.
auto parameter_description(std::vector<Type*> parameter_types)
    -> ParameterDescription;

// Given an LCC IR function, return a description of how the parameters
// would be passed in the SysV convention.
auto parameter_description(Function* function)
    -> ParameterDescription;

} // namespace lcc::cconv::sysv

#endif /* LCC_CALLING_CONVENTION_SYSV_X86_64_HH */
