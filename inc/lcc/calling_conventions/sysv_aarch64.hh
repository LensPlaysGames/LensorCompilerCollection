#ifndef LCC_CALLING_CONVENTION_SYSV_AARCH64_HH
#define LCC_CALLING_CONVENTION_SYSV_AARCH64_HH

#include <lcc/codegen/aarch64/aarch64.hh>

#include <array>
#include <vector>

namespace lcc::cconv::sysv_aarch64 {

constexpr aarch64::RegisterId return_register = aarch64::RegisterId::R0;

/// If you don't like the long signature, use like
///     `constexpr auto arg_regs = lcc::cconv::args_regs;`
constexpr const std::array<aarch64::RegisterId, 8> arg_regs = {
    aarch64::RegisterId::R0,
    aarch64::RegisterId::R1,
    aarch64::RegisterId::R2,
    aarch64::RegisterId::R3,
    aarch64::RegisterId::R4,
    aarch64::RegisterId::R5,
    aarch64::RegisterId::R6,
    aarch64::RegisterId::R7
};

constexpr const std::array<aarch64::RegisterId, 16> volatile_regs = {
    aarch64::RegisterId::R0,
    aarch64::RegisterId::R1,
    aarch64::RegisterId::R2,
    aarch64::RegisterId::R3,
    aarch64::RegisterId::R4,
    aarch64::RegisterId::R5,
    aarch64::RegisterId::R6,
    aarch64::RegisterId::R7,
    aarch64::RegisterId::R8,
    aarch64::RegisterId::R9,
    aarch64::RegisterId::R10,
    aarch64::RegisterId::R11,
    aarch64::RegisterId::R12,
    aarch64::RegisterId::R13,
    aarch64::RegisterId::R14,
    aarch64::RegisterId::R15
};

// All scalar regs are volatile
constexpr const std::array<aarch64::RegisterId, 32> scalar_regs = {
    aarch64::RegisterId::V0,
    aarch64::RegisterId::V1,
    aarch64::RegisterId::V2,
    aarch64::RegisterId::V3,
    aarch64::RegisterId::V4,
    aarch64::RegisterId::V5,
    aarch64::RegisterId::V6,
    aarch64::RegisterId::V7,
    aarch64::RegisterId::V8,
    aarch64::RegisterId::V9,
    aarch64::RegisterId::V10,
    aarch64::RegisterId::V11,
    aarch64::RegisterId::V12,
    aarch64::RegisterId::V13,
    aarch64::RegisterId::V14,
    aarch64::RegisterId::V15,
    aarch64::RegisterId::V16,
    aarch64::RegisterId::V17,
    aarch64::RegisterId::V18,
    aarch64::RegisterId::V19,
    aarch64::RegisterId::V20,
    aarch64::RegisterId::V21,
    aarch64::RegisterId::V22,
    aarch64::RegisterId::V23,
    aarch64::RegisterId::V24,
    aarch64::RegisterId::V25,
    aarch64::RegisterId::V26,
    aarch64::RegisterId::V27,
    aarch64::RegisterId::V28,
    aarch64::RegisterId::V29,
    aarch64::RegisterId::V30,
    aarch64::RegisterId::V31
};

constexpr const std::array<aarch64::RegisterId, 8> scalar_arg_regs = {
    aarch64::RegisterId::V0,
    aarch64::RegisterId::V1,
    aarch64::RegisterId::V2,
    aarch64::RegisterId::V3,
    aarch64::RegisterId::V4,
    aarch64::RegisterId::V5,
    aarch64::RegisterId::V6,
    aarch64::RegisterId::V7
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
auto parameter_description(std::vector<Type*>& parameter_types)
    -> ParameterDescription;

// Given an LCC IR function, return a description of how the parameters
// would be passed in the SysV convention.
auto parameter_description(Function* function)
    -> ParameterDescription;

} // namespace lcc::cconv::sysv_aarch64

#endif /* LCC_CALLING_CONVENTION_SYSV_AARCH64_HH */
