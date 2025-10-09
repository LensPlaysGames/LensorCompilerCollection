#ifndef LCC_CALLING_CONVENTION_MS_X64_HH
#define LCC_CALLING_CONVENTION_MS_X64_HH

#include <lcc/codegen/x86_64/x86_64.hh>

#include <array>

namespace lcc::cconv::msx64 {

// Known Examples
//
//   func1(int a, int b, int c, int d, int e, int f);
//   // a in RCX, b in RDX, c in R8, d in R9, f then e pushed on stack
//
//   func2(float a, double b, float c, double d, float e, float f);
//   // a in XMM0, b in XMM1, c in XMM2, d in XMM3, f then e pushed on stack
//
//   func3(int a, double b, int c, float d, int e, float f);
//   // a in RCX, b in XMM1, c in R8, d in XMM3, f then e pushed on stack
//
//   func4(__m64 a, __m128 b, struct c, float d, __m128 e, __m128 f);
//   // a in RCX, ptr to b in RDX, ptr to c in R8, d in XMM3,
//   // ptr to f pushed on stack, then ptr to e pushed on stack
//
//   __int64 func1(int a, float b, int c, int d, int e);
//   // Caller passes a in RCX, b in XMM1, c in R8, d in R9, e pushed on stack,
//   // callee returns __int64 result in RAX.
//
//   __m128 func2(float a, double b, int c, __m64 d);
//   // Caller passes a in XMM0, b in XMM1, c in R8, d in R9,
//   // callee returns __m128 result in XMM0.
//
//   struct Struct1 {
//      int j, k, l;    // Struct1 exceeds 64 bits.
//   };
//   Struct1 func3(int a, double b, int c, float d);
//   // Caller allocates memory for Struct1 returned and passes pointer in RCX,
//   // a in RDX, b in XMM2, c in R9, d pushed on the stack;
//   // callee returns pointer to Struct1 result in RAX.
//
//   struct Struct2 {
//      int j, k;    // Struct2 fits in 64 bits, and meets requirements for return by value.
//   };
//   Struct2 func4(int a, double b, int c, float d);
//   // Caller passes a in RCX, b in XMM1, c in R8, and d in XMM3;
//   // callee returns Struct2 result by value in RAX.

//
// A scalar return value that can fit into 64 bits, including the __m64
// type, is returned through RAX. Nonscalar types including floats,
// doubles, and vector types such as __m128, __m128i, __m128d are returned
// in XMM0. The state of unused bits in the value returned in RAX or XMM0
// is undefined.
// Structs and unions of size 8, 16, 32, or 64 bits, and __m64 types, are
// passed as if they were integers of the same size (in RAX).
constexpr x86_64::RegisterId return_register = x86_64::RegisterId::RAX;

constexpr const std::array<x86_64::RegisterId, 4> arg_regs{
    x86_64::RegisterId::RCX,
    x86_64::RegisterId::RDX,
    x86_64::RegisterId::R8,
    x86_64::RegisterId::R9 //
};

// The x64 ABI considers the registers RAX, RCX, RDX, R8, R9, R10, R11,
// and XMM0-XMM5 volatile. When present, the upper portions of YMM0-YMM15
// and ZMM0-ZMM15 are also volatile. On AVX512VL, the ZMM, YMM, and XMM
// registers 16-31 are also volatile. When AMX support is present, the TMM
// tile registers are volatile. Consider volatile registers destroyed on
// function calls unless otherwise safety-provable by analysis such as
// whole program optimization.
// The x64 ABI considers registers RBX, RBP, RDI, RSI, RSP, R12, R13, R14,
// R15, and XMM6-XMM15 nonvolatile. They must be saved and restored by a
// function that uses them.
constexpr const std::array<x86_64::RegisterId, 7> volatile_regs{
    x86_64::RegisterId::RAX,
    x86_64::RegisterId::RCX,
    x86_64::RegisterId::RDX,
    x86_64::RegisterId::R8,
    x86_64::RegisterId::R9,
    x86_64::RegisterId::R10,
    x86_64::RegisterId::R11,
};

enum class ParameterClass {
    INVALID,

    REGISTER,
    MEMORY,

    COUNT
};

struct ParameterDescription {
    struct Parameter {
        ParameterClass location{};
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

        bool is_overlarge{false};

        enum class Kinds {
            SingleRegister,
            PointerInRegister,
            Stack,
        };

        [[nodiscard("Return value is meant to be used in a switch statement...")]]
        Kinds kind() {
            if (arg_regs == 1) {
                if (is_overlarge)
                    return Kinds::PointerInRegister;
                return Kinds::SingleRegister;
            }
            return Kinds::Stack;
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

} // namespace lcc::cconv::msx64

#endif /* LCC_CALLING_CONVENTION_MS_X64_HH */
