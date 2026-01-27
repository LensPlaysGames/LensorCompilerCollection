#ifndef LCC_FRACTIONALS_HH
#define LCC_FRACTIONALS_HH

#include <lcc/utils.hh>

namespace lcc {

struct FixedPointNumber {
    u64 whole{};
    u64 fractional{};
};

// Given "5" return binary fractional for "0.5", 0b0.10
// Given "25" return binary fractional for "0.25", 0b0.01
//
// The output binary fractional number is in Q0.64 format.
u64 whole_to_fractional(u64 whole);

// Input fractional must be in Q0.64 fixed point format.
//
// Given "0b0.100..." return whole number 5
// Given "0b0.010..." return whole number 25
u64 fractional_to_whole(u64 fractional);

} // namespace lcc

#endif // LCC_FRACTIONALS_HH
