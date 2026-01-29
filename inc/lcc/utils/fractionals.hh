#ifndef LCC_FRACTIONALS_HH
#define LCC_FRACTIONALS_HH

#include <lcc/utils.hh>

#include <fmt/format.h>

namespace lcc {

struct FixedPointNumber {
    u64 whole{};
    u64 fractional{};
};

struct DecimalFraction {
    u64 whole{};
    uint leading_zeroes{};

    explicit constexpr DecimalFraction(
        u64 whole_,
        uint leading_zeroes_
    )
        : whole(whole_),
          leading_zeroes(leading_zeroes_) {
        LCC_ASSERT(
            leading_zeroes < 64,
            "Obscene amount of leading zeroes..."
        );
    }
};

// @brief Convert a decimal representation of a fractional part of a
// number into the binary fractional representation of that fractional
// number.
//
// Basically, the input number gets treated as a whole representation of
// the numbers following the decimal point, rather than the numbers preceding.
//
// Given "5" return binary fractional for "0.5", 0b0.10
// Given "25" return binary fractional for "0.25", 0b0.01
// Given "250" return binary fractional for "0.25", 0b0.01
//
// The output binary fractional number is in Q0.64 format.
u64 whole_to_fractional(DecimalFraction whole);

// Input fractional must be in Q0.64 fixed point format.
//
// Given "0b0.100..." return whole number 5  (i.e. "0.5")
// Given "0b0.010..." return whole number 25 (i.e. "0.25")
DecimalFraction fractional_to_whole(u64 fractional);

// Convert a fixed point fractional number into a binary32 float value (1
// sign bit, 8 bit exponent, 23 bit mantissa/significand).
u32 fixed_to_binary32_float(const FixedPointNumber&);

} // namespace lcc

template <>
struct fmt::formatter<lcc::DecimalFraction> : formatter<string_view> {
    template <typename FormatContext>
    auto format(const lcc::DecimalFraction& f, FormatContext& ctx) const {
        return fmt::format_to(
            ctx.out(),
            "{}{}",
            std::string(f.leading_zeroes, '0'),
            f.whole
        );
    }
};

template <>
struct fmt::formatter<lcc::FixedPointNumber> : formatter<string_view> {
    template <typename FormatContext>
    auto format(const lcc::FixedPointNumber& f, FormatContext& ctx) const {
        return fmt::format_to(
            ctx.out(),
            "{}.{}",
            f.whole,
            lcc::fractional_to_whole(f.fractional)
        );
    }
};

#endif // LCC_FRACTIONALS_HH
