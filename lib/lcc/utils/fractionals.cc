#include <lcc/utils/fractionals.hh>

namespace lcc {
u64 whole_to_fractional(u64 whole) {
    u64 fractional{};
    constexpr u64 one = 1;
    constexpr u64 bitwidth = sizeof(fractional) * CHAR_BIT;

    // The decimal point in the input is placed just to the left of the
    // first *decimal* digit. To properly understand the scale of each digit,
    // we have to adjust it by the total amount of decimal digit places in the
    // input number (powers of 10).
    u64 divisor{10};
    {
        u64 whole_copy{whole / 10};
        while (whole_copy) {
            divisor *= 10;
            whole_copy /= 10;
        }
    }

    // For every digit
    u64 place{1};
    while (whole and place < bitwidth) {
        u64 fractional_for_digit{};

        // If the input decimal number multiplied by 2 increases past the next
        // power of 10, then we know that we have *some value* over one half; this
        // means we are able to set the relevant binary bit (some negative power
        // of 2, AKA a half of a half of a half, etc).
        whole *= 2;

        if (whole >= divisor) {
            whole -= divisor;
            u64 adjusted_bit = one << (bitwidth - place);
            fractional_for_digit |= adjusted_bit;
        }

        // Accumulate
        fractional += fractional_for_digit;

        ++place;
    }

    // if (whole) {
    //     fmt::print("Inexact decimal to fractional conversion\n");
    // }

    return fractional;
}

u64 fractional_to_whole(u64 fractional) {
    u64 whole{};
    u64 exponent{1};

    while (fractional) {
        // Stop if times ten would overflow (precision error)
        // We try not to do the divide unless we absolutely have to.
        // A 64 bit number can represent at least 10^19, so if it's less than that
        // it can't overflow when multiplying by 10...
        if (
            whole
            and exponent > 18
            and (whole > std::numeric_limits<u64>::max() / 10)
        ) break;

        whole *= 10;

        // Get top bit of fractional
        u64 top_bit = fractional >> ((sizeof(fractional) * CHAR_BIT) - 1);
        // Adjust fractional
        fractional <<= 1;

        // i := relevant_digit
        // n := exponent
        // i * 2^-n = x * 10^-n
        //     multiply by 10
        //     until (n + 1) == 0 for (10^0 = 1)
        // 10i * 2^-n = x * 10^-(n + 1)
        // 10i * 2^-1 = x * 1
        //     rearrange
        // x = 2^-1 * 10 * i
        //     simplify
        // x = 5i
        if (top_bit) {
            for (
                u64 exponent_application = 0;
                exponent_application < exponent;
                ++exponent_application
            ) top_bit *= 5;

            whole += top_bit;
        }

        ++exponent;
    }

    return whole;
}

} // namespace lcc
