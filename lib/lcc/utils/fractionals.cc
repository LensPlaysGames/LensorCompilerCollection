#include <lcc/utils/fractionals.hh>

namespace lcc {
u64 whole_to_fractional(DecimalFraction decimal_fraction) {
    u64 whole = decimal_fraction.whole;

    u64 fractional{};
    constexpr decltype(whole) one = 1;
    constexpr decltype(whole) bitwidth = sizeof(fractional) * CHAR_BIT;

    // Adjust whole to remove all trailing zeroes.
    while (whole and whole % 10 == 0)
        whole /= 10;

    // The decimal point in the input is placed just to the left of the
    // first *decimal* digit. To properly understand the scale of each digit,
    // we have to adjust it by the total amount of decimal digit places in the
    // input number (powers of 10).
    u64 divisor{10};
    {
        decltype(whole) whole_copy{whole / 10};
        while (whole_copy) {
            divisor *= 10;
            whole_copy /= 10;
        }
    }

    for (uint i = 0; i < decimal_fraction.leading_zeroes; ++i)
        divisor *= 10;

    // For every digit
    uint place{1};
    while (whole and place < bitwidth) {
        // If the input decimal number multiplied by 2 increases past the next
        // power of 10, then we know that we have *some value* over one half; this
        // means we are able to set the relevant binary bit (some negative power
        // of 2, AKA a half of a half of a half, etc).
        whole *= 2;

        if (whole >= divisor) {
            whole -= divisor;
            u64 adjusted_bit = one << (bitwidth - place);
            fractional |= adjusted_bit;
        }

        ++place;
    }

    // if (whole) {
    //     fmt::print("Inexact decimal to fractional conversion\n");
    // }

    return fractional;
}

DecimalFraction fractional_to_whole(u64 fractional) {
    u64 whole{};
    uint exponent{1};

    // TODO: Multiples of five may overflow quickly
    // 2^64 - 1 = 18446744073709551615
    // 5^27     =  7450580596923827000

    while (fractional) {
        // Get most significant bit of fractional
        constexpr u64 bitwidth = (sizeof(fractional) * CHAR_BIT);
        u64 top_bit = fractional >> (bitwidth - 1);
        // Adjust fractional
        // NOTE: It is *meant* to "overflow"; we are *trying* to discard the most
        // significant binary digit.
        fractional *= 2;

        // i := top_bit
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
            // FIXME: It may be smarter to use a LUT; an array of powers of five up to
            // exponent 27.
            for (
                u64 exponent_application = 0;
                exponent_application < exponent;
                ++exponent_application
            ) {
                // Prevent overflow of top_bit via multiplication.
                if (top_bit > std::numeric_limits<decltype(top_bit)>::max() / 5)
                    break;
                top_bit *= 5;
            }

            // Prevent overflow of whole via addition.
            if (whole > std::numeric_limits<decltype(top_bit)>::max() - top_bit)
                break;

            whole += top_bit;
        }

        // Stop if times ten would overflow (precision error)
        if (whole > std::numeric_limits<u64>::max() / 10)
            break;
        whole *= 10;

        // Perfect conversion. Break here to ensure exponent is correct.
        if (not fractional)
            break;

        ++exponent;
    }

    // Adjust whole to remove all trailing zeroes.
    while (whole and whole % 10 == 0)
        whole /= 10;

    // NOTE: We could also divide by 10 until we get zero and count how many
    // times we had to do that.
    uint scale{1};
    {
        decltype(whole) whole_copy{whole / 10};
        while (whole_copy) {
            ++scale;
            whole_copy /= 10;
        }
    }
    LCC_ASSERT(
        scale <= exponent,
        "Invalid scale (exponent: {}, scale: {})",
        exponent,
        scale
    );
    auto leading_zeroes = exponent - scale;
    return DecimalFraction{whole, leading_zeroes};
}

u32 fixed_to_binary32_float(const FixedPointNumber& f) {
    bool negative{}; // TODO
    // binary32 exponent bias is 127
    u8 exponent{127};
    auto mantissa{f.fractional};

    auto whole{f.whole};

    while (whole > 1) {
        bool misfit = whole % 2;
        whole /= 2;
        mantissa /= 2;
        if (misfit)
            mantissa |= ((u64) 1) << 63;
        ++exponent;
    }

    // If the mantissa is non-zero *and* the most significant bit isn't set,
    // we have to adjust it until the most significant bit *was* just set...
    if (not whole and mantissa) {
        // If the top bit is not set, adjust mantissa and whole such that t
        // Multiply until the top bit is set.
        while (not (mantissa & ((u64) 1) << 63)) {
            --exponent;
            mantissa *= 2;
        };
        // Multiply the top bit, which was set, out of the mantissa. This discards
        // the top "one" bit, as it is implicit in the binary32 format.
        --exponent;
        mantissa *= 2;
    }

    // Set exponent
    u32 binary32_value{exponent};
    binary32_value <<= 23;

    // Set the least significant 23 bits using the most significant 23 bits of
    // the mantissa.
    binary32_value |= (u32) (mantissa >> (64 - 23));
    // If the 24th most significant bit is set, round up...
    if (mantissa & (((u64) 1) << (64 - 24)))
        binary32_value |= 1;

    if (negative)
        binary32_value |= ((u32) 1) << 31;

    return binary32_value;
}

} // namespace lcc
