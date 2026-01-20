#ifndef LCC_AINT_HH
#define LCC_AINT_HH

#include <lcc/utils.hh>

namespace lcc {
/// \brief Arbitrary-precision integer type.
///
/// This currently supports computations on i1-i64. Arithmetic
/// operations can only be performed on two `aint`s of the same
/// bit width.
///
/// Because this type is signless, there are no operator overloads
/// for operators that care about the sign bit; e.g. for dividing
/// two `aint`s, use the `.sdiv()` or `.udiv()` member functions.
///
/// However, operator overloads are provided for use with `u64`
/// and `i64`.
class aint {
public:
    using Word = u64;
    using SWord = i64;

private:
    Word w{};
    u8 bit_width{64};

    static constexpr Word Bits = sizeof(Word) * CHAR_BIT;

    constexpr auto SExt() const -> SWord { return SWord(sign_bit() ? ~Word(0) & w : w); }

public:
    constexpr aint() = default;
    constexpr aint(bool b) : aint(1, Word(b)) {}
    constexpr aint(std::integral auto value) : aint(sizeof value * CHAR_BIT, Word(value)) {}
    constexpr aint(u64 width, SWord value) : aint(width, aint(width, Word(value)).sext(width).value()) {}
    constexpr aint(u64 width, Word value) : w(Word(value)), bit_width(u8(width)) {
        LCC_ASSERT(width and width <= Bits, "Bit width must be between 1 and 64");

        /// Truncate the value to the given bit width.
        w &= sign_bit() | (sign_bit() - 1);
    }

    [[nodiscard]] constexpr aint operator-() const { return {bit_width, -w}; }
    [[nodiscard]] constexpr aint operator~() const { return {bit_width, ~w}; }
    [[nodiscard]] constexpr aint operator!() const { return {not w}; }

    [[nodiscard]] constexpr aint operator+(aint rhs) const { return {bit_width, w + rhs.w}; }
    [[nodiscard]] constexpr aint operator-(aint rhs) const { return {bit_width, w - rhs.w}; }
    [[nodiscard]] constexpr aint operator*(aint rhs) const { return {bit_width, w * rhs.w}; }
    [[nodiscard]] constexpr aint operator|(aint rhs) const { return {bit_width, w | rhs.w}; }
    [[nodiscard]] constexpr aint operator&(aint rhs) const { return {bit_width, w & rhs.w}; }
    [[nodiscard]] constexpr aint operator^(aint rhs) const { return {bit_width, w ^ rhs.w}; }
    [[nodiscard]] constexpr aint operator<<(aint rhs) const { return {bit_width, w << rhs.w}; }
    [[nodiscard]] constexpr bool operator==(aint rhs) const { return w == rhs.w; }
    [[nodiscard]] constexpr bool operator!=(aint rhs) const { return w != rhs.w; }

    [[nodiscard]] constexpr aint operator+(std::integral auto rhs) const { return {Bits, w + Word(rhs)}; }
    [[nodiscard]] constexpr aint operator-(std::integral auto rhs) const { return {Bits, w - Word(rhs)}; }
    [[nodiscard]] constexpr aint operator*(std::integral auto rhs) const { return {Bits, w * Word(rhs)}; }
    [[nodiscard]] constexpr aint operator|(std::integral auto rhs) const { return {Bits, w | Word(rhs)}; }
    [[nodiscard]] constexpr aint operator&(std::integral auto rhs) const { return {Bits, w & Word(rhs)}; }
    [[nodiscard]] constexpr aint operator^(std::integral auto rhs) const { return {Bits, w ^ Word(rhs)}; }
    [[nodiscard]] constexpr aint operator<<(std::integral auto rhs) const { return {Bits, w << Word(rhs)}; }
    [[nodiscard]] constexpr bool operator==(std::integral auto rhs) const { return w == Word(rhs); }
    [[nodiscard]] constexpr bool operator!=(std::integral auto rhs) const { return w != Word(rhs); }

    [[nodiscard]] constexpr aint operator/(u64 rhs) const { return {Bits, w / rhs}; }
    [[nodiscard]] constexpr aint operator%(u64 rhs) const { return {Bits, w % rhs}; }
    [[nodiscard]] constexpr aint operator>>(u64 rhs) const { return {Bits, w >> rhs}; }
    [[nodiscard]] constexpr aint operator/(i64 rhs) const { return {Bits, SExt() / rhs}; }
    [[nodiscard]] constexpr aint operator%(i64 rhs) const { return {Bits, SExt() % rhs}; }
    [[nodiscard]] constexpr aint operator>>(i64 rhs) const { return {Bits, SExt() >> rhs}; }

    [[nodiscard]] constexpr aint sdiv(aint rhs) const { return {bit_width, Word(SExt() / rhs.SExt())}; }
    [[nodiscard]] constexpr aint srem(aint rhs) const { return {bit_width, Word(SExt() % rhs.SExt())}; }
    [[nodiscard]] constexpr aint udiv(aint rhs) const { return {bit_width, w / rhs.w}; }
    [[nodiscard]] constexpr aint urem(aint rhs) const { return {bit_width, w % rhs.w}; }

    [[nodiscard]] constexpr aint shl(aint rhs) const { return {bit_width, w << rhs.w}; }
    [[nodiscard]] constexpr aint shr(aint rhs) const { return {bit_width, w >> rhs.w}; }
    [[nodiscard]] constexpr aint sar(aint rhs) const { return {bit_width, Word(SExt() >> rhs.SExt())}; }

    [[nodiscard]] constexpr aint sext(u64 bits) const {
        if (bits == bit_width) return *this;
        return {bits, SExt()};
    }
    [[nodiscard]] constexpr aint zext(u64 bits) const { return {bits, w}; }
    [[nodiscard]] constexpr aint trunc(u64 bits) const { return {bits, w}; }

    [[nodiscard]] constexpr bool ult(aint rhs) const { return w < rhs.w; }
    [[nodiscard]] constexpr bool ule(aint rhs) const { return w <= rhs.w; }
    [[nodiscard]] constexpr bool ugt(aint rhs) const { return w > rhs.w; }
    [[nodiscard]] constexpr bool uge(aint rhs) const { return w >= rhs.w; }
    [[nodiscard]] constexpr bool slt(aint rhs) const { return SExt() < rhs.SExt(); }
    [[nodiscard]] constexpr bool sle(aint rhs) const { return SExt() <= rhs.SExt(); }
    [[nodiscard]] constexpr bool sgt(aint rhs) const { return SExt() > rhs.SExt(); }
    [[nodiscard]] constexpr bool sge(aint rhs) const { return SExt() >= rhs.SExt(); }

    [[nodiscard]] constexpr bool operator>(u64 rhs) const { return w > rhs; }
    [[nodiscard]] constexpr bool operator<(u64 rhs) const { return w < rhs; }
    [[nodiscard]] constexpr bool operator>=(u64 rhs) const { return w >= rhs; }
    [[nodiscard]] constexpr bool operator<=(u64 rhs) const { return w <= rhs; }
    [[nodiscard]] constexpr bool operator>(i64 rhs) const { return SExt() > rhs; }
    [[nodiscard]] constexpr bool operator<(i64 rhs) const { return SExt() < rhs; }
    [[nodiscard]] constexpr bool operator>=(i64 rhs) const { return SExt() >= rhs; }
    [[nodiscard]] constexpr bool operator<=(i64 rhs) const { return SExt() <= rhs; }

    [[nodiscard]] constexpr Word operator*() const { return w; }
    [[nodiscard]] constexpr bool is_negative() const { return bool(w & sign_bit()); }
    [[nodiscard]] constexpr bool is_power_of_two() const { return std::has_single_bit(value()); }
    [[nodiscard]] constexpr auto bits() const -> u64 { return bit_width; }
    [[nodiscard]] constexpr Word log2() const { return Word(std::countr_zero(value())); }
    [[nodiscard]] constexpr Word popcount() const { return Word(std::popcount(value())); }
    [[nodiscard]] constexpr Word sign_bit() const { return (Word(1) << Word(bit_width - 1)); }
    [[nodiscard]] constexpr auto value() const -> Word { return w; }

    [[nodiscard]] constexpr explicit operator Word() { return w; }
    [[nodiscard]] constexpr explicit operator SWord() { return SExt(); }

    [[nodiscard]] auto str() const -> std::string { return fmt::format("{}", w); }
};
} // namespace lcc

template <>
struct fmt::formatter<lcc::aint> : fmt::formatter<lcc::aint::Word> {
    template <typename FormatContext>
    auto format(lcc::aint a, FormatContext& ctx) const {
        return formatter<lcc::aint::Word>::format(a.value(), ctx);
    }
};

template <>
struct std::hash<lcc::aint> {
    auto operator()(lcc::aint a) const noexcept {
        return std::hash<lcc::aint::Word>{}(a.value());
    }
};

#endif // LCC_AINT_HH
