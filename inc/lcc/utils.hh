#ifndef LCC_UTILS_HH
#define LCC_UTILS_HH

// TODO: Get rid of this file! We do _not_ want any "catch-all" headers
// like "misc", "utils", etc. It should be it's own library, or it should
// have it's own file in the LCC library, if it's worth implementing.

#include <hdronly/lcc/typedefs.hh>

#include <lccbase/assert.hh>

#include <fmt/color.h>
#include <fmt/compile.h>
#include <fmt/core.h>
#include <fmt/format.h>
#include <fmt/ranges.h>
#include <fmt/std.h>

#include <algorithm>
#include <array>
#include <concepts>
#include <cstring>
#include <deque>
#include <iterator>
#include <memory>
#include <ranges>
#include <span>
#include <string>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

/// Internal stuff.
namespace lcc::detail {

/// Enum that has a StringifyEnum() function.
template <typename T>
concept FormattableEnum = requires (T t) {
    requires std::is_enum_v<T>;
    { StringifyEnum(t) } -> std::convertible_to<std::string_view>;
};

/// Compile-time string that can be passed as a template parameter.
template <usz sz>
struct static_string {
    char chars[sz]{};
    usz elem_count{};

    consteval static_string() {}

    consteval static_string(const char (&raw)[sz])
        : elem_count{sz} {
        std::copy_n(raw, sz, chars);
    }

    constexpr char operator[](usz idx) const {
        LCC_ASSERT(idx < size(), "Index out of bounds");
        return chars[idx];
    }

    constexpr auto data() const -> const char* { return chars; }
    constexpr auto size() const -> usz { return elem_count; }
    constexpr auto view() const -> std::string_view { return {data(), size()}; }
};

} // namespace lcc::detail

namespace lcc {

/// Fixed-sized buffer, intended to be used where a vector
/// could be used, but resizing is not needed.
template <typename T>
requires (not std::is_reference_v<T>, not std::is_const_v<T>)
class Buffer {
    /// The buffer.
    std::unique_ptr<T[]> buffer{};
    usz element_count{};

public:
    using value_type = T;
    using reference = T&;
    using const_reference = const T&;
    using iterator = T*;
    using const_iterator = const T*;

    /// Create an empty buffer.
    explicit Buffer() = default;

    /// Create a new buffer that can hold N elements.
    explicit Buffer(usz size)
        : buffer{std::make_unique<T[]>(size)}
        , element_count{size} {}

    /// Create a new buffer that can hold N elements and initialize it with a value.
    explicit Buffer(usz size, T val)
        : Buffer{size} {
        std::fill(begin(), end(), val);
    }

    /// Create a new buffer from an iterator range.
    template <typename Iter>
    explicit Buffer(Iter begin, Iter end)
        : buffer{std::make_unique<T[]>(std::distance(begin, end))}
        , element_count{std::distance(begin, end)} {
        std::copy(begin, end, buffer.get());
    }

    /// Create a new buffer from a range.
    template <typename Range>
    requires (not std::is_same_v<std::remove_cvref_t<Range>, Buffer<T>>)
    explicit Buffer(Range&& range)
        : Buffer{std::begin(range), std::end(range)} {}

    /// Get an iterator to the start of the buffer.
    auto begin() const -> const_iterator { return buffer.get(); }
    auto begin() -> iterator { return buffer.get(); }

    /// Get a pointer to the buffer data.
    auto data() const -> const T* { return buffer.get(); }
    auto data() -> T* { return buffer.get(); }

    /// Check if the buffer is empty.
    auto empty() const -> bool { return size() == 0; }

    /// Get an iterator to the end of the buffer.
    auto end() const -> const_iterator { return buffer.get() + size(); }
    auto end() -> iterator { return buffer.get() + size(); }

    /// Get the buffer size.
    auto size() const -> usz { return element_count; }

    /// Access an element of the buffer.
    auto operator[](usz idx) const -> const_reference {
        CheckInbounds(idx);
        return buffer[idx];
    }

    /// Access an element of the buffer.
    auto operator[](usz idx) -> reference {
        CheckInbounds(idx);
        return buffer[idx];
    }

    /// Access a range of elements of the buffer.
    auto operator[](usz start, usz end) const -> std::span<const T> {
        CheckInbounds(start);
        CheckInbounds(end);
        return std::span{buffer.get() + start, end - start};
    }

    /// Access a range of elements of the buffer.
    auto operator[](usz start, usz end) -> std::span<T> {
        CheckInbounds(start);
        CheckInbounds(end);
        return std::span{buffer.get() + start, end - start};
    }

private:
    void CheckInbounds(usz idx) const {
        LCC_ASSERT(idx < size(), "Index out of bounds: {} >= {}", idx, size());
    }
};

} // namespace lcc

/// More rarely used functions go here so as to not pollute the lcc namespace too much.
namespace lcc::utils {

/// Align a value to a given alignment.
/// Will never return a value less than given value.
template <typename T = usz>
constexpr T AlignTo(T value, T align) {
    LCC_ASSERT(align != 0);
    const auto padding = (align - (value % align)) % align;
    return value + padding;
}

/// Compute the maximum value of an n-bit integer.
constexpr usz MaxBitValue(usz bits) {
    /// Example for 8 bits:
    ///
    /// 0000'0001 | 1
    /// 1000'0000 | b := 1 << bits - 1
    /// 0111'1111 | (b - 1)
    /// 1111'1111 | res := b | (b - 1)
    ///
    /// Note: the fastest way of doing this on x86_64 is actually this
    ///   `-1zu >> -bits`,
    /// but that’s *super* illegal to write in C++; fortunately, the
    /// compiler can figure out what we’re doing here and will generate the
    /// same code.
    auto b = usz(1) << (bits - usz(1));
    return b | (b - usz(1));
}

/// Determine the width of a number.
// auto NumberWidth(usz number, usz base = 10) -> usz;

/// Replace all occurrences of `from` with `to` in `str`.
// void ReplaceAll(
//     std::string& str,
//     std::string_view from,
//     std::string_view to
// );

/// Invoke a templated lambda or class w/ a templated operator().
///
/// \code
///   auto f = [] <typename T> (int x) { ... };
///   invoke_template<int>(f, 42);
/// \endcode
template <typename... TemplateArgs, typename... Args, typename Templ>
auto invoke_template(Templ&& t, Args&&... args) -> decltype(auto) {
    return std::forward<Templ>(t).template operator()<TemplateArgs...>(std::forward<Args>(args)...);
}

/// Convert a range to a container.
template <rgs::range Range>
auto to_vec(Range r) -> std::vector<rgs::range_value_t<Range>> {
    std::vector<rgs::range_value_t<Range>> out{};
    out.insert(out.end(), rgs::begin(r), rgs::end(r));
    return out;
}

// FIXME: This should probably be backwards for big endian machines, afaik.
template <typename T>
auto to_bytes(const T object) -> std::array<lcc::u8, sizeof(T) / sizeof(lcc::u8)> {
    std::array<lcc::u8, sizeof(T)> out{};
    const lcc::u8* begin = reinterpret_cast<const lcc::u8*>(&object);
    const lcc::u8* end = begin + (sizeof(T));
    std::copy(begin, end, out.begin());
    return out;
}

// FIXME: This should probably be backwards for big endian machines, afaik.
// FIXME: requires std::is_trivially_constructible?
template <typename T>
auto from_bytes(std::array<lcc::u8, sizeof(T)> bytes) -> T {
    T out{};
    std::memcpy(&out, bytes.data(), sizeof(T));
    return out;
}

} // namespace lcc::utils

template <lcc::detail::FormattableEnum T>
struct fmt::formatter<T> {
    template <typename ParseContext>
    constexpr auto parse(ParseContext& ctx) { return ctx.begin(); }

    template <typename FormatContext>
    auto format(T t, FormatContext& ctx) const {
        return fmt::format_to(ctx.out(), "{}", StringifyEnum(t));
    }
};

#endif // LCC_UTILS_HH
