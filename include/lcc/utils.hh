#ifndef LCC_UTILS_HH
#define LCC_UTILS_HH

#include <fmt/color.h>
#include <fmt/compile.h>
#include <fmt/format.h>

#include <algorithm>
#include <array>
#include <chrono>
#include <coroutine>
#include <cstdio>
#include <deque>
#include <filesystem>
#include <functional>
#include <iterator>
#include <memory>
#include <new>
#include <numeric>
#include <optional>
#include <ranges>
#include <span>
#include <string>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

namespace lcc {
using namespace std::literals;

namespace fs = std::filesystem;
namespace chr = std::chrono;
namespace rgs = std::ranges;
namespace vws = std::ranges::views;

using u8 = uint8_t;
using u16 = uint16_t;
using u32 = uint32_t;
using u64 = uint64_t;
using usz = size_t;
using uptr = uintptr_t;
using uint = unsigned int;

using i8 = int8_t;
using i16 = int16_t;
using i32 = int32_t;
using i64 = int64_t;
using isz = ptrdiff_t;
using iptr = intptr_t;

using f32 = float;
using f64 = double;

#define LCC_STR_(X) #X
#define LCC_STR(X)  LCC_STR_(X)

#define LCC_CAT_(X, Y) X##Y
#define LCC_CAT(X, Y)  LCC_CAT_(X, Y)

// clang-format off
#define LCC_ASSERT(cond, ...) (cond ? void(0) :                \
    ::lcc::detail::AssertFail(                                 \
        fmt::format(                                           \
            "Assertion failed: \"" #cond "\" in {} at line {}" \
            __VA_OPT__(".\nMessage: {}"), __FILE__, __LINE__   \
            __VA_OPT__(, fmt::format(__VA_ARGS__))             \
        )                                                      \
    )                                                          \
)
// clang-format on

#define LCC_UNREACHABLE() LCC_ASSERT(false, "Unreachable")

#define LCC_TODO(...) LCC_ASSERT(false, "TODO" __VA_OPT__(": {}", fmt::format(__VA_ARGS__)))

template <typename>
concept always_false = false;

/// Helper to cast an enum to its underlying type.
template <typename t>
requires std::is_enum_v<t>
constexpr std::underlying_type_t<t> operator+(t e) {
    return static_cast<std::underlying_type_t<t>>(e);
}
} // namespace lcc

/// Internal stuff.
namespace lcc::detail {
[[noreturn]] void AssertFail(std::string&& msg);

/// Hash for string maps.
struct StringHash {
    using is_transparent = void;

    [[nodiscard]] usz operator()(std::string_view txt) const { return std::hash<std::string_view>{}(txt); }
    [[nodiscard]] usz operator()(const std::string& txt) const { return std::hash<std::string>{}(txt); }

    template <typename Type>
    requires requires (const Type& t) {
        { t.size() } -> std::convertible_to<usz>;
        { t.data() } -> std::convertible_to<const char*>;
    }
    [[nodiscard]] usz operator()(const Type& txt) const {
        return std::hash<std::string_view>{}(std::string_view{txt.data(), txt.size()});
    }
};

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

    consteval static_string(const char (&raw)[sz]) : elem_count{sz} {
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
/// Map with heterogeneous lookup.
///
/// Use this whenever you need a \c std::unordered_map from strings
/// to some other type. The reason for this is that, by default,
/// \c std::unordered_map will require you to create a \c std::string
/// to look up a value; this is annoying if you only have a
/// \c std::string_view, because you have to create a copy just to
/// search for something.
///
/// There is an API to fix that, but it’s not exactly obvious, so this
/// takes care of that. This map type allows you to use pretty much
/// anything that supports \c data() and \c size() accessors as a key.
template <typename Type>
using StringMap = std::unordered_map<std::string, Type, detail::StringHash, std::equal_to<>>;

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
        : buffer{std::make_unique<T[]>(size)},
          element_count{size} {}

    /// Create a new buffer that can hold N elements and initialize it with a value.
    explicit Buffer(usz size, T val)
        : Buffer{size} {
        std::fill(begin(), end(), val);
    }

    /// Create a new buffer from an iterator range.
    template <typename Iter>
    explicit Buffer(Iter begin, Iter end)
        : buffer{std::make_unique<T[]>(std::distance(begin, end))},
          element_count{std::distance(begin, end)} {
        std::copy(begin, end, buffer.get());
    }

    /// Create a new buffer from a range.
    template <typename Range>
    requires (not std::is_same_v<std::remove_cvref_t<Range>, Buffer<T>>)
    explicit Buffer(Range&& range) : Buffer{std::begin(range), std::end(range)} {}

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
        LCC_ASSERT(idx < size(), "Index out of bounds");
    }
};

/// Compile time fmt::format.
template <detail::static_string format, auto... Args>
consteval auto ConstexprFormat() -> std::string_view {
    static constexpr usz size = fmt::formatted_size(FMT_COMPILE(format.view()), Args...);
    static constexpr std::array<char, size> data = [] {
        std::array<char, size> d{};
        fmt::format_to(d.begin(), FMT_COMPILE(format.view()), Args...);
        return d;
    }();

    /// Exclude the null terminator.
    return {data.data(), size - 1};
}

} // namespace lcc

/// More rarely used functions go here so as to not pollute the lcc namespace too much.
namespace lcc::utils {
/// ANSI Terminal colours.
/// If you are writing something in bold and red, use BoldRed. If you are
/// writing everything in bold, and some things in red, use C(Bold),
/// C(Red) and C(Default).
enum struct Colour {
    Reset = 0,

    // Composable...
    // Apply bold styling.
    Bold = 1,
    // Apply faint styling.
    Faint = 2,
    // Apply strikethrough styling.
    Strikethrough = 9,
    // Apply color.
    Red = 31,
    Green = 32,
    Yellow = 33,
    Blue = 34,
    Magenta = 35,
    Cyan = 36,
    White = 37,
    Default = 39,

    // Non-composable
    // Reset styling, then apply color.
    Normal = 0,
    NormalRed = 3000,
    NormalGreen,
    NormalYellow,
    NormalBlue,
    NormalMagenta,
    NormalCyan,
    NormalWhite,
    NormalDefault,

    // Apply bold styling and color.
    BoldRed = 1000,
    BoldGreen,
    BoldYellow,
    BoldBlue,
    BoldMagenta,
    BoldCyan,
    BoldWhite,
    BoldDefault,

    // Apply faint styling and color.
    FaintRed = 2000,
    FaintGreen,
    FaintYellow,
    FaintBlue,
    FaintMagenta,
    FaintCyan,
    FaintWhite,
    FaintDefault,
};

/// RAII helper to toggle colours when printing.
///
/// Example:
/// \code{.cpp}
///     using enum Colour;
///     Colours C{true};
///     out += C(Red);
///     out += fmt::format("{}foo{}", C(Green), C(Reset));
/// \endcode
struct Colours {
    bool use_colours{};
    constexpr Colours(bool should_use_colours) : use_colours{should_use_colours} {}
    constexpr auto operator()(Colour c) const -> std::string_view {
        if (not use_colours) return "";
        switch (c) {
            case Colour::Reset: return "\033[m";
            case Colour::Red: return "\033[31m";
            case Colour::Green: return "\033[32m";
            case Colour::Yellow: return "\033[33m";
            case Colour::Blue: return "\033[34m";
            case Colour::Magenta: return "\033[35m";
            case Colour::Cyan: return "\033[36m";
            case Colour::White: return "\033[37m";
            case Colour::Default: return "\033[39m";

            case Colour::NormalRed: return "\033[0;31m";
            case Colour::NormalGreen: return "\033[0;32m";
            case Colour::NormalYellow: return "\033[0;33m";
            case Colour::NormalBlue: return "\033[0;34m";
            case Colour::NormalMagenta: return "\033[0;35m";
            case Colour::NormalCyan: return "\033[0;36m";
            case Colour::NormalWhite: return "\033[0;37m";
            case Colour::NormalDefault: return "\033[0;39m";

            case Colour::Bold: return "\033[1m";
            case Colour::BoldRed: return "\033[1;31m";
            case Colour::BoldGreen: return "\033[1;32m";
            case Colour::BoldYellow: return "\033[1;33m";
            case Colour::BoldBlue: return "\033[1;34m";
            case Colour::BoldMagenta: return "\033[1;35m";
            case Colour::BoldCyan: return "\033[1;36m";
            case Colour::BoldWhite: return "\033[1;37m";
            case Colour::BoldDefault: return "\033[1;39m";

            case Colour::Faint: return "\033[2m";
            case Colour::FaintRed: return "\033[2;31m";
            case Colour::FaintGreen: return "\033[2;32m";
            case Colour::FaintYellow: return "\033[2;33m";
            case Colour::FaintBlue: return "\033[2;34m";
            case Colour::FaintMagenta: return "\033[2;35m";
            case Colour::FaintCyan: return "\033[2;36m";
            case Colour::FaintWhite: return "\033[2;37m";
            case Colour::FaintDefault: return "\033[2;39m";

            case Colour::Strikethrough: return "\033[9m";
        }
        LCC_UNREACHABLE();
    }
};

/// Align a value to a given alignment.
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
auto NumberWidth(usz number, usz base = 10) -> usz;

/// Replace all occurrences of `from` with `to` in `str`.
void ReplaceAll(
    std::string& str,
    std::string_view from,
    std::string_view to
);

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

} // namespace lcc::utils

template <lcc::detail::FormattableEnum T>
struct fmt::formatter<T> {
    template <typename ParseContext>
    constexpr auto parse(ParseContext& ctx) { return ctx.begin(); }

    template <typename FormatContext>
    auto format(T t, FormatContext& ctx) {
        return fmt::format_to(ctx.out(), "{}", StringifyEnum(t));
    }
};

#endif // LCC_UTILS_HH
