#ifndef LCC_UTILS_HH
#define LCC_UTILS_HH

#include <algorithm>
#include <array>
#include <chrono>
#include <coroutine>
#include <cstdio>
#include <deque>
#include <filesystem>
#include <fmt/color.h>
#include <fmt/format.h>
#include <functional>
#include <memory>
#include <new>
#include <numeric>
#include <optional>
#include <ranges>
#include <span>
#include <string>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
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
}

/// More rarely used functions go here so as to not pollute the lcc namespace too much.
namespace lcc::utils {
/// ANSI Terminal colours.
enum struct Colour {
    Reset = 0,
    Red = 31,
    Green = 32,
    Yellow = 33,
    Blue = 34,
    Magenta = 35,
    Cyan = 36,
    White = 37,
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
    bool use_colours;
    Colours(bool use_colours) : use_colours{use_colours} {}

    auto operator()(Colour c) -> std::string_view {
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
        }
        return "";
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
    /// 1000'0000 | b   := << bits - 1 [== (a) << 7]
    /// 0111'1111 | (b - 1)
    /// 1111'1111 | res := b | (b - 1)
    ///
    /// Note: the fastest way of doing this on x86_64
    /// is actually this `-1zu >> -bits`, but that’s
    /// *super* illegal to write in C++; fortunately,
    /// the compiler can figure out what we’re doing
    /// here and will generate the same code.
    auto b = 1zu << (bits - 1zu);
    return b | (b - 1zu);
}

/// Determine the width of a number.
auto NumberWidth(usz number, usz base = 10) -> usz;

/// Replace all occurrences of `from` with `to` in `str`.
void ReplaceAll(
    std::string& str,
    std::string_view from,
    std::string_view to
);
} // namespace lcc::utils

#endif // LCC_UTILS_HH
