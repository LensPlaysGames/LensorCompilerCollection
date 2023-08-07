#ifndef LCC_UTILS_HH
#define LCC_UTILS_HH

#include <algorithm>
#include <chrono>
#include <cstdio>
#include <deque>
#include <filesystem>
#include <fmt/color.h>
#include <fmt/format.h>
#include <memory>
#include <numeric>
#include <optional>
#include <ranges>
#include <string>
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

template <typename>
concept always_false = false;

/// Helper to cast an enum to its underlying type.
template <typename t>
requires std::is_enum_v<t>
constexpr std::underlying_type_t<t> operator+(t e) {
    return static_cast<std::underlying_type_t<t>>(e);
}
} // namespace lcc

/// Forward decls.
namespace lcc::detail {
[[noreturn]] void AssertFail(std::string&& msg);
} // namespace lcc::detail

/// More rarely used functions go here so as to not pollute the lcc namespace too much.
namespace lcc::utils {
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
