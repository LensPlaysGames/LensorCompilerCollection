#ifndef LCC_UTILS_HH
#define LCC_UTILS_HH

#include <algorithm>
#include <chrono>
#include <cstdio>
#include <deque>
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

template <typename>
concept always_false = false;

template <typename... arguments>
[[noreturn]] void fatal(fmt::format_string<arguments...> fmt, arguments&&... args) {
    fmt::print(stderr, fmt, std::forward<arguments>(args)...);
    fmt::print(stderr, "\n");
    std::exit(1);
}

} // namespace lcc

#endif // LCC_UTILS_HH
