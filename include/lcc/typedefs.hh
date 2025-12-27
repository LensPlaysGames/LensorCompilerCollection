#ifndef LCC_TYPEDEFS_HH
#define LCC_TYPEDEFS_HH

#include <chrono>
#include <cstdint>
#include <filesystem>
#include <ranges>
#include <string>

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
} // namespace lcc

#endif /* LCC_TYPEDEFS_HH */
