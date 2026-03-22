#include <string>

#include <fmt/format.h>

namespace lcc::detail {
[[noreturn]] void AssertFail(std::string&& msg);
}

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
