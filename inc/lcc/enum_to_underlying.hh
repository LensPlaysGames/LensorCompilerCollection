#ifndef LCC_ENUM_TO_UNDERLYING
#define LCC_ENUM_TO_UNDERLYING

#include <type_traits>

namespace lcc {

/// Helper to cast an enum to its underlying type.
template <typename t>
requires std::is_enum_v<t>
constexpr std::underlying_type_t<t> operator+(t e) {
    return static_cast<std::underlying_type_t<t>>(e);
}

} // namespace lcc

#endif /* LCC_ENUM_TO_UNDERLYING */
