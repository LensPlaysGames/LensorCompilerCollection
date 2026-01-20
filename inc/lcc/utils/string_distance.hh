#ifndef LCC_UTILS_STRING_DISTANCE_HH
#define LCC_UTILS_STRING_DISTANCE_HH

#include <algorithm>
#include <cstddef>
#include <string>
#include <string_view>

namespace lcc::utils {
auto optimal_string_alignment_distance(
    std::string_view s,
    std::string_view t
) -> size_t;

} // namespace lcc::utils

#endif /* LCC_UTILS_STRING_DISTANCE_HH */
