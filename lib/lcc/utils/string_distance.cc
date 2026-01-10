#include <lcc/utils/string_distance.hh>

#include <algorithm>
#include <cstddef>
#include <string>
#include <string_view>

namespace lcc::utils {
auto optimal_string_alignment_distance(
    std::string_view s,
    std::string_view t
) -> size_t {
    auto m = s.size();
    auto n = t.size();
    // Allocate 2d array
    // d :: [uint (m + 1) * (n + 1)]; <- equivalent in Glint
    size_t* d = (decltype(d))
        calloc(
            (m + 1) * (n + 1),
            sizeof(typeof(*d))
        );

    auto ref = [d, n](size_t i, size_t j) -> size_t& {
        return d[(i * n) + j];
    };

    for (size_t i = 0; i <= m; ++i) ref(i, 0) = i;
    for (size_t j = 0; j <= n; ++j) ref(0, j) = j;

    for (size_t j = 1; j <= n; ++j) {
        auto j_i = j - 1;
        for (size_t i = 1; i <= m; ++i) {
            auto i_i = i - 1;
            // SUBSTITUTION CHECK
            size_t cost{0};
            if (s.at(i_i) != t.at(j_i))
                cost = 1;

            ref(i, j) = std::min(
                {ref(i - 1, j) + 1,
                 ref(i, j - 1) + 1,
                 ref(i - 1, j - 1) + cost}
            );

            // TRANSPOSITION CHECK
            // abcd and acbd are very likely closer in distance, so we do that.
            if (i > 1 and j > 1
                and s.at(i_i) == t.at(j_i - 1)
                and s.at(i_i - 1) == t.at(j_i)) {
                ref(i, j) = std::min(
                    {ref(i, j),
                     ref(i - 2, j - 2) + 1}
                );
            }
        }
    }
    size_t out = ref(m, n);
    free(d);
    return out;
}

} // namespace lcc::utils
