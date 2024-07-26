#ifndef LCC_UTILS_TWO_COLUMN_LAYOUT_HELPER_HH
#define LCC_UTILS_TWO_COLUMN_LAYOUT_HELPER_HH

#include <string>
#include <string_view>
#include <vector>

struct TwoColumnLayoutHelper {
    struct Line {
        std::string_view a;
        std::string_view b;
    };
    std::vector<Line> lines;

    /// Gap inserted between A and B columns.
    std::string gap{"    "};

    char padding_character{' '};

    [[nodiscard]]
    constexpr auto get() const -> std::string {
        size_t longest_a_side = 0;
        for (const auto& c : lines) {
            if (c.a.size() > longest_a_side)
                longest_a_side = c.a.size();
        }

        std::string out{};
        for (const auto& c : lines) {
            out += c.a;

            if (not c.b.empty()) {
                // Do padding if needed to line up b side
                for (size_t i = c.a.size(); i < longest_a_side; ++i)
                    out += padding_character;

                out += gap;
                out += c.b;
            }
        }

        return out;
    }

    [[nodiscard]] explicit constexpr operator std::string() const {
        return get();
    }
};

#endif /* LCC_UTILS_TWO_COLUMN_LAYOUT_HELPER_HH */
