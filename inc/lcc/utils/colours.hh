#ifndef LCC_UTILS_COLOURS_HH
#define LCC_UTILS_COLOURS_HH

#include <string_view>
#include <utility>

namespace lcc {
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
    constexpr Colours(bool should_use_colours)
        : use_colours{should_use_colours} {}
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
        std::unreachable();
    }
};

} // namespace lcc

#endif /* LCC_UTILS_COLOURS_HH */
