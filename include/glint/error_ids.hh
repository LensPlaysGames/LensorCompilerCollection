#ifndef LCC_GLINT_ERROR_IDS_HH
#define LCC_GLINT_ERROR_IDS_HH

#include <lcc/utils.hh>

#include <array>
#include <utility>

namespace lcc::glint {

// NOTE: If you add an ID, please also add a mapping in
//       @see error_id_strings
enum struct ErrorId : unsigned {
    INVALID,

    InvalidByteLiteral,
    InvalidIntegerLiteral,
    InvalidStringLiteral,

    Expected,

    Miscellaneous,

    COUNT
};

constexpr std::array<
    std::pair<ErrorId, const char*>,
    7>
    error_id_strings{
        std::pair{ErrorId::INVALID, "ICE: INVALID ERROR ID"},

        {ErrorId::InvalidByteLiteral, "invalid-literal/byte"},
        {ErrorId::InvalidIntegerLiteral, "invalid-literal/integer"},
        {ErrorId::InvalidStringLiteral, "invalid-literal/string"},

        {ErrorId::Expected, "expected"},

        {ErrorId::Miscellaneous, "miscellaneous"},

        {ErrorId::COUNT, "ICE: INVALID ERROR ID"}
    };

static_assert(
    error_id_strings.size() == +ErrorId::COUNT + 1,
    "Exhaustive handling of ErrorId"
);

} // namespace lcc::glint

#endif /* LCC_GLINT_ERROR_IDS_HH */
