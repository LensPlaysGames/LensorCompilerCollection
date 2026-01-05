#include <glint/error_ids.hh>

#include <lcc/utils.hh>

#include <array>
#include <utility>

namespace lcc::glint {

const std::array<
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
