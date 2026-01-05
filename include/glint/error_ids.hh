#ifndef LCC_GLINT_ERROR_IDS_HH
#define LCC_GLINT_ERROR_IDS_HH

#include <lcc/utils.hh>

#include <array>
#include <utility>

namespace lcc::glint {

// NOTE: If you add an ID, please also add a mapping in
//       @see error_id_strings
enum struct ErrorId {
    INVALID,

    InvalidByteLiteral,
    InvalidIntegerLiteral,
    InvalidStringLiteral,

    COUNT
};

extern const std::array<std::pair<ErrorId, const char*>, +ErrorId::COUNT + 1>
    error_id_strings;

} // namespace lcc::glint

#endif /* LCC_GLINT_ERROR_IDS_HH */
