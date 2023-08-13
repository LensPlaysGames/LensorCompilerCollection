#ifndef LCC_SYNTAX_TOKEN_HH
#define LCC_SYNTAX_TOKEN_HH

#include <lcc/core.hh>
#include <lcc/utils.hh>
#include <lcc/diags.hh>

namespace lcc::syntax {
template <typename TKind>
requires std::is_enum_v<TKind>
struct Token {
    TKind kind = TKind::Invalid;
    Location location{};
    std::string text{};
    u64 integer_value{};
    bool artificial : 1 = false;

    bool operator ==(const Token<TKind>& rhs) const {
        if (this == &rhs) return true;
        if (kind != rhs.kind) return false;
        if (text != rhs.text) return false;
        if (integer_value != rhs.integer_value) return false;
        if (artificial != rhs.artificial) return false;
        return true;
    }
};
}

#endif // LCC_SYNTAX_TOKEN_HH
