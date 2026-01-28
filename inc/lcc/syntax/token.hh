#ifndef LCC_SYNTAX_TOKEN_HH
#define LCC_SYNTAX_TOKEN_HH

#include <lcc/core.hh>
#include <lcc/diags.hh>
#include <lcc/utils.hh>

namespace lcc::syntax {

template <typename TKind>
requires std::is_enum_v<TKind>
struct Token {
    TKind kind = TKind::Invalid;
    Location location{};
    std::string text{};
    u64 integer_value{};
    long double float_value{};
    bool artificial : 1 = false;
};

} // namespace lcc::syntax

#endif // LCC_SYNTAX_TOKEN_HH
