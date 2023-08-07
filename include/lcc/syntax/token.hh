#ifndef LCC_SYNTAX_TOKEN_HH
#define LCC_SYNTAX_TOKEN_HH

#include <lcc/core.hh>
#include <lcc/utils.hh>
#include <lcc/diags.hh>

namespace lcc::syntax {
template <typename TKind>
requires std::is_enum_v<TKind>
struct Token {
    TKind kind;
    Location location;
    std::string text_value;
    u64 integer_value;
    bool artificial : 1 = false;

    Token(TKind kind, Location location)
        : kind(kind), location(location) { }

    Token(TKind kind, Location location, std::string textValue)
        : kind(kind), location(location), text_value(std::move(textValue)) { }

    Token(TKind kind, Location location, u64 integerValue)
        : kind(kind), location(location), integer_value(integerValue) { }
};
}

#endif // LCC_SYNTAX_TOKEN_HH
