#ifndef LANGUAGE_C_PARSER_HH
#define LANGUAGE_C_PARSER_HH

#include <language_c/ast.hh>

#include <lcc/context.hh>
#include <lcc/file.hh>
#include <lcc/syntax/lexer.hh>
#include <lcc/syntax/token.hh>
#include <lcc/utils/result.hh>

#include <fmt/base.h>

#include <string>
#include <vector>

namespace lcc::language_c {

enum class TokenKind : unsigned int {
    Invalid = 0,

    Identifier,
    Integer,
    Fractional,

    KwInt,
    KwReturn,

    OpAsterisk,
    OpComma,

    LeftParenthesis,
    RightParenthesis,
    LeftSquareBracket,
    RightSquareBracket,
    LeftCurlyBrace,
    RightCurlyBrace,

    Semicolon,
    Eof,
    Count,
};
using Token = syntax::Token<TokenKind>;
using Lexer = syntax::Lexer<Token>;

class Parser : Lexer {
    Node* tree{};

    std::vector<Scope*> _scopes{};
    Scope* _current_scope{};

private:
    void NextToken();
    void NextNumber();

    [[nodiscard]]
    Result<std::vector<Node*>> ParseDeclarators(Type* type_specifier);

    [[nodiscard]]
    Result<Node*> ParseExpression();

    // @param of_file
    // name of file that we are parsing the top level of.
    auto ParseTopLevel(std::string of_file) -> TranslationUnit;

public:
    Parser(Context* c, File& f) : Lexer(c, &f) {
        // Initialise first token.
        NextToken();
        // Create global scope
        _current_scope = new Scope();
        _scopes.emplace_back(_current_scope);
    }

    auto scopes() const { return _scopes; }
    auto current_scope() const { return _current_scope; }

    static auto Parse(Context*, File&) -> TranslationUnit;
};

std::string_view ToString(TokenKind k);

} // namespace lcc::language_c

template <>
struct fmt::formatter<lcc::language_c::TokenKind> {
    constexpr auto parse(format_parse_context& ctx) { return ctx.begin(); }
    auto format(lcc::language_c::TokenKind k, format_context& ctx) const
        -> format_context::iterator {
        return fmt::format_to(
            ctx.out(),
            "{}",
            lcc::language_c::ToString(k)
        );
    }
};

#endif /* LANGUAGE_C_PARSER_HH */
