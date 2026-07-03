#ifndef LANGUAGE_C_PARSER_HH
#define LANGUAGE_C_PARSER_HH

#include <language_c/ast.hh>

#include <lcc/syntax/lexer.hh>
#include <lcc/syntax/token.hh>
#include <lcc/utils/result.hh>
#include <lccbase/context.hh>
#include <lccbase/file.hh>

#include <fmt/base.h>

#include <list>
#include <string>
#include <string_view>
#include <vector>

namespace lcc::language_c {

enum class TokenKind : unsigned int {
    Invalid = 0,

    // Literals
    Identifier,
    Integer,
    Fractional,
    String,

    KwVoid,
    KwBool,
    KwChar,
    KwShort,
    KwInt,
    KwLong,
    KwReturn,
    KwSizeof,
    KwAlignof,

    // Type Qualifiers
    KwConst,
    KwVolatile,
    KwRestrict,
    KwAtomic,
    KwConstexpr,

    // Storage Class Specifiers
    KwAuto,
    KwExtern,
    KwRegister,
    KwStatic,

    OpEqual,
    Assign = OpEqual,
    OpLessThan,
    OpGreaterThan,
    OpDoublePipe,
    OpDoubleAmpersand,

    OpExclamation,

    OpPlus,
    OpMinus,
    OpAsterisk,
    OpSlash,
    OpPercent,
    OpComma,
    OpDot,
    OpArrow, // ->

    OpPlusPlus,
    OpMinusMinus,

    OpCaret,
    OpPipe,
    OpAmpersand,
    OpTilde,
    OpShiftLeft,
    OpShiftRight,

    OpDoubleEqual,
    OpLessThanEqual,
    OpGreaterThanEqual,
    OpExclamationEqual,

    OpPlusEqual,
    OpMinusEqual,
    OpAsteriskEqual,
    OpSlashEqual,
    OpPercentEqual,
    OpCaretEqual,
    OpPipeEqual,
    OpAmpersandEqual,
    OpShiftLeftEqual,
    OpShiftRightEqual,

    LeftParenthesis,
    RightParenthesis,
    LeftSquareBracket,
    Subscript = LeftSquareBracket,
    RightSquareBracket,
    LeftCurlyBrace,
    RightCurlyBrace,

    Semicolon,
    Eof,
    Count,
};

using Token = syntax::Token<TokenKind>;

class Lexer : public syntax::Lexer<Token> {
    std::list<Token> _next_tokens{};
    std::list<std::vector<char>> _including{};
    usz _including_offset{};

    StringMap<std::vector<Token>> _simple_defines{};

    bool preprocessing{false};

    static constexpr std::string_view preprocessor_whitespace{" \t\f"};
    Result<void> preprocessor_define(std::string_view name, std::vector<Token> contents);
    void preprocessor_undefine(std::string_view name);

    void NextNumber();
    void NextIdentifier();

public:
    Lexer(Context* c, File* f)
        : syntax::Lexer<Token>(c, f) {}

    void NextToken();
    void NextChar();

    auto& defines() const { return _simple_defines; }
};

class Parser : Lexer {
    TranslationUnit& tu;

    std::vector<Scope*> _scopes{};
    Scope* _current_scope{};

    bool IsFunctionDefinition(Node*);

    Result<std::vector<Node*>> ParseDeclarators(Type* type_specifier);
    Result<Node*> ParseDeclarations(Type* type_specifier);
    Result<Type*> ParseBaseType(Type::flag_t flags);
    Result<Node*> ParseExpression(size_t precedence);
    Result<std::vector<Node*>> ParseExpressions(TokenKind until);

    // @param of_file  name of file that we are parsing the top level of.
    // Parses into translation unit member.
    void ParseTopLevel(std::string of_file);

public:
    Parser(Context* c, TranslationUnit& tu_, File& f)
        : Lexer(c, &f)
        , tu(tu_) {
        // Initialise first token.
        NextToken();
        // Create global scope
        _current_scope = new (tu) Scope();
        _scopes.emplace_back(_current_scope);
    }

    auto scopes() const { return _scopes; }
    auto current_scope() const { return _current_scope; }

    static auto Parse(Context*, File&) -> TranslationUnit;
};

std::string_view ToString(TokenKind k);
Result<std::string> ToSource(Token&);

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
