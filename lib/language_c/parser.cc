#include <language_c/parser.hh>

#include <hdronly/lcc/fixcompilers.hh>

#include <language_c/ast.hh>
#include <language_c/type.hh>

#include <lcc/utils/macros.hh>
#include <lcc/utils/result.hh>

#include <lccbase/diags.hh>

#include <fmt/format.h>
#include <fmt/std.h>

#include <cstdlib>
#include <filesystem>
#include <ranges>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace lcc::language_c {

namespace {

[[nodiscard]]
Type::flag_t flag_from_keyword(TokenKind k) {
    switch (k) {
        case TokenKind::KwConst:
            return Type::flag_value(Type::Flag::Const);
        case TokenKind::KwVolatile:
            return Type::flag_value(Type::Flag::Volatile);
        case TokenKind::KwRestrict:
            return Type::flag_value(Type::Flag::Restrict);
        case TokenKind::KwAtomic:
            return Type::flag_value(Type::Flag::Atomic);
        case TokenKind::KwConstexpr:
            return Type::flag_value(Type::Flag::Constexpr)
                 | Type::flag_value(Type::Flag::Const);
        case TokenKind::KwExtern:
            return Type::flag_value(Type::Flag::Extern);
        case TokenKind::KwRegister:
            return Type::flag_value(Type::Flag::Register);
        case TokenKind::KwStatic:
            return Type::flag_value(Type::Flag::Static);

        case TokenKind::KwAuto:
        case TokenKind::Invalid:
        case TokenKind::Identifier:
        case TokenKind::Integer:
        case TokenKind::Fractional:
        case TokenKind::String:
        case TokenKind::KwVoid:
        case TokenKind::KwBool:
        case TokenKind::KwChar:
        case TokenKind::KwShort:
        case TokenKind::KwInt:
        case TokenKind::KwLong:
        case TokenKind::KwReturn:
        case TokenKind::KwSizeof:
        case TokenKind::KwAlignof:
        case TokenKind::OpEqual:
        case TokenKind::OpLessThan:
        case TokenKind::OpGreaterThan:
        case TokenKind::OpDoublePipe:
        case TokenKind::OpDoubleAmpersand:
        case TokenKind::OpExclamation:
        case TokenKind::OpPlus:
        case TokenKind::OpMinus:
        case TokenKind::OpAsterisk:
        case TokenKind::OpSlash:
        case TokenKind::OpPercent:
        case TokenKind::OpComma:
        case TokenKind::OpDot:
        case TokenKind::OpArrow:
        case TokenKind::OpPlusPlus:
        case TokenKind::OpMinusMinus:
        case TokenKind::OpCaret:
        case TokenKind::OpPipe:
        case TokenKind::OpAmpersand:
        case TokenKind::OpTilde:
        case TokenKind::OpShiftLeft:
        case TokenKind::OpShiftRight:
        case TokenKind::OpDoubleEqual:
        case TokenKind::OpLessThanEqual:
        case TokenKind::OpGreaterThanEqual:
        case TokenKind::OpExclamationEqual:
        case TokenKind::OpPlusEqual:
        case TokenKind::OpMinusEqual:
        case TokenKind::OpAsteriskEqual:
        case TokenKind::OpSlashEqual:
        case TokenKind::OpPercentEqual:
        case TokenKind::OpCaretEqual:
        case TokenKind::OpPipeEqual:
        case TokenKind::OpAmpersandEqual:
        case TokenKind::OpShiftLeftEqual:
        case TokenKind::OpShiftRightEqual:
        case TokenKind::LeftParenthesis:
        case TokenKind::RightParenthesis:
        case TokenKind::LeftSquareBracket:
        case TokenKind::RightSquareBracket:
        case TokenKind::LeftCurlyBrace:
        case TokenKind::RightCurlyBrace:
        case TokenKind::Semicolon:
        case TokenKind::Eof:
        case TokenKind::Count:
        case TokenKind::PpEndif:
            break;
    }
    Diag::ICE("unreachable");
}

} // namespace

std::string_view ToString(TokenKind k) {
    switch (k) {
        case TokenKind::Invalid: return "invalid";
        case TokenKind::Identifier: return "identifier";
        case TokenKind::Integer: return "integer-literal";
        case TokenKind::Fractional: return "float-literal";
        case TokenKind::String: return "string-literal";
        case TokenKind::KwVoid: return "void";
        case TokenKind::KwBool: return "bool";
        case TokenKind::KwChar: return "char";
        case TokenKind::KwShort: return "short";
        case TokenKind::KwInt: return "int";
        case TokenKind::KwLong: return "long";
        case TokenKind::KwReturn: return "return";
        case TokenKind::KwSizeof: return "sizeof";
        case TokenKind::KwAlignof: return "_Alignof";
        case TokenKind::KwConst: return "const";
        case TokenKind::KwVolatile: return "volatile";
        case TokenKind::KwRestrict: return "restrict";
        case TokenKind::KwAtomic: return "_Atomic";
        case TokenKind::KwConstexpr: return "constexpr";
        case TokenKind::KwAuto: return "auto";
        case TokenKind::KwExtern: return "extern";
        case TokenKind::KwRegister: return "register";
        case TokenKind::KwStatic: return "static";
        case TokenKind::OpEqual: return "=";
        case TokenKind::OpLessThan: return "<";
        case TokenKind::OpGreaterThan: return ">";
        case TokenKind::OpDoublePipe: return "||";
        case TokenKind::OpDoubleAmpersand: return "&&";
        case TokenKind::OpExclamation: return "!";
        case TokenKind::OpPlus: return "+";
        case TokenKind::OpMinus: return "-";
        case TokenKind::OpAsterisk: return "*";
        case TokenKind::OpSlash: return "/";
        case TokenKind::OpPercent: return "%";
        case TokenKind::OpComma: return ",";
        case TokenKind::OpDot: return ".";
        case TokenKind::OpArrow: return "->";
        case TokenKind::OpPlusPlus: return "++";
        case TokenKind::OpMinusMinus: return "--";
        case TokenKind::OpCaret: return "^";
        case TokenKind::OpPipe: return "|";
        case TokenKind::OpAmpersand: return "&";
        case TokenKind::OpTilde: return "~";
        case TokenKind::OpShiftLeft: return "<<";
        case TokenKind::OpShiftRight: return ">>";
        case TokenKind::OpDoubleEqual: return "==";
        case TokenKind::OpLessThanEqual: return "<=";
        case TokenKind::OpGreaterThanEqual: return ">=";
        case TokenKind::OpExclamationEqual: return "!=";
        case TokenKind::OpPlusEqual: return "+=";
        case TokenKind::OpMinusEqual: return "-=";
        case TokenKind::OpAsteriskEqual: return "*=";
        case TokenKind::OpSlashEqual: return "/=";
        case TokenKind::OpPercentEqual: return "%=";
        case TokenKind::OpCaretEqual: return "^=";
        case TokenKind::OpPipeEqual: return "|=";
        case TokenKind::OpAmpersandEqual: return "&=";
        case TokenKind::OpShiftLeftEqual: return "<<=";
        case TokenKind::OpShiftRightEqual: return ">>=";
        case TokenKind::LeftParenthesis: return "(";
        case TokenKind::RightParenthesis: return ")";
        case TokenKind::LeftSquareBracket: return "[";
        case TokenKind::RightSquareBracket: return "]";
        case TokenKind::LeftCurlyBrace: return "{";
        case TokenKind::RightCurlyBrace: return "}";
        case TokenKind::Semicolon: return ";";
        case TokenKind::Eof: return "EOF";
        case TokenKind::PpEndif: return "#endif";
        case TokenKind::Count: break;
    }
    Diag::ICE("unreachable");
}

Result<std::string> ToSource(Token& t) {
    switch (t.kind) {
        case TokenKind::Eof:
        case TokenKind::Count:
        case TokenKind::Invalid:
            return Diag::Error("c/token-to-source", "invalid token");

        case TokenKind::Identifier:
            return t.text;

        case TokenKind::Integer:
            return fmt::format("{}", t.integer_value);

        case TokenKind::Fractional:
            return fmt::format("{}", t.fractional_value);

        case TokenKind::String:
            return fmt::format("\"{}\"", t.text);

        case TokenKind::KwVoid:
        case TokenKind::KwBool:
        case TokenKind::KwChar:
        case TokenKind::KwShort:
        case TokenKind::KwInt:
        case TokenKind::KwLong:
        case TokenKind::KwReturn:
        case TokenKind::KwSizeof:
        case TokenKind::KwAlignof:
        case TokenKind::KwConst:
        case TokenKind::KwVolatile:
        case TokenKind::KwRestrict:
        case TokenKind::KwAtomic:
        case TokenKind::KwConstexpr:
        case TokenKind::KwAuto:
        case TokenKind::KwExtern:
        case TokenKind::KwRegister:
        case TokenKind::KwStatic:
        case TokenKind::OpEqual:
        case TokenKind::OpLessThan:
        case TokenKind::OpGreaterThan:
        case TokenKind::OpDoublePipe:
        case TokenKind::OpDoubleAmpersand:
        case TokenKind::OpExclamation:
        case TokenKind::OpPlus:
        case TokenKind::OpMinus:
        case TokenKind::OpAsterisk:
        case TokenKind::OpSlash:
        case TokenKind::OpPercent:
        case TokenKind::OpComma:
        case TokenKind::OpDot:
        case TokenKind::OpArrow:
        case TokenKind::OpPlusPlus:
        case TokenKind::OpMinusMinus:
        case TokenKind::OpCaret:
        case TokenKind::OpPipe:
        case TokenKind::OpAmpersand:
        case TokenKind::OpTilde:
        case TokenKind::OpShiftLeft:
        case TokenKind::OpShiftRight:
        case TokenKind::OpDoubleEqual:
        case TokenKind::OpLessThanEqual:
        case TokenKind::OpGreaterThanEqual:
        case TokenKind::OpExclamationEqual:
        case TokenKind::OpPlusEqual:
        case TokenKind::OpMinusEqual:
        case TokenKind::OpAsteriskEqual:
        case TokenKind::OpSlashEqual:
        case TokenKind::OpPercentEqual:
        case TokenKind::OpCaretEqual:
        case TokenKind::OpPipeEqual:
        case TokenKind::OpAmpersandEqual:
        case TokenKind::OpShiftLeftEqual:
        case TokenKind::OpShiftRightEqual:
        case TokenKind::LeftParenthesis:
        case TokenKind::RightParenthesis:
        case TokenKind::LeftSquareBracket:
        case TokenKind::RightSquareBracket:
        case TokenKind::LeftCurlyBrace:
        case TokenKind::RightCurlyBrace:
        case TokenKind::Semicolon:
        case TokenKind::PpEndif:
            return std::string{ToString(t.kind)};
    }
    Diag::ICE("unreachable");
}

Result<void> Lexer::preprocessor_define(std::string_view name, std::vector<Token> contents) {
    if (_simple_defines.contains(name))
        return Error("c/preprocessor", "Redefinition of `{}`", name);
    _simple_defines.emplace(name, contents);
    return {};
}
void Lexer::preprocessor_undefine(std::string_view name) {
    // FIXME: Remove this shit once libc++ actually supports any semblance of
    // the modern language.
#ifdef __cpp_lib_associative_heterogeneous_erasure
    _simple_defines.erase(name);
#else
    _simple_defines.erase(std::string{name});
#endif
}

StringMap<TokenKind> keywords{
    {"int", TokenKind::KwInt},
    {"return", TokenKind::KwReturn},
    {"void", TokenKind::KwVoid},
    {"char", TokenKind::KwChar},
    {"bool", TokenKind::KwBool},
    {"sizeof", TokenKind::KwSizeof},
    {"_Alignof", TokenKind::KwAlignof},
    {"long", TokenKind::KwLong},
    {"short", TokenKind::KwShort},
    {"const", TokenKind::KwConst},
    {"volatile", TokenKind::KwVolatile},
    {"restrict", TokenKind::KwRestrict},
    {"_Atomic", TokenKind::KwAtomic},
    {"constexpr", TokenKind::KwConstexpr},
    {"auto", TokenKind::KwAuto},
    {"extern", TokenKind::KwExtern},
    {"register", TokenKind::KwRegister},
    {"static", TokenKind::KwStatic},
};

bool IsSpace(uint32_t c) {
    return c == ' ' or c == '\n'
        or c == '\r' or c == '\t';
}
bool IsIdentifierStartCharacter(uint32_t c) {
    return (c >= 'a' and c <= 'z')
        or (c >= 'A' and c <= 'Z')
        or c == '_';
}
bool IsIdentifierContinueCharacter(uint32_t c) {
    return IsIdentifierStartCharacter(c)
        or (c >= '0' and c <= '9');
}

void Lexer::NextNumber() {
    auto start_location = tok.location;

    constexpr auto DigitSeparator = '\'';

    LCC_ASSERT(
        lastc != DigitSeparator,
        "A number must not begin with the digit separator ({})",
        DigitSeparator
    );

    // Helper that actually parses the number.
    const auto ParseNumber = [&](std::string_view name, auto&& IsValidDigit, int base) {
        // Yeet prefix.
        if (base != 10) NextChar();

        // Lex digits (and maybe digit separators).
        while (IsValidDigit(lastc) or lastc == DigitSeparator) {
            if (lastc != DigitSeparator) {
                if (lastc > 0xff) LCC_TODO("Handle unicode codepoint in number literal");
                tok.text += char(lastc);
            }
            NextChar();
        }

        // We need at least one digit.
        if (tok.text.empty()) {
            Error(
                "c/invalid-literal",
                "Expected at least one {} digit",
                name
            );
        }

        tok.location.len = (u16) Location::length_from_two_offsets_exclusive(
            start_location.pos,
            (u32) _including_offset
        );
        // _including_offset can only ever be equal to the end, but the length of
        // the final token in a file will be one longer.
        if (lastc == 0)
            tok.location.len += 1;

        // Actually parse the number.
        const char* cstr = tok.text.c_str();

        // Convert the number.
        char* end;
        errno = 0;
        tok.integer_value = (u64) std::strtoull(cstr, &end, base);
        if (errno == ERANGE) {
            Error(
                "c/invalid-literal",
                "Bit width of integer is too large."
            );
        }
        if (end != cstr + tok.text.size()) {
            Error(
                "c/invalid-literal",
                "Invalid integer literal"
            );
        }
    };

    const auto ParseDecimalFraction = [&]() {
        LCC_ASSERT(
            lastc == '.',
            "ParseDecimalFraction called while not at `.`"
        );

        // Yeet "."
        NextChar();

        uint leading_zeroes{};
        while (lastc == '0') {
            ++leading_zeroes;
            NextChar();
        }

        // ".00000..."
        if (not IsDecimalDigit(lastc))
            return DecimalFraction{0, 0};

        ParseNumber("decimal_fraction", IsDecimalDigit, 10);

        return DecimalFraction{
            tok.integer_value,
            leading_zeroes
        };
    };

    // Record the start of the number.
    tok.text.clear();

    tok.integer_value = 0;
    tok.kind = TokenKind::Integer;

    // At least one leading zero.
    if (lastc == '0') {
        // Discard the zero.
        NextChar();

        // Another digit is an octal: no leading zeroes are allowed unless they
        // lead to a number base specifier.
        if (lastc == 'b' or lastc == 'B')
            ParseNumber("binary", IsBinaryDigit, 2);
        else if (IsDecimalDigit(lastc) or lastc == 'o' or lastc == 'O')
            ParseNumber("octal", IsOctalDigit, 8);
        else if (lastc == 'x' or lastc == 'X')
            ParseNumber("hexadecimal", IsHexDigit, 16);
        else if (lastc == '.') {
            auto decimal_fraction = ParseDecimalFraction();

            tok.kind = TokenKind::Fractional;
            tok.fractional_value = FixedPointNumber(0, decimal_fraction);
            return;
        }

        if (tok.kind == TokenKind::Integer) {
            // TODO: We should probably care about this a little more.
            while (lastc == 'u' or lastc == 'U' or lastc == 'l' or lastc == 'L')
                NextChar();
        }

        // If the next character is whitespace (or a delimiter), then this is just a zero.
        if (
            IsSpace(lastc)
            or lastc == ',' or lastc == ';'
            or lastc == '(' or lastc == ')'
            or lastc == '[' or lastc == ']'
            or lastc == '{' or lastc == '}'
            or lastc == '<' or lastc == '>'
            or lastc == '='
        ) return;

        // Anything else is an error.
        Error(
            "c/invalid-literal",
            "Invalid character in integer literal: U+{:04x} {}",
            lastc,
            (char) lastc
        );
    }

    // Any other digit means we have a decimal number.
    ParseNumber("decimal", IsDecimalDigit, 10);

    // TODO: We should probably care about this a little more.
    while (lastc == 'u' or lastc == 'U' or lastc == 'l' or lastc == 'L')
        NextChar();

    if (lastc == '.') {
        auto whole = tok.integer_value;
        auto decimal_fraction = ParseDecimalFraction();

        tok.kind = TokenKind::Fractional;
        tok.fractional_value = FixedPointNumber(
            whole,
            decimal_fraction
        );
    }
}

void Lexer::NextIdentifier() {
    tok.kind = TokenKind::Identifier;
    tok.text.clear();

    tok.location.pos = (u32) _including_offset;
    // _including_offset gets next character, and we are already at one.
    if (tok.location.pos) tok.location.pos -= 1;
    tok.location.len = 0;

    // Note: Istg if anyone gets the genius idea of extracting a substring
    // instead of appending character by character, DON’T. There is a REASON
    // why NextChar() exists. Character != byte in the source file.
    auto start_position = _including_offset;
    do {
        if (lastc > 0xff)
            Diag::ICE("Handle unicode codepoint in identifier");
        tok.text += char(lastc);
        NextChar();
    } while (IsIdentifierContinueCharacter(lastc));

    tok.location.len = (u16) Location::length_from_two_offsets_exclusive(
        (u32) start_position,
        (u32) _including_offset
    );
}

// Sometimes a single codepoint turns into a token kind with no token data
// required other than the kind.
// This is a quick-and-easy way to implement those sort of tokens.
std::unordered_map<uint32_t, TokenKind> easy_tokens{
    {0, TokenKind::Eof},
    {'=', TokenKind::OpEqual},
    {'<', TokenKind::OpLessThan},
    {'>', TokenKind::OpGreaterThan},
    {'!', TokenKind::OpExclamation},
    {'+', TokenKind::OpPlus},
    {'-', TokenKind::OpMinus},
    {'*', TokenKind::OpAsterisk},
    {'/', TokenKind::OpSlash},
    {'%', TokenKind::OpPercent},
    {',', TokenKind::OpComma},
    {'.', TokenKind::OpDot},
    {'^', TokenKind::OpCaret},
    {'|', TokenKind::OpPipe},
    {'&', TokenKind::OpAmpersand},
    {'~', TokenKind::OpTilde},
    {';', TokenKind::Semicolon},
    {'(', TokenKind::LeftParenthesis},
    {')', TokenKind::RightParenthesis},
    {'[', TokenKind::LeftSquareBracket},
    {']', TokenKind::RightSquareBracket},
    {'{', TokenKind::LeftCurlyBrace},
    {'}', TokenKind::RightCurlyBrace},
};
std::unordered_map<TokenKind, TokenKind> from_trailing_equal{
    {TokenKind::OpEqual, TokenKind::OpDoubleEqual},
    {TokenKind::OpLessThan, TokenKind::OpLessThanEqual},
    {TokenKind::OpGreaterThan, TokenKind::OpGreaterThanEqual},
    {TokenKind::OpExclamation, TokenKind::OpExclamationEqual},
    {TokenKind::OpPlus, TokenKind::OpPlusEqual},
    {TokenKind::OpMinus, TokenKind::OpMinusEqual},
    {TokenKind::OpAsterisk, TokenKind::OpAsteriskEqual},
    {TokenKind::OpSlash, TokenKind::OpSlashEqual},
    {TokenKind::OpPercent, TokenKind::OpPercentEqual},
    {TokenKind::OpCaret, TokenKind::OpCaretEqual},
    {TokenKind::OpPipe, TokenKind::OpPipeEqual},
    {TokenKind::OpAmpersand, TokenKind::OpAmpersandEqual},
    {TokenKind::OpShiftLeft, TokenKind::OpShiftLeftEqual},
    {TokenKind::OpShiftRight, TokenKind::OpShiftRightEqual},

};
std::unordered_map<TokenKind, TokenKind> has_trailing{
    {TokenKind::LeftParenthesis, TokenKind::RightParenthesis},
    {TokenKind::LeftSquareBracket, TokenKind::RightSquareBracket},
    {TokenKind::LeftCurlyBrace, TokenKind::RightCurlyBrace}
};

void Lexer::NextChar() {
    if (_including.empty()) {
        syntax::Lexer<Token>::NextChar();
        _including_offset = CurrentOffset() + 1;
        return;
    }
    auto& in = _including.front().file;
    // NOTE: Check necessary for completely empty files.
    if (_including_offset < in.size())
        lastc = u32(in.data()[_including_offset++]);
    if (_including_offset >= in.size()) {
        _including.pop_front();
        _including_offset = _including.empty()
                              ? CurrentOffset()
                              : _including.front().offset;
        tok.location = {
            (u32) _including_offset,
            1,
            static_cast<u16>(
                _including.empty()
                    ? _file_id
                    : _including.front().file.file_id()
            )
        };
    }
}

void Lexer::skip_past_expected_endif(Location connected_directive) {
    // Don't process #define/#undef, or process anything at all for that
    // matter.
    skipping = true;
    preprocessing = true;

    // Skip everything until #endif
    while (tok.kind != TokenKind::Eof and tok.kind != TokenKind::PpEndif) {
        while (lastc == '\n')
            NextChar();
        NextToken();
    }

    skipping = false;
    if (tok.kind != TokenKind::PpEndif)
        Error(connected_directive, "c/preprocessor", "Expected #endif; got {}", tok.kind);
    else NextToken();
    --_expected_endifs;
}

void Lexer::NextToken() {
    // If the preprocessor asks for a token, don't replace it.
    if (_next_tokens.size()) {
        tok = _next_tokens.front();
        _next_tokens.pop_front();
        return;
    }

    // Return EOF if we’re at EOF.
    if (not lastc) {
        tok.kind = TokenKind::Eof;
        return;
    }

    // Reset token (prevents confusing roll-over behavior, but technically not
    // necessary).
    tok.kind = TokenKind::Invalid;
    tok.artificial = false;

    // Skip different kinds of whitespace depending on if we are preprocessing
    // or not.
    if (preprocessing) {
        while (preprocessor_whitespace.contains((char) lastc)) {
            NextChar();
            if (lastc == '\\') {
                auto escape_location = Location{
                    (u32) _including_offset,
                    1,
                    tok.location.file_id
                };
                NextChar();
                if (lastc != '\n') {
                    Error(
                        escape_location,
                        "c/preprocessor",
                        "Invalid escaped character: `{}` (0x{:04x})\n",
                        (char) lastc,
                        lastc
                    );
                    tok.kind = TokenKind::Eof;
                    return;
                }
                // Preprocessor Escaped Newline
                NextChar();
            }
        }
    } else
        while (IsSpace(lastc)) NextChar();

    // Record start of token.
    tok.location.pos = (u32) _including_offset - 1;
    auto start_location = tok.location;

    // Determine token starting at current offset.
    switch (lastc) {
        case 0:
        case ',':
        case ';':
        case '(':
        case ')':
        case '[':
        case ']':
        case '{':
        case '}':
        case '.': {
            tok.kind = easy_tokens.at(lastc);
            NextChar();
        } break;

        case '+':
        case '-':
        case '*':
        case '/':
        case '%':
        case '&':
        case '^':
        case '|':
        case '<':
        case '>':
        case '!':
        case '~':
        case '=': {
            tok.kind = easy_tokens.at(lastc);
            NextChar();

            if (tok.kind == TokenKind::OpLessThan and lastc == '<') {
                tok.kind = TokenKind::OpShiftLeft;
                NextChar();
            } else if (tok.kind == TokenKind::OpGreaterThan and lastc == '>') {
                tok.kind = TokenKind::OpShiftRight;
                NextChar();
            } else if (tok.kind == TokenKind::OpPipe and lastc == '|') {
                tok.kind = TokenKind::OpDoublePipe;
                NextChar();
            } else if (tok.kind == TokenKind::OpAmpersand and lastc == '&') {
                tok.kind = TokenKind::OpDoubleAmpersand;
                NextChar();
            }

            else if (tok.kind == TokenKind::OpSlash) {
                // Line comment
                if (lastc == '/') {
                    NextChar();

                    while (lastc and lastc != '\n') NextChar();

                    // The actual token beyond the comment.
                    NextToken();
                    return;
                }
                // Block comment
                if (lastc == '*') {
                    NextChar();

                    while (lastc) {
                        NextChar();
                        if (lastc == '*') {
                            NextChar();
                            if (lastc == '/') {
                                NextChar();
                                break;
                            }
                        }
                    }

                    // The actual token beyond the comment.
                    NextToken();
                    return;
                }
            }

            if (lastc == '=' and from_trailing_equal.contains(tok.kind)) {
                tok.kind = from_trailing_equal.at(tok.kind);
                NextChar();
            }
        } break;

        case '#': {
            NextChar();

            // Skip non-newline whitespace...
            // #   define
            while (preprocessor_whitespace.contains((char) lastc))
                NextChar();

            if (lastc == '#')
                Diag::ICE("TODO: pp Concatenation");

            else if (IsIdentifierStartCharacter(lastc)) {
                NextIdentifier();

                bool was_preprocessing = preprocessing;
                preprocessing = true;

                if (skipping and tok.text != "endif") {
                    while (not (tok.kind == TokenKind::Eof or tok.kind == TokenKind::Invalid))
                        NextToken();

                    NextToken();
                    return;
                }

                if (tok.text == "define") {
                    NextToken();
                    if (tok.kind != TokenKind::Identifier) {
                        Error("c/preprocessor", "Macro name missing");
                        tok.kind = TokenKind::Eof;
                        return;
                    }

                    std::string name = tok.text;
                    NextToken();

                    std::vector<Token> contents{};
                    while (not (tok.kind == TokenKind::Eof or tok.kind == TokenKind::Invalid)) {
                        contents.emplace_back(tok);
                        NextToken();
                    }

                    (void) preprocessor_define(name, contents);
                } else if (tok.text == "undef") {
                    NextToken();
                    if (tok.kind != TokenKind::Identifier) {
                        Error("c/preprocessor", "Macro name missing!");
                        tok.kind = TokenKind::Eof;
                        return;
                    }

                    std::string name = tok.text;

                    NextToken();
                    while (not (tok.kind == TokenKind::Eof or tok.kind == TokenKind::Invalid)) {
                        Warning("c/preprocessor", "Junk following macro name of #undef directive");
                        NextToken();
                    }

                    preprocessor_undefine(name);
                } else if (tok.text == "if") {
                    ++_expected_endifs;
                    auto if_location = tok.location;
                    NextToken();

                    if (tok.kind == TokenKind::Invalid or tok.kind == TokenKind::Eof) {
                        Error("c/preprocessor", "#if directive requires a condition");
                        tok.kind = TokenKind::Eof;
                        return;
                    }

                    int value{false};
                    bool flip_next{false};

                    while (tok.kind != TokenKind::Invalid and tok.kind != TokenKind::Eof) {
                        if (tok.kind == TokenKind::Identifier) {
                            if (tok.text == "defined") {
                                NextToken();

                                if (tok.kind != TokenKind::LeftParenthesis) {
                                    Error("c/preprocessor", "Expected `(` following `defined`");
                                    tok.kind = TokenKind::Eof;
                                    return;
                                }
                                NextToken();

                                if (tok.kind != TokenKind::Identifier) {
                                    Error("c/preprocessor", "Expected macro name following `defined(`");
                                    tok.kind = TokenKind::Eof;
                                    return;
                                }
                                std::string name = tok.text;
                                NextToken();

                                if (tok.kind != TokenKind::RightParenthesis) {
                                    Error("c/preprocessor", "Expected `)` following macro name of `defined()`");
                                    tok.kind = TokenKind::Eof;
                                    return;
                                }

                                tok.kind = TokenKind::Integer;
                                tok.integer_value = _simple_defines.contains(name)
                                                      ? 1
                                                      : 0;
                                continue;
                            } else {
                                // TODO: Expand known macros to their definitions, expand unknown macros
                                // to zero.
                                if (_simple_defines.contains(tok.text)) {
                                    Diag::ICE("TODO: Expand known macro {} within #if", tok.text);
                                } else {
                                    tok.kind = TokenKind::Integer;
                                    tok.integer_value = 0;
                                    continue;
                                }
                            }
                        } else if (tok.kind == TokenKind::OpDoubleAmpersand) {
                            // Short Circuit Evaluation
                            if (not value) break;
                        } else if (tok.kind == TokenKind::OpDoublePipe) {
                            // Short Circuit Evaluation
                            if (value) break;
                        } else if (tok.kind == TokenKind::Integer) {
                            value = (int) tok.integer_value;
                        } else if (tok.kind == TokenKind::OpExclamation) {
                            flip_next = true;
                        } else {
                            { Note("c/unhandled", "Here"); }
                            Diag::ICE("Unhandled token in #if directive {}", tok.kind);
                        }
                        if (flip_next) {
                            value = ! value;
                            flip_next = false;
                        }
                        NextToken();
                    }

                    // Skip to end of line...
                    while (tok.kind != TokenKind::Invalid and tok.kind != TokenKind::Eof)
                        NextToken();

                    if (not value)
                        skip_past_expected_endif(if_location);

                } else if (tok.text == "ifdef" or tok.text == "ifndef") {
                    ++_expected_endifs;

                    auto if_location = tok.location;

                    bool inverted{tok.text == "ifndef"};
                    NextToken();
                    if (tok.kind != TokenKind::Identifier) {
                        Error(if_location, "c/preprocessor", "Macro name missing!");
                        tok.kind = TokenKind::Eof;
                        return;
                    }

                    std::string name = tok.text;

                    NextToken();
                    while (not (tok.kind == TokenKind::Eof or tok.kind == TokenKind::Invalid)) {
                        Warning(
                            if_location,
                            "c/preprocessor",
                            "Junk following macro name of #ifdef directive"
                        );
                        NextToken();
                    }

                    // If the check does not pass...
                    if (inverted == _simple_defines.contains(name))
                        skip_past_expected_endif(if_location);
                    // Otherwise, go on to parse everything like normal.
                } else if (tok.text == "endif") {
                    auto endif_location = tok.location;

                    if (not _expected_endifs) {
                        Error(
                            "c/preprocessor",
                            "Got #endif outside of any #if, #ifdef, or #ifndef"
                        );
                        tok.kind = TokenKind::Eof;
                        return;
                    }

                    NextToken();
                    // Expect newline
                    while (tok.kind != TokenKind::Eof and tok.kind != TokenKind::Invalid) {
                        Warning("c/preprocessor", "Junk following #endif directive");
                        NextToken();
                    }

                    if (tok.kind != TokenKind::Invalid) {
                        Warning(
                            endif_location,
                            "c/preprocessor",
                            "Expected newline directly following #endif directive"
                        );
                    }

                    // We return preprocessor tokens when we are preprocessing!
                    if (was_preprocessing) {
                        tok.kind = TokenKind::PpEndif;
                        return;
                    }
                } else if (tok.text == "include") {
                    NextToken();
                    std::string path{};
                    if (tok.kind == TokenKind::OpLessThan) {
                        auto open_location = tok.location;
                        while (lastc and lastc != '\n' and lastc != '>') {
                            path += char(lastc);
                            NextChar();
                        }
                        if (lastc == '>') {
                            // Yeet '>'
                            NextToken();
                            /** (!): Above lexes '>', below yeets it */
                            NextToken();
                        } else {
                            auto e = Error(
                                open_location,
                                "c/preprocessor",
                                "Expected `>` to close this `<`..."
                            );
                            e.fix_by_inserting_at(tok.location, ">");
                        }
                    } else if (tok.kind == TokenKind::String) {
                        path = tok.text;
                        NextToken();
                    } else {
                        Error(
                            "c/preprocessor",
                            "Expected `<` or `\"` to begin included path, but got {} instead",
                            tok.kind
                        );
                        // TODO: Synchronize to beginning of next line, or something.
                        tok.kind = TokenKind::Eof;
                        return;
                    }

                    fs::path fullpath{};
                    if (fs::exists(path))
                        fullpath = fs::absolute(path);
                    else {
                        for (auto dir : context->include_directories()) {
                            auto dirpath = fs::path(dir);
                            if (fs::exists(dirpath / path)) {
                                fullpath = fs::absolute(dirpath / path);
                                break;
                            }
                        }
                    }
                    if (fullpath.empty()) {
                        Error(
                            "c/preprocessor",
                            "Included file \"{}\" does not exist\nChecked:\n  {}",
                            path,
                            fmt::join(
                                lcc::rgs::transform_view(
                                    context->include_directories(),
                                    [&](const auto& d) {
                                        return fs::path(d) / path;
                                    }
                                ),
                                "\n  "
                            )
                        );
                        tok.kind = TokenKind::Eof;
                        return;
                    }

                    while (not (
                        tok.kind == TokenKind::Eof
                        or tok.kind == TokenKind::Invalid
                    )) {
                        Warning(
                            "c/preprocessor",
                            "Junk following path of #include directive `{}`",
                            tok.kind
                        );
                        NextToken();
                    }

                    // After this, the next characters we fetch via the lexer API will be from
                    // the included file. This means we can't do our normal handling of "go
                    // until EOF or newline", since, er, this file's tokens are in the way.
                    if (not _including.empty())
                        _including.front().offset = _including_offset;
                    _including.push_front(
                        {context->create_file(fullpath, File::Read(fullpath)),
                         0}
                    );
                    _including_offset = 0;
                    tok.location = {
                        0,
                        1,
                        (u16) _including.front().file.file_id()
                    };
                } else {
                    Error(
                        "c/preprocessor",
                        "Unrecognized preprocessor statement `{}`.\nIf you believe this should be a conditionally supported directive, let the maintainers know.",
                        tok.text
                    );
                    tok.kind = TokenKind::Eof;
                    return;
                }

                preprocessing = false;

                // This fetches the *actual* token, not preprocessor stuff.
                NextToken();

                break;
            } else Diag::ICE("TODO: pp Stringization");

            Diag::ICE("unreachable");
        }

        case '"': {
            auto operator_location = tok.location;
            tok.kind = TokenKind::String;
            NextChar();
            tok.text.clear();
            while (lastc and lastc != '"' and lastc != '\n') {
                if (lastc > 0xff) [[unlikely]] {
                    Diag::Error(
                        "Truncating non-ASCII character in string literal... {} -> {}",
                        lastc,
                        (int) ((char) lastc)
                    );
                }
                // Handle Escapes
                if (lastc == '\\') {
                    NextChar();
                    switch (lastc) {
#define PASSTHROUGH_ESCAPE(c, e) \
    case c:                      \
        tok.text += e;           \
        continue; /** (!) **/
                        PASSTHROUGH_ESCAPE('r', '\r');
                        PASSTHROUGH_ESCAPE('n', '\n');
                        PASSTHROUGH_ESCAPE('t', '\t');
                        PASSTHROUGH_ESCAPE('e', 0x1b);
                        PASSTHROUGH_ESCAPE('f', '\f');
                        PASSTHROUGH_ESCAPE('b', '\b');
                        PASSTHROUGH_ESCAPE('a', '\a');
                        PASSTHROUGH_ESCAPE('v', '\v');
#undef PASSTHROUGH_ESCAPE

                        case '\n':
                            NextChar();
                            continue;

                        default:
                            tok.text += '\\';
                            if (lastc <= 0xff) {
                                Diag::Warning(
                                    "c/unrecognized-escape-sequence",
                                    "Unrecognized escape sequence `\\{}`",
                                    (char) lastc
                                );
                            } else Diag::Warning(
                                "c/unrecognized-escape-sequence",
                                "Unrecognized escaped character 0x{:x}",
                                lastc
                            );
                            break;
                    }
                }
                tok.text += (char) lastc;
                NextChar();
            }
            if (lastc != '"') {
                auto e = Error(
                    "c/invalid-literal",
                    "Expected closing `\"`"
                );
                e.attach(Note(
                    operator_location,
                    "c/invalid-literal",
                    "To match this `\"`"
                ));
            }
            NextChar(); // Eat closing `"`
        } break;

        // The only way to get here is if the preprocessor calls NextToken but
        // there isn't another one in the line, and the newline isn't escaped.
        case '\n':
            tok.kind = TokenKind::Invalid;
            break;

        default: {
            if (IsDecimalDigit(lastc)) {
                NextNumber();
                /// The character after a number must be a whitespace or delimiter.
                if (IsAlpha(lastc)) {
                    Error(
                        "c/invalid-literal",
                        "Invalid integer literal---the character after a number must not be an alpha character (got `{}` (0x{:04x}))",
                        (char) lastc,
                        lastc
                    );
                }
                break;
            }

            if (IsIdentifierStartCharacter(lastc)) {
                NextIdentifier();

                // Detect simple macros.
                if (not preprocessing and _simple_defines.contains(tok.text)) {
#ifdef __cpp_lib_containers_ranges
                    _next_tokens.append_range(
                        _simple_defines.at(tok.text)
                    );
#else
                    const auto& replacement_tokens = _simple_defines.at(tok.text);
                    _next_tokens.insert(
                        _next_tokens.end(),
                        replacement_tokens.cbegin(),
                        replacement_tokens.cend()

                    );
#endif
                    NextToken();
                }

                // Detect keywords.
                else if (keywords.contains(tok.text))
                    tok.kind = keywords.at(tok.text);

                break;
            }

            tok.location.len = (u16) Location::length_from_two_offsets_exclusive(
                start_location.pos,
                (u32) _including_offset
            );
            Error("c/expected", "Unknown: `{}` (0x{:04x})", (char) lastc, lastc);
            NextChar();
        } break;
    }

    tok.location.len = (u16) Location::length_from_two_offsets_exclusive(
        start_location.pos,
        _including_offset ? (u32) _including_offset - 1 : 0
    );
    if (lastc == 0)
        tok.location.len += 1;
}

// @return zero for non operators, otherwise the precedence value.
constexpr size_t unary_precedence(TokenKind kind) {
    switch (kind) {
        case TokenKind::OpPlus:
        case TokenKind::OpMinus:
        case TokenKind::OpPlusPlus:
        case TokenKind::OpMinusMinus:
        case TokenKind::OpExclamation:
        case TokenKind::OpAsterisk:
        case TokenKind::OpAmpersand:
        case TokenKind::OpTilde:
        case TokenKind::KwSizeof:
        case TokenKind::KwAlignof:
            return 2;

            NOT_A_UNARY_OPERATOR
            return 0;
    }
    Diag::ICE("unreachable");
}

// @return zero for non operators, otherwise the precedence value.
constexpr size_t binary_precedence(TokenKind kind) {
    switch (kind) {
        case TokenKind::OpComma:
            return 15;

        // Assignment
        case TokenKind::OpEqual:
        case TokenKind::OpPlusEqual:
        case TokenKind::OpMinusEqual:
        case TokenKind::OpAsteriskEqual:
        case TokenKind::OpSlashEqual:
        case TokenKind::OpPercentEqual:
        case TokenKind::OpCaretEqual:
        case TokenKind::OpPipeEqual:
        case TokenKind::OpAmpersandEqual:
        case TokenKind::OpShiftLeftEqual:
        case TokenKind::OpShiftRightEqual:
            return 14;

        case TokenKind::OpDoublePipe:
            return 12;

        case TokenKind::OpDoubleAmpersand:
            return 11;

        case TokenKind::OpPipe:
            return 10;

        case TokenKind::OpCaret:
            return 9;

        case TokenKind::OpAmpersand:
            return 8;

        case TokenKind::OpDoubleEqual:
        case TokenKind::OpExclamationEqual:
            return 7;

        case TokenKind::OpLessThan:
        case TokenKind::OpGreaterThan:
        case TokenKind::OpLessThanEqual:
        case TokenKind::OpGreaterThanEqual:
            return 6;

        case TokenKind::OpShiftLeft:
        case TokenKind::OpShiftRight:
            return 5;

        case TokenKind::OpPlus:
        case TokenKind::OpMinus:
            return 4;

        case TokenKind::OpAsterisk:
        case TokenKind::OpSlash:
        case TokenKind::OpPercent:
            return 3;

        case TokenKind::LeftSquareBracket:
        case TokenKind::OpDot:
        case TokenKind::OpArrow:
        case TokenKind::LeftParenthesis:
            return 1;

            NOT_A_BINARY_OPERATOR
            return 0;
    }
    Diag::ICE("unreachable");
}
constexpr size_t reset_precedence{0};

auto try_declarations_to_parameters(
    Context* context,
    std::vector<FunctionType::Parameter>& parameters,
    const Node* parameter
) -> Result<void> {
    if (parameter->kind() == NodeKind::Group) {
        for (auto c : ((Group*) parameter)->constituents()) {
            auto result = try_declarations_to_parameters(context, parameters, c);
            if (not result) return result.diag();
        }
    } else if (parameter->kind() == NodeKind::Declaration) {
        auto* d = (Declaration*) parameter;
        parameters.emplace_back(
            std::string(d->name()),
            d->type()
        );
    } else {
        return Diag::Error(
            context,
            parameter->location(),
            "c/expected",
            "Expected parameter to be a declaration, but got {}",
            parameter->name()
        );
    }
    return {};
};

auto Parser::ParseDeclarators(Type* type_specifier)
    -> Result<std::vector<Node*>> {
    std::vector<Node*> parsed_declarations{};
    // We have just parsed a type specifier (like "int").
    // We are now at the beginning of the list of declarators.
    // Like "foo", "*x", "(foo)", "main()", or "bar[5]".

    if (not type_specifier) Diag::ICE("nullptr argument");

    bool definition_possible{true};

    do {
        // Skip separating commas, but not one that shows up right after the type.
        if (not definition_possible and tok.kind == TokenKind::OpComma)
            NextToken();

        // Should be one of left parenthesis, star, or an identifier.
        Type* current_declarator_type{type_specifier};
        std::string current_declarator_name{};
        while (
            tok.kind != TokenKind::Invalid and tok.kind != TokenKind::Count
            and tok.kind != TokenKind::Eof
            and tok.kind != TokenKind::OpComma and tok.kind != TokenKind::Semicolon
            and tok.kind != TokenKind::LeftParenthesis and tok.kind != TokenKind::LeftSquareBracket
            and tok.kind != TokenKind::OpEqual
        ) {
            switch (tok.kind) {
                default:
                    return Error("c/expected", "Invalid start of declarator");

                case TokenKind::Identifier:
                    if (not current_declarator_name.empty()) {
                        return Error(
                            "c/expected",
                            "Multiple identifiers encountered within a single declarator"
                        );
                    }

                    current_declarator_name = tok.text;
                    NextToken();
                    break;

                case TokenKind::OpAsterisk:
                    auto location = Location{current_declarator_type->location(), tok.location};
                    NextToken();
                    current_declarator_type = new (tu) PointerType(current_declarator_type, location);
                    // const pointer syntax
                    if (tok.kind == TokenKind::KwConst) {
                        current_declarator_type->flag(Type::Flag::Const, true);
                        NextToken();
                    }
                    if (tok.kind == TokenKind::KwVolatile) {
                        current_declarator_type->flag(Type::Flag::Volatile, true);
                        NextToken();
                    }
                    if (tok.kind == TokenKind::KwAtomic) {
                        current_declarator_type->flag(Type::Flag::Atomic, true);
                        NextToken();
                    }
                    if (tok.kind == TokenKind::KwRestrict) {
                        current_declarator_type->flag(Type::Flag::Restrict, true);
                        NextToken();
                    }
                    break;
            }
        }

        if (current_declarator_name.empty()) {
            Warning("c/missing-declarations", "Declaration does not declare anything (no name)");
        }

        // Trailing declarator type specifiers (functions, arrays)
        switch (tok.kind) {
            default: break;

            case TokenKind::LeftParenthesis: {
                NextToken();
                std::vector<FunctionType::Parameter> parameters{};

                while (
                    tok.kind != TokenKind::RightParenthesis
                    and tok.kind != TokenKind::Eof
                    and tok.kind != TokenKind::Invalid
                ) {
                    auto parameter = ParseExpression(reset_precedence);
                    if (not parameter) return parameter.diag();
                    auto declared = try_declarations_to_parameters(
                        context,
                        parameters,
                        *parameter
                    );
                    if (not declared) return declared.diag();
                }
                if (tok.kind != TokenKind::RightParenthesis) {
                    return Error(
                        "c/expected",
                        "Expected right parenthesis to close function parameter list following `{}`",
                        current_declarator_name
                    );
                }
                auto location = Location{current_declarator_type->location(), tok.location};
                // Eat ")".
                NextToken();

                current_declarator_type = new (tu) FunctionType(
                    current_declarator_type,
                    parameters,
                    location
                );
            } break;

            case TokenKind::LeftSquareBracket: {
                auto opening_location = tok.location;
                NextToken();

                auto dimension = ParseExpression(
                    binary_precedence(TokenKind::LeftSquareBracket)
                );
                if (not dimension) return dimension.diag();

                if (tok.kind != TokenKind::RightSquareBracket) {
                    auto e = Error("c/expected", "Expected closing square bracket");
                    e.attach(
                        Note(
                            opening_location,
                            "c/expected",
                            "To close this opening square bracket"
                        ),
                        true
                    );
                    return e;
                }
                auto location = Location{current_declarator_type->location(), tok.location};
                NextToken();

                current_declarator_type = new (tu) ArrayType(
                    current_declarator_type,
                    *dimension,
                    location
                );
            }
        }

        Node* initialiser{nullptr};
        if (definition_possible and current_declarator_type->kind() == TypeKind::Function) {
            // Look for function definition
            if (tok.kind == TokenKind::LeftCurlyBrace) {
                /** (!) -- DO NOT EAT CURLY BRACE **/
                auto body = ParseExpression(reset_precedence);
                if (not body) return body.diag();
                initialiser = *body;
            }

        } else {
            if (tok.kind == TokenKind::OpEqual) {
                NextToken();
                auto maybe_initialiser = ParseExpression(reset_precedence);
                if (not maybe_initialiser) return maybe_initialiser.diag();
                initialiser = *maybe_initialiser;
            }
        }

        parsed_declarations.push_back(
            new (tu) Declaration(
                current_declarator_type,
                current_declarator_name,
                current_scope(),
                initialiser,
                Location{type_specifier->location(), tok.location}
            )
        );

        definition_possible = false;

        // Loop back around to parse another declarator if there is a comma...
    } while (tok.kind == TokenKind::OpComma);

    return parsed_declarations;
}

auto Parser::ParseDeclarations(Type* type_specifier) -> Result<Node*> {
    auto maybe_decls = ParseDeclarators(type_specifier);
    if (maybe_decls) {
        auto out = Node::MaybeToGroup(tu, *maybe_decls);
        // FIXME: Should we return an "empty" node?
        if (not out)
            Diag::ICE("No declarations parsed, but no error returned");
        return out;
    } else return maybe_decls.diag();
}

bool Parser::IsFunctionDefinition(Node* node) {
    if (node->kind() == NodeKind::Group)
        node = ((Group*) node)->constituents().back();
    if (node->kind() == NodeKind::Declaration) {
        auto d = (Declaration*) node;
        if (d->type()->kind() == TypeKind::Function and d->initialising_expression()) {
            return true;
        }
    }
    return false;
}

auto Parser::ParseExpressions(TokenKind until) -> Result<std::vector<Node*>> {
    std::vector<Node*> constituents{};
    while (
        tok.kind != TokenKind::Eof
        and tok.kind != TokenKind::Invalid
        and tok.kind != until
    ) {
        auto maybe_expression = ParseExpression(reset_precedence);
        if (not maybe_expression) return maybe_expression.diag();
        constituents.emplace_back(*maybe_expression);
        if (tok.kind == TokenKind::Semicolon)
            NextToken();
        else if (not IsFunctionDefinition(*maybe_expression)) {
            auto e = Error(
                "c/expected",
                "Expected `;` but got `{}`",
                ToString(tok.kind)
            );
            if (maybe_expression and maybe_expression->location().seekable(context)) {
                e.fix_by_inserting_at(maybe_expression->get_past_location(), ";");
                e.attach(
                    Note(
                        maybe_expression->location(),
                        "c/expected",
                        "After this"
                    )
                );
            } else e.fix_by_inserting_at(tok.location, ";");
            return e;
        }
    }
    return constituents;
}

auto Parser::ParseBaseType(Type::flag_t flags) -> Result<Type*> {
    Type* out{};

#define out_already                           \
    if (out) return Error(                    \
        "c/expected",                         \
        "Unexpected `{}` while parsing type", \
        ToString(tok.kind)                    \
    )

    do {
        switch (tok.kind) {
            case TokenKind::Identifier:
                if (_declared_types.contains(tok.text)) {
                    out_already;
                    out = _declared_types.at(tok.text);
                    NextToken();
                }
                break;

            case TokenKind::KwVoid: {
                out_already;
                out = new (tu) VoidType(tok.location);
                NextToken();
            }
                continue;

            case TokenKind::KwBool: {
                out_already;
                out = new (tu) BoolType(tok.location);
                NextToken();
            }
                continue;

            case TokenKind::KwChar: {
                out_already;
                out = new (tu) CharType(true, tok.location);
                NextToken();
            }
                continue;

            case TokenKind::KwShort: {
                out_already;
                out = new (tu) ShortType(true, tok.location);
                NextToken();
            }
                continue;

            case TokenKind::KwInt: {
                out_already;
                out = new (tu) IntType(true, tok.location);
                NextToken();
            }
                continue;

            case TokenKind::KwLong: {
                out_already;
                auto location = tok.location;
                NextToken();
                if (tok.kind == TokenKind::KwLong) {
                    out = new (tu) LongLongType(true, {location, tok.location});
                    NextToken();
                } else out = new (tu) LongType(true, location);
            }
                continue;

            case TokenKind::KwConst:
            case TokenKind::KwVolatile:
            case TokenKind::KwRestrict:
            case TokenKind::KwAtomic:
            case TokenKind::KwConstexpr:
            case TokenKind::KwExtern:
            case TokenKind::KwRegister:
            case TokenKind::KwStatic: {
#ifdef LCC_C_EXTENSIONS
                if (out) {
                    Warning(
                        "lcc/declaration-inner-ordering",
                        "Storage class specifier or type qualifier `{}` appears _after_ base type",
                        ToString(tok.kind)
                    );
                }
#endif
                flags |= flag_from_keyword(tok.kind);
                NextToken();
            }
                continue;

            case TokenKind::KwAuto:
                /** (!) -- No-op */
                NextToken();
                continue;

            case TokenKind::Invalid:
            case TokenKind::Integer:
            case TokenKind::Fractional:
            case TokenKind::String:
            case TokenKind::KwReturn:
            case TokenKind::KwSizeof:
            case TokenKind::KwAlignof:
            case TokenKind::OpEqual:
            case TokenKind::OpLessThan:
            case TokenKind::OpGreaterThan:
            case TokenKind::OpDoublePipe:
            case TokenKind::OpDoubleAmpersand:
            case TokenKind::OpExclamation:
            case TokenKind::OpPlus:
            case TokenKind::OpMinus:
            case TokenKind::OpAsterisk:
            case TokenKind::OpSlash:
            case TokenKind::OpPercent:
            case TokenKind::OpComma:
            case TokenKind::OpDot:
            case TokenKind::OpArrow:
            case TokenKind::OpPlusPlus:
            case TokenKind::OpMinusMinus:
            case TokenKind::OpCaret:
            case TokenKind::OpPipe:
            case TokenKind::OpAmpersand:
            case TokenKind::OpTilde:
            case TokenKind::OpShiftLeft:
            case TokenKind::OpShiftRight:
            case TokenKind::OpDoubleEqual:
            case TokenKind::OpLessThanEqual:
            case TokenKind::OpGreaterThanEqual:
            case TokenKind::OpExclamationEqual:
            case TokenKind::OpPlusEqual:
            case TokenKind::OpMinusEqual:
            case TokenKind::OpAsteriskEqual:
            case TokenKind::OpSlashEqual:
            case TokenKind::OpPercentEqual:
            case TokenKind::OpCaretEqual:
            case TokenKind::OpPipeEqual:
            case TokenKind::OpAmpersandEqual:
            case TokenKind::OpShiftLeftEqual:
            case TokenKind::OpShiftRightEqual:
            case TokenKind::LeftParenthesis:
            case TokenKind::RightParenthesis:
            case TokenKind::LeftSquareBracket:
            case TokenKind::RightSquareBracket:
            case TokenKind::LeftCurlyBrace:
            case TokenKind::RightCurlyBrace:
            case TokenKind::Semicolon:
            case TokenKind::Eof:
            case TokenKind::PpEndif:
            case TokenKind::Count:
                break;
        }

        break;

    } while (true);

    if (not out)
        return Error("c/expected", "Expected type, got {}", tok.kind);

    out->_flags = flags;

    return out;

#undef out_already
};

auto Parser::ParseExpression(size_t current_precedence) -> Result<Node*> {
    Location start_location = tok.location;
    UnaryOperation* unary{nullptr};
    Result<Node*> lhs = Result<Node*>::Null(); /** (!) **/

    // Prefix unary operators
    while (unary_precedence(tok.kind)) {
        unary = new (tu) UnaryOperation(tok.kind, unary, tok.location);
        NextToken();
    }

    switch (tok.kind) {
        case TokenKind::Invalid:
        case TokenKind::Count:
        case TokenKind::Eof:
        case TokenKind::PpEndif:
            Diag::ICE("Parser encountered unexpected token kind `{}`...", tok.kind);

        case TokenKind::OpSlash:
        case TokenKind::OpPercent:
        case TokenKind::OpComma:
        case TokenKind::RightParenthesis:
        case TokenKind::RightSquareBracket:
        case TokenKind::RightCurlyBrace:
        case TokenKind::OpEqual:
        case TokenKind::OpLessThan:
        case TokenKind::OpGreaterThan:
        case TokenKind::OpDoublePipe:
        case TokenKind::OpDoubleAmpersand:
        case TokenKind::OpDot:
        case TokenKind::OpArrow:
        case TokenKind::OpShiftLeft:
        case TokenKind::OpShiftRight:
        case TokenKind::OpDoubleEqual:
        case TokenKind::OpLessThanEqual:
        case TokenKind::OpGreaterThanEqual:
        case TokenKind::OpExclamationEqual:
        case TokenKind::OpPlusEqual:
        case TokenKind::OpMinusEqual:
        case TokenKind::OpAsteriskEqual:
        case TokenKind::OpSlashEqual:
        case TokenKind::OpPercentEqual:
        case TokenKind::OpCaretEqual:
        case TokenKind::OpPipeEqual:
        case TokenKind::OpAmpersandEqual:
        case TokenKind::OpShiftLeftEqual:
        case TokenKind::OpShiftRightEqual:
        case TokenKind::OpCaret:
        case TokenKind::OpPipe:
            return Error("expected-qualified-id", "Unexpected `{}`", tok.kind);

        case TokenKind::KwReturn: {
            NextToken();
            if (tok.kind != TokenKind::Semicolon and tok.kind != TokenKind::Eof) {
                auto expression = ParseExpression(reset_precedence);
                if (not expression) return expression.diag();
                lhs = new (tu) Return(
                    *expression,
                    {start_location, expression->location()}
                );
            } else lhs = new (tu) Return(nullptr, start_location);
        } break;

        case TokenKind::KwConst:
        case TokenKind::KwVolatile:
        case TokenKind::KwRestrict:
        case TokenKind::KwAtomic: // _Atomic
        case TokenKind::KwConstexpr:
        case TokenKind::KwExtern:
        case TokenKind::KwRegister:
        case TokenKind::KwStatic: {
            Type::flag_t flags = flag_from_keyword(tok.kind);
            NextToken();
            // Encountering type qualifier or storage class specifier implies a type
            // follows... Parsing a type implies a declaration follows.
            auto t = ParseBaseType(flags);
            if (not t) return t.diag();
            lhs = ParseDeclarations(*t);
        } break;

        case TokenKind::KwAuto:
        case TokenKind::KwVoid:
        case TokenKind::KwBool:
        case TokenKind::KwChar:
        case TokenKind::KwShort:
        case TokenKind::KwInt:
        case TokenKind::KwLong: {
            // Encountering just 'bool' (etc.) implies a declaration follows.
            auto t = ParseBaseType(0);
            if (not t) return t.diag();
            lhs = ParseDeclarations(*t);
        } break;

        case TokenKind::Semicolon: {
            NextToken();
            return Warning(start_location, "c/empty-statement", "Empty statement");
        };

        case TokenKind::LeftCurlyBrace: {
            // Eat "{"
            NextToken();

            auto constituents = ParseExpressions(TokenKind::RightCurlyBrace);
            if (not constituents) return constituents.diag();

            if (tok.kind != TokenKind::RightCurlyBrace) {
                auto e = Error(
                    "c/expected",
                    "Expected right curly brace to close block expression"
                );
                e.fix_by_inserting_at(tok.location, "}");
                return e;
            }

            auto location = Location{start_location, tok.location};
            // Eat "}"
            NextToken();

            lhs = new (tu) Block(*constituents, location);
        } break;

        case TokenKind::LeftParenthesis: {
            // Eat "("
            NextToken();

            if (tok.kind == TokenKind::RightParenthesis) {
                return Error(
                    "c/expected",
                    "Expected expression inside parenthesis..."
                );
            }

            lhs = ParseExpression(reset_precedence);

            if (tok.kind != TokenKind::RightParenthesis) {
                auto e = Error(
                    "c/expected",
                    "Expected right parenthesis to close parenthesized expression"
                );
                e.fix_by_inserting_at(tok.location, "}");
                return e;
            }

            // Eat ")"
            NextToken();
        } break;

        case TokenKind::Integer: {
            lhs = new (tu) IntegerLiteral(size_t(tok.integer_value), tok.location);
            NextToken();
        } break;

        case TokenKind::String: {
            // The idea is to intern the string, then reference the interned string
            // via it's name.
            auto interned_index = tu.intern(tok.text);
            auto string_literal_name = fmt::format("__str{}", interned_index);

            // Don't double-declare global (but do declare it)
            auto s = scopes().at(0); // TODO: global scope
            if (not s->declarations.contains(string_literal_name)) {
                std::vector<Node*> elements{};
                for (auto c : tok.text) {
                    elements.emplace_back(
                        new (tu) IntegerLiteral((usz) c, tok.location)
                    );
                }
                elements.emplace_back(new (tu) IntegerLiteral(0, tok.location));
                auto init = new (tu) ArrayLiteral(
                    std::move(elements),
                    tok.location
                );
                auto* char_type = new (tu) CharType(false, tok.location);
                char_type->flag(Type::Flag::Const, true);
                init->_element_type = char_type;

                // For side effect of declaring global (so sema lookup will resolve
                // properly), as well as telling IRGen to .
                if (not tu.tree or tu.tree->kind() != NodeKind::Block)
                    Diag::ICE("unexpected root");
                auto decl = new (tu) Declaration(
                    new (tu) PointerType(char_type, tok.location),
                    string_literal_name,
                    s,
                    init,
                    tok.location
                );
                // Place in top level manually...
                ((Block*) tu.tree)->_constituents.emplace_back(decl);
            }

            lhs = new (tu) NameReference(
                string_literal_name,
                s,
                tok.location
            );
            NextToken();
        } break;

        case TokenKind::Identifier: {
            lhs = new (tu) NameReference(
                tok.text,
                current_scope(),
                tok.location
            );
            NextToken();
        } break;

        case TokenKind::KwAlignof:
        case TokenKind::KwSizeof:
        case TokenKind::OpAmpersand:
        case TokenKind::OpAsterisk:
        case TokenKind::OpExclamation:
        case TokenKind::OpMinus:
        case TokenKind::OpMinusMinus:
        case TokenKind::OpPlus:
        case TokenKind::OpPlusPlus:
        case TokenKind::OpTilde: {
            Diag::ICE("Unary prefix operator {} handled above...", tok.kind);
        } break;

        case TokenKind::Fractional:
        case TokenKind::LeftSquareBracket:
            Diag::ICE("{} is unhandled...", tok.kind);
    }

    if (not lhs) return lhs.diag();

    if (unary) {
        unary->operand() = *lhs;
        lhs = unary;
    }

    if (tok.kind == TokenKind::OpComma)
        return lhs;

    // Once we've parsed an expression, we should check if that expression is
    // the lhs of a binary expression.
    // NOTE: non-zero precedence = an operator
    while (binary_precedence(tok.kind)) {
        if (
            current_precedence
            and binary_precedence(tok.kind) > current_precedence
        ) return lhs;

        switch (tok.kind) {
            case lcc::language_c::TokenKind::OpComma:
                Diag::ICE("unreachable");

            case TokenKind::OpEqual:
            case TokenKind::OpLessThan:
            case TokenKind::OpGreaterThan:
            case TokenKind::OpDoublePipe:
            case TokenKind::OpDoubleAmpersand:
            case TokenKind::OpPlus:
            case TokenKind::OpMinus:
            case TokenKind::OpAsterisk:
            case TokenKind::OpSlash:
            case TokenKind::OpPercent:
            case TokenKind::OpDot:
            case TokenKind::OpArrow:
            case TokenKind::OpCaret:
            case TokenKind::OpPipe:
            case TokenKind::OpAmpersand:
            case TokenKind::OpShiftLeft:
            case TokenKind::OpShiftRight:
            case TokenKind::OpDoubleEqual:
            case TokenKind::OpLessThanEqual:
            case TokenKind::OpGreaterThanEqual:
            case TokenKind::OpExclamationEqual:
            case TokenKind::OpPlusEqual:
            case TokenKind::OpMinusEqual:
            case TokenKind::OpAsteriskEqual:
            case TokenKind::OpSlashEqual:
            case TokenKind::OpPercentEqual:
            case TokenKind::OpCaretEqual:
            case TokenKind::OpPipeEqual:
            case TokenKind::OpAmpersandEqual:
            case TokenKind::OpShiftLeftEqual:
            case TokenKind::OpShiftRightEqual:
            case TokenKind::LeftSquareBracket: {
                const auto operator_ = tok.kind;
                const auto operator_location = tok.location;
                NextToken();
                auto rhs = ParseExpression(binary_precedence(operator_));
                if (not rhs) return rhs.diag();
                lhs = new (tu) BinaryOperation(
                    operator_,
                    *lhs,
                    *rhs,
                    {lhs->location(), rhs->location()}
                );

                if (has_trailing.contains(operator_)) {
                    if (tok.kind != has_trailing.at(operator_)) {
                        auto e = Error("c/expected", "Expected `{}`", has_trailing.at(operator_));
                        e.attach(Note(operator_location, "c/expected", "To match this `{}`", operator_));
                        return e;
                    }
                }
            } break;

            case TokenKind::LeftParenthesis: {
                const auto operator_location = tok.location;
                // Yeet '(';
                NextToken();
                std::vector<Node*> arguments{};
                while (tok.kind != TokenKind::Eof and tok.kind != TokenKind::RightParenthesis) {
                    auto argument = ParseExpression(reset_precedence);
                    if (not argument) return argument.diag();
                    arguments.push_back(*argument);
                    // Yeet separating comma, or hard expect right parenthesis.
                    if (tok.kind == TokenKind::OpComma)
                        NextToken();
                    else break;
                }
                if (tok.kind != TokenKind::RightParenthesis) {
                    auto e = Error("c/expected", "Expected `)`");
                    e.attach(Note(operator_location, "c/expected", "To match this `(`"));
                    return e;
                }
                lhs = new (tu) Call(
                    *lhs,
                    std::move(arguments),
                    {lhs->location(), tok.location}
                );
                // Yeet ')';
                NextToken();
            } break;

                NOT_A_BINARY_OPERATOR
                Diag::ICE("Invalid binary operator");
        }
    }

    return lhs;
};

void Parser::ParseTopLevel(std::string of_file) {
    tu.tree = new (tu) Block({}, {});
    auto top_level = ParseExpressions(TokenKind::Eof);
    if (not top_level) return;
    for (auto n : *top_level)
        ((Block*) tu.tree)->_constituents.emplace_back(n);

    if (context->option_print_ast()) { /** Print Debug Info **/
        for (auto [i, s] : std::ranges::views::enumerate(scopes())) {
            fmt::print("Scope[{}] ({}):\n", i, fmt::ptr(s));
            for (auto [n, d] : s->declarations) {
                fmt::print("- {} <- {} ({})\n", *d->type(), d->name(), fmt::ptr(d));
            }
        }
        for (auto [name, contents] : defines()) {
            fmt::print(
                "-D{}=\"{}\"\n",
                name,
                fmt::join(
                    std::ranges::views::transform(contents, [](auto token) {
                        auto token_source = ToSource(token);
                        if (not token_source)
                            Diag::ICE("Invalid token recorded in preprocessor definition");
                        return fmt::format("{}", *token_source);
                    }),
                    " "
                )
            );
        }
    }
}

auto Parser::Parse(Context* context, File& file) -> TranslationUnit {
    TranslationUnit tu{};
    Parser parser{context, tu, file};
    parser.ParseTopLevel(file.path().filename().string());
    return tu;
}

} // namespace lcc::language_c
