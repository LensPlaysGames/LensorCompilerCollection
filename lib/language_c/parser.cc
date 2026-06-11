#include <language_c/parser.hh>

#include <language_c/ast.hh>
#include <language_c/type.hh>

#include <lcc/diags.hh>
#include <lcc/utils/result.hh>

#include <fmt/format.h>
#include <fmt/std.h>

#include <cstdlib>
#include <ranges>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace lcc::language_c {

std::string_view ToString(TokenKind k) {
    switch (k) {
        case TokenKind::Invalid: return "invalid";
        case TokenKind::Identifier: return "identifier";
        case TokenKind::Integer: return "integer-literal";
        case TokenKind::Fractional: return "float-literal";
        case TokenKind::KwVoid: return "void";
        case TokenKind::KwInt: return "int";
        case TokenKind::KwReturn: return "return";
        case TokenKind::KwSizeof: return "sizeof";
        case TokenKind::KwAlignof: return "_Alignof";
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

        case TokenKind::KwVoid:
        case TokenKind::KwInt:
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
    {"sizeof", TokenKind::KwSizeof},
    {"_Alignof", TokenKind::KwAlignof},
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
            CurrentOffset()
        );
        // CurrentOffset() can only ever be equal to the end, but the length of
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

    // Note: Istg if anyone gets the genius idea of extracting a substring
    // instead of appending character by character, DON’T. There is a REASON
    // why NextChar() exists. Character != byte in the source file.
    tok.location.pos = CurrentOffset();
    auto start_position = tok.location.pos;
    do {
        if (lastc > 0xff)
            Diag::ICE("Handle unicode codepoint in identifier");
        tok.text += char(lastc);
        NextChar();
    } while (IsIdentifierContinueCharacter(lastc));

    tok.location.len = (u16) Location::length_from_two_offsets_exclusive(
        start_position,
        CurrentOffset()
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
        return;
    }
    auto& in = _including.front();
    lastc = u32(in.at(_including_offset++));
    if (_including_offset >= in.size()) {
        _including.pop_front();
        _including_offset = 0;
    }
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
    // TODO: Handle escaped newlines
    if (preprocessing)
        while (preprocessor_whitespace.contains((char) lastc)) NextChar();
    else
        while (IsSpace(lastc)) NextChar();

    // Record start of token.
    tok.location.pos = CurrentOffset();
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
        case '}': {
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

        // The only way to get here is if the preprocessor calls NextToken but
        // there isn't another one in the line, and the newline isn't escaped.
        case '\n':
            tok.kind = TokenKind::Invalid;
            break;

        case '#': {
            NextChar();

            if (lastc == '#')
                Diag::ICE("TODO: pp Concatenation");

            else if (IsIdentifierStartCharacter(lastc)) {
                NextIdentifier();

                preprocessing = true;

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
                            auto e = Error(open_location, "c/preprocessor", "Expected `>` to close this `<`...");
                            e.fix_by_inserting_at(tok.location, ">");
                        }
                    }
                    // else if (tok.kind == TokenKind::String) {
                    //     path = tok.text;
                    // }
                    else {
                        Error(
                            "c/preprocessor",
                            "Expected `<` or `\"` to begin included path, but got {} instead",
                            tok.kind
                        );
                        // TODO: Synchronize to beginning of next line, or something.
                        tok.kind = TokenKind::Eof;
                        return;
                    }

                    if (not std::filesystem::exists(path)) {
                        Error(
                            "c/preprocessor",
                            "Included file \"{}\" does not exist",
                            path
                        );
                        tok.kind = TokenKind::Eof;
                        return;
                    }

                    while (not (tok.kind == TokenKind::Eof or tok.kind == TokenKind::Invalid)) {
                        Warning("c/preprocessor", "Junk following path of #include directive `{}`", tok.kind);
                        NextToken();
                    }

                    // After this, the next characters we fetch via the lexer API will be from
                    // the included file. This means we can't do our normal handling of "go
                    // until EOF or newline", since, er, this file's tokens are in the way.
                    _including.push_front(File::Read(path));
                } else {
                    Error(
                        "c/preprocessor",
                        "Unrecognized preprocessor statement.\nIf you believe this should be a conditionally supported directive, let the maintainers know."
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

        default: {
            if (IsDecimalDigit(lastc)) {
                NextNumber();
                /// The character after a number must be a whitespace or delimiter.
                if (IsAlpha(lastc)) {
                    Error(
                        "c/invalid-literal",
                        "Invalid integer literal---the character after a number must not be an alpha character"
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

            Error("c/expected", "Unknown: `{}`", lastc);
        } break;
    }

    tok.location.len = (u16) Location::length_from_two_offsets_exclusive(
        start_location.pos,
        CurrentOffset()
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

        case TokenKind::Invalid:
        case TokenKind::Identifier:
        case TokenKind::Integer:
        case TokenKind::Fractional:
        case TokenKind::KwVoid:
        case TokenKind::KwInt:
        case TokenKind::KwReturn:
        case TokenKind::OpEqual:
        case TokenKind::OpLessThan:
        case TokenKind::OpGreaterThan:
        case TokenKind::OpDoublePipe:
        case TokenKind::OpDoubleAmpersand:
        case TokenKind::OpSlash:
        case TokenKind::OpPercent:
        case TokenKind::OpComma:
        case TokenKind::OpCaret:
        case TokenKind::OpPipe:
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
        case TokenKind::OpDot:
        case TokenKind::OpArrow:
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

        case TokenKind::Invalid:
        case TokenKind::Identifier:
        case TokenKind::Integer:
        case TokenKind::Fractional:
        case TokenKind::KwVoid:
        case TokenKind::KwInt:
        case TokenKind::KwReturn:
        case TokenKind::KwSizeof:
        case TokenKind::KwAlignof:
        case TokenKind::OpPlusPlus:
        case TokenKind::OpMinusMinus:
        case TokenKind::OpTilde:
        case TokenKind::RightParenthesis:
        case TokenKind::RightSquareBracket:
        case TokenKind::LeftCurlyBrace:
        case TokenKind::RightCurlyBrace:
        case TokenKind::Semicolon:
        case TokenKind::Eof:
        case TokenKind::OpExclamation:
        case TokenKind::Count:
            return 0;
    }
    Diag::ICE("unreachable");
}
constexpr size_t reset_precedence{0};

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
        ) {
            switch (tok.kind) {
                default:
                    return Error("c/expected", "Invalid start of declarator");

                case TokenKind::Identifier:
                    if (not current_declarator_name.empty())
                        Diag::ICE("Multiple identifiers encountered within a single declarator");

                    current_declarator_name = tok.text;
                    break;

                case TokenKind::OpAsterisk:
                    auto location = Location{current_declarator_type->location(), tok.location};
                    NextToken();
                    current_declarator_type = new PointerType(current_declarator_type, location);
                    break;
            }
            NextToken();
        }

        if (current_declarator_name.empty()) {
            Warning("c/missing-declarations", "Declaration does not declare anything (no name)");
        }

        // Trailing declarator type specifiers (functions, arrays)
        switch (tok.kind) {
            default: break;

            case TokenKind::LeftParenthesis: {
                NextToken();
                while (
                    tok.kind != TokenKind::RightParenthesis
                    and tok.kind != TokenKind::Eof
                    and tok.kind != TokenKind::Invalid
                ) {
                    Diag::ICE("Parse parameters (at `{}`)...", tok.kind);
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

                current_declarator_type = new FunctionType(
                    current_declarator_type,
                    {},
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

                current_declarator_type = new ArrayType(
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
            // TODO: Parse initialiser, if present (i.e. "= <expression>");
            // if (tok.kind == TokenKind::Equal)
            if (tok.kind == TokenKind::OpEqual) {
                NextToken();
                auto maybe_initialiser = ParseExpression(reset_precedence);
                if (not maybe_initialiser) return maybe_initialiser.diag();
                initialiser = *maybe_initialiser;
            }
        }

        parsed_declarations.push_back(
            new Declaration(
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
        auto out = Node::MaybeToGroup(*maybe_decls);
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
            auto e = Error("c/expected", "Expected `;` but got `{}`", ToString(tok.kind));
            if (maybe_expression and maybe_expression->location().seekable(context)) {
                e.fix_by_inserting_at(maybe_expression->get_past_location(), ";");
                e.attach(
                    Note(
                        maybe_expression->location(),
                        "c/expected",
                        "After this"
                    ),
                    true
                );
            } else e.fix_by_inserting_at(tok.location, ";");
            return e;
        }
    }
    return constituents;
}

auto Parser::ParseExpression(size_t current_precedence) -> Result<Node*> {
    Location start_location = tok.location;
    Result<Node*> lhs = Result<Node*>::Null(); /** (!) **/

    // Prefix unary operators
    if (unary_precedence(tok.kind)) {
        Diag::ICE("Handle unary prefix operator");
    }

    switch (tok.kind) {
        case TokenKind::Invalid:
        case TokenKind::Count:
        case TokenKind::Eof:
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
        case TokenKind::OpAmpersand:
            return Error("expected-qualified-id", "Unexpected `{}`", tok.kind);

        case TokenKind::KwReturn: {
            NextToken();
            if (tok.kind != TokenKind::Semicolon and tok.kind != TokenKind::Eof) {
                auto expression = ParseExpression(reset_precedence);
                if (not expression) return expression.diag();
                lhs = new Return(
                    *expression,
                    {start_location, expression->location()}
                );
            } else lhs = new Return(nullptr, start_location);
        } break;

        case TokenKind::KwInt: {
            NextToken();
            // Encountering just 'int' implies a declaration follows.
            lhs = ParseDeclarations(new IntType(start_location));
        } break;

        case TokenKind::KwVoid: {
            NextToken();
            // Encountering just 'void' implies a declaration follows.
            lhs = ParseDeclarations(new VoidType(start_location));
        } break;

        case TokenKind::Semicolon: {
            NextToken();
            return Warning("c/empty-statement", "Empty statement");
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

            lhs = new Block(*constituents, location);
        } break;

        case TokenKind::LeftParenthesis: {
            // Eat "("
            NextToken();

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
            lhs = new IntegerLiteral(size_t(tok.integer_value), tok.location);
            NextToken();
        } break;

        case TokenKind::Identifier:
            Diag::ICE("identifier is unhandled (got `{}`)...", tok.text);

        case TokenKind::Fractional:
        case TokenKind::OpAsterisk:
        case TokenKind::LeftSquareBracket:
        case TokenKind::OpPlus:
        case TokenKind::OpMinus:
        case TokenKind::KwSizeof:
        case TokenKind::KwAlignof:
        case TokenKind::OpExclamation:
        case TokenKind::OpPlusPlus:
        case TokenKind::OpMinusMinus:
        case TokenKind::OpTilde:
            Diag::ICE("{} is unhandled...", tok.kind);
    }

    if (not lhs) return lhs.diag();

    // Once we've parsed an expression, we should check if that expression is
    // the lhs of a binary expression.
    // NOTE: non-zero precedence = an operator
    while (binary_precedence(tok.kind)) {
        if (
            current_precedence
            and binary_precedence(tok.kind) > current_precedence
        ) return lhs;

        switch (tok.kind) {
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
                lhs = new BinaryOperation(
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
                Diag::ICE("Handle function call parsing...");
            }

            // These are NOT binary operators
            case TokenKind::OpComma:
            case TokenKind::OpExclamation:
            case TokenKind::Invalid:
            case TokenKind::Identifier:
            case TokenKind::Integer:
            case TokenKind::Fractional:
            case TokenKind::KwVoid:
            case TokenKind::KwInt:
            case TokenKind::KwReturn:
            case TokenKind::RightParenthesis:
            case TokenKind::RightSquareBracket:
            case TokenKind::LeftCurlyBrace:
            case TokenKind::RightCurlyBrace:
            case TokenKind::Semicolon:
            case TokenKind::Eof:
            case TokenKind::Count:
            case TokenKind::KwSizeof:
            case TokenKind::KwAlignof:
            case TokenKind::OpPlusPlus:
            case TokenKind::OpMinusMinus:
            case TokenKind::OpTilde:
                Diag::ICE("Invalid binary operator");
        }
    }

    return lhs;
};

auto Parser::ParseTopLevel(std::string of_file) -> TranslationUnit {
    auto top_level = ParseExpressions(TokenKind::Eof);
    if (not top_level) return {};
    tree = new Block(*top_level, {});

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

    return {.tree = tree};
}

auto Parser::Parse(Context* context, File& file) -> TranslationUnit {
    Parser parser{context, file};
    return parser.ParseTopLevel(file.path().filename().string());
}

} // namespace lcc::language_c
