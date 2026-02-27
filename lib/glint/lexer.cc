#include <lcc/diags.hh>
#include <lcc/utf8.hh>
#include <lcc/utils.hh>
#include <lcc/utils/macros.hh>
#include <lcc/utils/result.hh>

#include <fmt/format.h>

#include <glint/ast.hh>
#include <glint/error_ids.hh>
#include <glint/lexer.hh>
#include <glint/parser.hh>

#include <concepts>
#include <cstdlib>
#include <iterator>
#include <ranges>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

using Tk = lcc::glint::TokenKind;

/// All keywords.
namespace {
lcc::StringMap<Tk> keywords{
    {"if", Tk::If},
    {"else", Tk::Else}, // Maybe make contextual
    {"while", Tk::While},
    {"external", Tk::External},
    {"export", Tk::Export},
    {"void", Tk::Void},
    {"bool", Tk::Bool},
    {"byte", Tk::Byte},
    {"int", Tk::Int},
    {"uint", Tk::UInt},
    {"float", Tk::Float},
    {"has", Tk::Has},
    {"cfor", Tk::For},
    {"for", Tk::RangedFor},
    {"return", Tk::Return},
    {"struct", Tk::Struct},
    {"enum", Tk::Enum},
    {"sum", Tk::Sum},
    {"union", Tk::Union},
    {"lambda", Tk::Lambda},
    {"supplant", Tk::Supplant},
    {"match", Tk::Match},
    {"switch", Tk::Switch},
    {"print", Tk::Print},
    {"true", Tk::True},
    {"false", Tk::False},
    {"not", Tk::Exclam},
    {"and", Tk::And},
    {"or", Tk::Or},
    {"sizeof", Tk::Sizeof},
    {"alignof", Tk::Alignof},
    {"template", Tk::Template},
    {"typeof", Tk::Typeof},
    {"apply", Tk::Apply},
    {"continue", Tk::Continue},
    {"break", Tk::Break},
    // C FFI Types
    {"cshort", Tk::CShort},
    {"cushort", Tk::CUShort},
    {"cint", Tk::CInt},
    {"cuint", Tk::CUInt},
    {"clong", Tk::CLong},
    {"culong", Tk::CULong},
    {"clonglong", Tk::CLongLong},
    {"culonglong", Tk::CULongLong},
    {"csize", Tk::CLongLong},
    {"cusize", Tk::CULongLong},
    // Bitwise Operators
    {"bit&", Tk::BitAND},
    {"bit|", Tk::BitOR},
    {"bit^", Tk::BitXOR},
    {"bit~", Tk::BitNOT},
};
} // namespace

auto lcc::glint::Lexer::LookAhead(usz n) -> Token* {
    if (n == 0) return &tok;

    /// If we already have enough tokens, just return the nth token.
    const auto idx = n - 1;
    if (idx < lookahead_tokens.size()) return &lookahead_tokens[idx];

    /// Otherwise, lex enough tokens.
    tempset looking_ahead = true;
    auto current = std::move(tok);
    for (usz i = lookahead_tokens.size(); i < n; i++) {
        tok = {};
        NextToken();
        lookahead_tokens.push_back(std::move(tok));
    }
    tok = std::move(current);

    /// Return the nth token.
    return &lookahead_tokens[idx];
}

void lcc::glint::Lexer::NextToken() {
    /// If we have lookahead tokens, and we’re not looking
    /// ahead, use those first.
    if (not looking_ahead and not lookahead_tokens.empty()) {
        tok = std::move(lookahead_tokens.front());
        lookahead_tokens.pop_front();
        return;
    }

    /// Pop empty macro expansions off the expansion stack.
    std::erase_if(macro_expansion_stack, [](MacroExpansion& x) { return x.done(); });

    /// Iff there are macro expansions to handle, get tokens from there
    /// instead of from the file.
    if (not macro_expansion_stack.empty()) {
        auto& expansion = macro_expansion_stack.back();
        tok = ++expansion;

        /// If this token is another macro definition, handle it.
        if (tok.kind == TokenKind::Ident and tok.text == "macro")
            HandleMacroDefinition();

        return;
    }

    /// Reset the token.
    tok.artificial = false;
    tok.kind = TokenKind::Invalid;

    /// Keep returning EOF if we’re at EOF.
    if (not lastc) {
        tok.kind = TokenKind::Eof;
        return;
    }

    /// Skip whitespace.
    while (IsSpace(lastc)) NextChar();
    tok.location.pos = CurrentOffset();

    auto start_location = tok.location;

    switch (lastc) {
        /// EOF.
        case 0: {
            tok.kind = TokenKind::Eof;
        } break;

        // `<character>` byte literal
        case '`': {
            // Yeet opening grave.
            NextChar();
            tok.kind = TokenKind::ByteLiteral;

            if (lastc > 0xff) {
                Error(
                    ErrorId::InvalidByteLiteral,
                    "Byte literal contains a character with a value greater than a byte may contain"
                );
            }

            // Handle Escapes
            if (lastc == '\\') {
                // Yeet escape character
                NextChar();
                switch (lastc) {
                    default:
                        Diag::ICE(
                            "Unhandled escaped character in byte literal: '{}' (U+{:x})\n",
                            lastc,
                            (unsigned int) lastc
                        );

                    case '`':
                        Error(
                            ErrorId::InvalidByteLiteral,
                            "Expected character following escape character '{}'. For byte literal '{}', use two in a row (i.e. '`{}{}`')",
                            '\\',
                            '\\',
                            '\\',
                            '\\'
                        );
                        break;
                    case '\\':
                        tok.integer_value = '\\';
                        break;
                    // Newline
                    case 'n':
                        tok.integer_value = '\n';
                        break;
                    // NUL byte
                    case '0':
                        tok.integer_value = '\0';
                        break;
                }
            } else tok.integer_value = lastc;

            // Yeet character used for byte literal value.
            NextChar();

            // Yeet closing grave.
            if (not (lastc == '`')) {
                Error(
                    ErrorId::InvalidByteLiteral,
                    "Expected '`' character to close byte literal, but got '{}', U+{:04x} instead",
                    (char) lastc,
                    lastc
                );
            }

            NextChar();
        } break;

        // Identifier Escape
        case '\\': {
            // Yeet backslash
            NextChar();

            // Get identifier
            tempset raw_mode = true;
            NextToken();

            // If not already, convert it to text.
            if (tok.kind != TokenKind::Ident) {
                switch (tok.kind) {
                    default: {
                        tok.text = ToString(tok.kind);
                    } break;

                    case TokenKind::MacroArg: {
                        // Prepend dollar sign.
                        tok.text = fmt::format("${}", tok.text);
                    } break;
                    case TokenKind::String: {
                        // Wrap in double quotes.
                        tok.text = fmt::format("\"{}\"", tok.text);
                    } break;
                    case TokenKind::Integer: {
                        // Convert number to string
                        tok.text = fmt::format("{}", tok.integer_value);
                    } break;
                    case TokenKind::Fractional: {
                        // Convert number to string
                        tok.text = fmt::format("{}", tok.fractional_value);
                    } break;
                    case TokenKind::ArbitraryInt: {
                        tok.text = fmt::format("{}{}", tok.text[0], tok.integer_value);
                    } break;
                }
            }

            /// Mark it as artificial.
            tok.kind = TokenKind::Ident;
            tok.artificial = true;
        } break;

        case '$': {
            if (raw_mode) {
                // Yeet '$';
                NextChar();
                // Get name of macro argument (identifier)
                NextToken();

                if (tok.kind != TokenKind::Ident) {
                    Error(
                        ErrorId::Expected,
                        "Expected identifier following '$' to name macro argument"
                    );
                }

                std::string name = tok.text;

                // Parse token category term or whatever (sets integer token member)
                if (lastc == ':') {
                    // Yeet ':'
                    NextChar();
                    // Get selector identifier.
                    NextToken();
                    if (tok.kind != TokenKind::Ident) {
                        Error(
                            ErrorId::Expected,
                            "Expected identifier following ':' in named macro argument"
                        );
                    }

                    auto selector = MacroArgumentSelector::Token;
                    if (tok.text == "expr")
                        selector = MacroArgumentSelector::Expr;
                    else if (tok.text == "expr_once")
                        selector = MacroArgumentSelector::ExprOnce;
                    else if (tok.text == "token") {
                        // no-op
                    } else {
                        Error(
                            ErrorId::LexerMacro,
                            "Unrecognised macro argument selector identifier"
                        );
                    }
                    tok.integer_value = (u64) selector;
                }

                tok.text = std::move(name);
                tok.kind = TokenKind::MacroArg;
            } else {
                NextIdentifier();
                HandleIdentifier();
            }
        } break;

        case '(': {
            tok.kind = TokenKind::LParen;
            NextChar();
        } break;

        case ')': {
            tok.kind = TokenKind::RParen;
            NextChar();
        } break;

        case '[': {
            tok.kind = TokenKind::LBrack;
            NextChar();

            // "[-" :: block comment
            if (lastc == '-') {
                NextChar();
                bool lbrack{false};
                bool exit_possible{false};
                while (lastc) {
                    if (exit_possible and lastc == ']')
                        break;
                    else exit_possible = false;

                    if (lbrack and lastc == '-') {
                        Warning(
                            ErrorId::Miscellaneous,
                            "Apparent nested block comment; this is not supported"
                        );
                    } else lbrack = false;

                    if (lastc == '-')
                        exit_possible = true;
                    else if (lastc == '[')
                        lbrack = true;

                    NextChar();
                }
                if (lastc != ']') {
                    Error(
                        ErrorId::Expected,
                        "Unterminated block comment: expected `-]`"
                    );
                } else NextChar();
                return NextToken();
            }
        } break;

        case ']': {
            tok.kind = TokenKind::RBrack;
            NextChar();
        } break;

        case '{': {
            tok.kind = TokenKind::LBrace;
            NextChar();
        } break;

        case '}': {
            tok.kind = TokenKind::RBrace;
            NextChar();
        } break;

        case ',': {
            tok.kind = TokenKind::Comma;
            NextChar();
        } break;

        case '@': {
            tok.kind = TokenKind::At;
            NextChar();
        } break;

        case ':': {
            NextChar();
            if (lastc == '=') {
                tok.kind = TokenKind::ColonEq;
                NextChar();
            } else if (lastc == ':') {
                tok.kind = TokenKind::ColonColon;
                NextChar();
            } else {
                tok.kind = TokenKind::Colon;
            }
        } break;

        case ';': {
            // Yeet ';'
            NextChar();
            // Line comments begin with `;;`
            if (lastc == ';') {
                while (lastc and lastc != '\n') NextChar();
                return NextToken();
            }

            tok.kind = TokenKind::Semicolon;
        } break;

        case '#': {
            tok.kind = TokenKind::Hash;
            NextChar();
        } break;

        case '.': {
            tok.kind = TokenKind::Dot;
            NextChar();
        } break;

        case '+': {
            tok.kind = TokenKind::Plus;
            NextChar();

            if (lastc == '+') {
                tok.kind = TokenKind::PlusPlus;
                NextChar();
            } else if (lastc == '=') {
                tok.kind = TokenKind::PlusEq;
                NextChar();
            }
        } break;

        case '-': {
            NextChar();
            if (IsDecimalDigit(lastc)) {
                NextNumber();
                tok.integer_value = -tok.integer_value;

                /// The character after a number must be a whitespace or delimiter.
                if (IsAlpha(lastc)) {
                    Error(
                        ErrorId::InvalidIntegerLiteral,
                        "Invalid integer literal"
                    );
                }
            } else if (lastc == '>') {
                tok.kind = TokenKind::RightArrow;
            } else if (lastc == '=') {
                tok.kind = TokenKind::MinusEq;
                NextChar();
            } else if (lastc == '-') {
                tok.kind = TokenKind::MinusMinus;
                NextChar();
            } else {
                tok.kind = TokenKind::Minus;
            }
        } break;

        case '*': {
            tok.kind = TokenKind::Star;
            NextChar();

            if (lastc == '=') {
                tok.kind = TokenKind::StarEq;
                NextChar();
            }
        } break;

        case '/': {
            tok.kind = TokenKind::Slash;
            NextChar();

            if (lastc == '=') {
                tok.kind = TokenKind::SlashEq;
                NextChar();
            }
        } break;

        case '%': {
            tok.kind = TokenKind::Percent;
            NextChar();

            if (lastc == '=') {
                tok.kind = TokenKind::PercentEq;
                NextChar();
            }
        } break;

        case '&': {
            tok.kind = TokenKind::Ampersand;
            NextChar();

            if (lastc == '=') {
                tok.kind = TokenKind::AmpersandEq;
                NextChar();
            }
        } break;

        case '|': {
            tok.kind = TokenKind::Pipe;
            NextChar();

            if (lastc == '=') {
                tok.kind = TokenKind::PipeEq;
                NextChar();
            }
        } break;

        case '^': {
            tok.kind = TokenKind::Caret;
            NextChar();

            if (lastc == '=') {
                tok.kind = TokenKind::CaretEq;
                NextChar();
            }
        } break;

        case '~': {
            tok.kind = TokenKind::Tilde;
            NextChar();

            if (lastc == '=') {
                tok.kind = TokenKind::TildeEq;
                NextChar();
            }
        } break;

        case '!': {
            NextChar();
            if (lastc == '{') {
                NextChar();
                tok.kind = TokenKind::BangLBrace;
            } else if (lastc == '=') {
                NextChar();
                tok.kind = TokenKind::Ne;
            } else tok.kind = TokenKind::Exclam;
        } break;

        case '=': {
            tok.kind = TokenKind::Eq;
            NextChar();
        } break;

        case '<': {
            NextChar();
            if (lastc == '=') {
                NextChar();
                tok.kind = TokenKind::Le;
            } else if (lastc == '<') {
                NextChar();
                tok.kind = TokenKind::Shl;
            } else {
                tok.kind = TokenKind::Lt;
            }
        } break;

        case '>': {
            NextChar();
            if (lastc == '=') {
                NextChar();
                tok.kind = TokenKind::Ge;
            } else if (lastc == '>') {
                NextChar();
                tok.kind = TokenKind::Shr;
            } else {
                tok.kind = TokenKind::Gt;
            }
        } break;

        case '"':
        case '\'': {
            NextString();
        } break;

        default: {
            if (IsIdentStart(lastc)) {
                NextIdentifier();
                HandleIdentifier();
                break;
            }

            if (IsDecimalDigit(lastc)) {
                NextNumber();
                /// The character after a number must be a whitespace or delimiter.
                if (IsAlpha(lastc)) {
                    Error(
                        ErrorId::InvalidIntegerLiteral,
                        "Invalid integer literal"
                    );
                }
                break;
            }

            Error(
                ErrorId::Expected,
                "Invalid token (codepoint 0x{:x}, `{}')",
                lastc,
                utf8::ToString(lastc)
            );
            // Yeet invalid character, skipping it.
            NextChar();
        } break;
    }

    // Roundabout way to shoe-horn regular tokens into existing identifier-
    // only macro system.
    if (not raw_mode and tok.kind != TokenKind::Invalid and tok.kind != TokenKind::Eof) {
        auto stringified_token = ToSource(tok);
        if (stringified_token) {
            auto macro = rgs::find_if(
                macros,
                [&](const auto& m) { return m.name == *stringified_token; }
            );
            if (macro != macros.end()) {
                ExpandMacro(*macro);
                return;
            }
        }
    }

    // If we have a token that's been expanded from a macro, then it's
    // location is already set, and would be severely mucked up if we tried to
    // adjust the length all the way from the macro definition until after the
    // macro invocation.
    if (not tok.from_macro) {
        tok.location.len = (u16) Location::length_from_two_offsets_exclusive(
            start_location.pos,
            CurrentOffset()
        );
        if (lastc == 0)
            tok.location.len += 1;
    }
}

void lcc::glint::Lexer::NextIdentifier() {
    tok.text.clear();

    LCC_ASSERT(IsIdentStart(lastc));

    // Note: Istg if anyone gets the genius idea of extracting a substring
    // instead of appending character by character, DON’T. There is a REASON
    // why NextChar() exists. Character != byte in the source file.
    do {
        if (lastc > 0xff) LCC_TODO("Handle unicode codepoint in identifier");
        tok.text += char(lastc);
        NextChar();
    } while (IsIdentContinue(lastc));
    tok.kind = TokenKind::Ident;
}

void lcc::glint::Lexer::HandleIdentifier() {
    if (not raw_mode and not tok.artificial and tok.text == "macro") {
        HandleMacroDefinition();
        return;
    }

    auto macro = rgs::find_if(
        macros,
        [&](const auto& m) { return m.name == tok.text; }
    );

    if (macro != macros.end()) {
        ExpandMacro(*macro);
        return;
    }

    if (auto kw = keywords.find(tok.text); kw != keywords.end()) {
        tok.kind = kw->second;
        return;
    }

    /// Try and parse a number just after encountering `s` or `u` at the
    /// beginning of an identifier.
    if (tok.text.size() > 1
        and (tok.text[0] == 's' or tok.text[0] == 'i' or tok.text[0] == 'u')
        and IsDecimalDigit(u32(tok.text[1]))) {
        const char* cstr = tok.text.c_str();

        /// Convert the number.
        char* end;
        errno = 0;
        tok.integer_value
            = (u64) std::strtoull(cstr + 1, &end, 10);
        if (errno == ERANGE) {
            Error(
                ErrorId::InvalidIntegerLiteral,
                "Bit width of integer is too large."
            );
        }

        /// If the identifier is something like `s64iam`, it's simply an identifier.
        if (end != cstr + tok.text.size()) return;
        tok.kind = TokenKind::ArbitraryInt;
    }
}

void lcc::glint::Lexer::NextString() {
    auto delim = lastc;
    NextChar();

    tok.text.clear();

    if (delim == '\'') {
        while (lastc != delim) {
            if (lastc == 0) {
                // An unterminated string literal is NOT a valid token.
                Error(
                    ErrorId::InvalidStringLiteral,
                    "Unterminated string literal"
                );
                tok.kind = TokenKind::Eof;
                return;
            }
            if (lastc > 0xff) LCC_TODO("Handle unicode codepoint in string literal");
            tok.text += char(lastc);
            NextChar();
        }
    } else {
        LCC_ASSERT(delim == '"');
        while (lastc != delim) {
            if (lastc == 0) {
                // An unterminated string literal is NOT a valid token.
                Error(
                    ErrorId::InvalidStringLiteral,
                    "Unterminated string literal"
                );
                tok.kind = TokenKind::Eof;
                return;
            }
            if (lastc == '\\') {
                NextChar();
                switch (lastc) {
                    case 'n': tok.text += '\n'; break;
                    case 'r': tok.text += '\r'; break;
                    case 't': tok.text += '\t'; break;
                    case 'f': tok.text += '\f'; break;
                    case 'v': tok.text += '\v'; break;
                    case 'a': tok.text += '\a'; break;
                    case 'b': tok.text += '\b'; break;
                    case 'e': tok.text += '\033'; break;
                    case '0': tok.text += '\0'; break;
                    case '\'': tok.text += '\''; break;
                    case '\"': tok.text += '\"'; break;
                    case '\\': tok.text += '\\'; break;
                    default:
                        Error(
                            ErrorId::InvalidStringLiteral,
                            "Invalid escape sequence"
                        );
                }
            } else {
                if (lastc > 0xff) LCC_TODO("Handle unicode codepoint in string literal");
                tok.text += char(lastc);
            }
            NextChar();
        }
    }

    if (lastc != delim) {
        // An unterminated string literal is NOT a valid token.
        Error(
            ErrorId::InvalidStringLiteral,
            "Unterminated string literal"
        );
        tok.kind = TokenKind::Eof;
        return;
    }
    tok.kind = TokenKind::String;
    NextChar();
}

void lcc::glint::Lexer::NextNumber() {
    auto start_location = tok.location;

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
                ErrorId::InvalidIntegerLiteral,
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
                ErrorId::InvalidIntegerLiteral,
                "Bit width of integer is too large."
            );
        }
        if (end != cstr + tok.text.size()) {
            Error(
                ErrorId::InvalidIntegerLiteral,
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

        // Another digit is an error: no leading zeroes are allowed unless they
        // lead to a number base specifier.
        if (IsDecimalDigit(lastc)) {
            Error(
                ErrorId::InvalidIntegerLiteral,
                "Leading zeroes are not allowed in number literals."
            );
            // TODO: Two possible fixes; either replace second zero (currently lastc)
            // with `o` to make it an octal literal, or remove all leading zeroes
            // (unless the literal is all zeroes, in which case, all but one).
        } else if (lastc == 'b' or lastc == 'B')
            ParseNumber("binary", IsBinaryDigit, 2);
        else if (lastc == 'o' or lastc == 'O')
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
            ErrorId::InvalidIntegerLiteral,
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

void lcc::glint::Lexer::ExpandMacro(Macro& m) {
    bool error_reported = false;
    auto start_location = tok.location;
    raw_mode = true;

    /// Match the parameters against the input stream.
    StringMap<Token> bound_toks;
    for (const auto& param_tok : m.parameters) {
        NextToken();

        /// Macro args bind to a value.
        if (param_tok.kind == TokenKind::MacroArg) {
            switch (MacroArgumentSelector(param_tok.integer_value)) {
                /// Bind a token.
                case MacroArgumentSelector::Token:
                    bound_toks[param_tok.text] = tok;
                    continue;

                /// Bind an expression.
                case MacroArgumentSelector::ExprOnce:
                case MacroArgumentSelector::Expr: {
                    static_assert(std::derived_from<Parser, Lexer>);
                    auto* parser = static_cast<Parser*>(this);
                    auto start = tok.location;
                    auto expr = parser->ParseExpr();

                    /// Just set an int if parsing failed.
                    if (not expr) {
                        Token t;
                        t.kind = TokenKind::Integer;
                        t.integer_value = 0;
                        bound_toks[param_tok.text] = t;
                        continue;
                    }

                    /// Otherwise, create an expression token and bind it.
                    Token e;
                    e.kind = TokenKind::Expression;
                    e.location = {start, expr->location()};
                    e.expression = *expr;
                    e.eval_once = param_tok.integer_value == +MacroArgumentSelector::ExprOnce;
                    bound_toks[param_tok.text] = e;
                    continue;
                }
            }
        }

        /// Otherwise, make sure the tokens match.
        if (tok != param_tok and not error_reported) {
            error_reported = true;
            auto arg = ToSource(tok);
            auto param = ToSource(param_tok);
            if (arg and param) {
                Error(
                    ErrorId::LexerMacro,
                    "Ill-formed macro invocation: got token '{}', but expected token '{}' (parameter of macro {})",
                    *arg,
                    *param,
                    m.name
                );
            } else {
                Error(
                    ErrorId::LexerMacro,
                    "Ill-formed macro invocation: argument token does not match fixed parameter token of macro {}",
                    m.name
                );
            }
        }
    }

    macro_expansion_stack.emplace_back(*this, m, std::move(bound_toks), start_location);
    raw_mode = false;
    NextToken();
}

void lcc::glint::Lexer::HandleMacroDefinition() {
    LCC_ASSERT(
        tok.kind == Tk::Ident && tok.text == "macro",
        "HandleMacroDefinition must only be called when lexer state is at `macro`."
    );

    /// Check if we’re at EOF.
    const auto AtEof = [&] {
        if (tok.kind == Tk::Eof) {
            Error(
                ErrorId::Expected,
                "Unexpected end of file while parsing macro"
            );
            return true;
        }

        return false;
    };

    /// Check if we’re at a non-artificial identifier with
    /// a specific value.
    const auto AtMacroKw = [&](auto&&... names) {
        return tok.kind == Tk::Ident
           and not tok.artificial
           and ((tok.text == names) or ...);
    };

    /// At this point, the parser state is at the "macro" token.
    raw_mode = true;
    NextToken(); // eat "macro", plate macro name identifier

    /// Lex macro name.
    auto name_result = ToSource(tok);
    if (not name_result) {
        Error(
            ErrorId::LexerMacro,
            "Could not lex name of macro..."
        );
    }
    auto name = *name_result;

    /// Check that the macro isn’t already defined.
    if (rgs::find_if(macros, [&](auto&& m) { return m.name == name; }) != macros.end()) {
        Error(
            ErrorId::Redefinition,
            "Macro '{}' is already defined",
            name
        );
    }

    auto& macro = macros.emplace_back(name);

    /// Lex parameter token list.
    for (;;) {
        // First iteration eats macro name; afterwards, eats parameter tokens.
        NextToken();

        if (AtEof()) return;
        if (AtMacroKw("emits", "defines")) break;

        /// If the token is a macro arg, make sure we don’t have a duplicate name.
        if (tok.kind == Tk::MacroArg) {
            auto it = rgs::find_if(
                macro.parameters,
                [&](auto&& t) {
                    return t.kind == Tk::MacroArg and t.text == tok.text;
                }
            );

            if (it != macro.parameters.end()) {
                Error(
                    ErrorId::LexerMacro,
                    "Duplicate macro argument name '{}'",
                    tok.text
                );
            }
        }

        /// Add the token.
        macro.parameters.push_back(tok);
    }

    /// If the next token is 'defines', lex the definition list.
    if (tok.kind == Tk::Ident and tok.text == "defines") {
        for (;;) {
            NextToken();
            if (AtEof()) return;
            if (AtMacroKw("emits")) break;

            /// Definitions must be identifiers.
            if (tok.kind != Tk::Ident) {
                Error(
                    ErrorId::Expected,
                    "Expected identifier in macro definition list"
                );
                continue;
            }

            /// Check for duplicates.
            if (rgs::find(macro.definitions, tok.text) != macro.definitions.end()) {
                Error(
                    ErrorId::Redefinition,
                    "Duplicate macro definition '{}'",
                    tok.text
                );
                continue;
            }

            /// Add the definition.
            macro.definitions.push_back(tok.text);
            if (tok.kind == Tk::Comma) NextToken();
        }
    }

    /// Parse output list.
    for (;;) {
        // First iteration eats "emits"; afterwards, eats output tokens.
        NextToken();

        if (AtEof()) return;
        if (AtMacroKw("endmacro")) break;

        /// If the next token is a macro arg, make sure it exists.
        if (tok.kind == Tk::MacroArg) {
            auto arg = rgs::find_if(
                macro.parameters,
                [&](auto&& t) {
                    return t.kind == Tk::MacroArg and t.text == tok.text;
                }
            );

            if (arg == macro.parameters.end())
                Error(
                    ErrorId::LexerMacro,
                    "Undefined macro argument '{}'",
                    tok.text
                );
        }

        else if (tok.kind == Tk::Ident) {
            // If it's a gensym, create a gensym token for it.
            auto it = rgs::find_if(
                macro.definitions,
                [&](auto&& t) { return t == tok.text; }
            );

            if (it != macro.definitions.end()) {
                tok.kind = Tk::Gensym;
                tok.integer_value = usz(std::distance(macro.definitions.begin(), it));
            }
        }

        /// Add the token.
        macro.expansion.push_back(tok);
    }

    LCC_ASSERT(
        tok.kind == Tk::Ident && tok.text == "endmacro",
        "At end of macro but token is not endmacro; likely error in macro lexing code"
    );

    /// Yeet 'endmacro'.
    raw_mode = false;
    NextToken();
}

lcc::glint::Lexer::MacroExpansion::MacroExpansion(
    Lexer& lexer,
    Macro& macro,
    StringMap<Token> args,
    Location l
) : m(&macro), it(macro.expansion.begin()), bound_arguments(std::move(args)), location(l) {
    for (usz i = 0; i < macro.definitions.size(); i++)
        gensyms.push_back(fmt::format("__L{}", lexer.gensym_counter++));
}

auto lcc::glint::Lexer::MacroExpansion::operator++() -> Token {
    Token ret;
    LCC_ASSERT(not done());

    // If the token is a macro arg, get the bound argument.
    if (it->kind == TokenKind::MacroArg) {
        auto arg = bound_arguments.find(it->text);
        LCC_ASSERT(arg != bound_arguments.end(), "Unbound macro argument '{}'", it->text);
        it++;
        ret = arg->second;
    }
    // Otherwise, return a copy of the token.
    else
        ret = *it++;

    /// If the token is a gensym, get its value.
    if (ret.kind == Tk::Gensym) {
        ret.kind = Tk::Ident;
        ret.text = gensyms[(usz) ret.integer_value];
    }

    // Mark the token as non-artificial, because, for example, if we are
    // inserting "endmacro", then we want the inserted identifier to *not* be
    // artificial so we actually end up closing a macro definition.
    ret.artificial = false;

    ret.from_macro = true;

    return ret;
}

auto lcc::glint::GlintToken::operator==(const GlintToken& rhs) const -> bool {
    if (kind != rhs.kind) return false;
    switch (kind) {
        case TokenKind::Ident:
        case TokenKind::String:
        case TokenKind::Gensym:
        case TokenKind::MacroArg:
            return text == rhs.text;

        case TokenKind::Integer:
        case TokenKind::ByteLiteral:
            return integer_value == rhs.integer_value;

        case TokenKind::Fractional: {
            return std::memcmp(
                       &fractional_value,
                       &rhs.fractional_value,
                       sizeof(fractional_value)
                   )
                == 0;
        }

        case TokenKind::ArbitraryInt:
            return integer_value == rhs.integer_value and text == rhs.text;

        case TokenKind::Expression:
            return expression == rhs.expression;

        case TokenKind::Invalid:
        case TokenKind::Eof:
        case TokenKind::Apply:
        case TokenKind::LParen:
        case TokenKind::RParen:
        case TokenKind::LBrack:
        case TokenKind::RBrack:
        case TokenKind::LBrace:
        case TokenKind::BangLBrace:
        case TokenKind::RBrace:
        case TokenKind::Comma:
        case TokenKind::Colon:
        case TokenKind::Semicolon:
        case TokenKind::Dot:
        case TokenKind::Plus:
        case TokenKind::Minus:
        case TokenKind::Star:
        case TokenKind::Slash:
        case TokenKind::Percent:
        case TokenKind::Ampersand:
        case TokenKind::Pipe:
        case TokenKind::Caret:
        case TokenKind::Tilde:
        case TokenKind::Exclam:
        case TokenKind::At:
        case TokenKind::Hash:
        case TokenKind::Shl:
        case TokenKind::Shr:
        case TokenKind::Eq:
        case TokenKind::Ne:
        case TokenKind::Lt:
        case TokenKind::Gt:
        case TokenKind::Le:
        case TokenKind::Ge:
        case TokenKind::ColonEq:
        case TokenKind::ColonColon:
        case TokenKind::RightArrow:
        case TokenKind::If:
        case TokenKind::Else:
        case TokenKind::While:
        case TokenKind::External:
        case TokenKind::Void:
        case TokenKind::Byte:
        case TokenKind::Bool:
        case TokenKind::Int:
        case TokenKind::UInt:
        case TokenKind::Float:
        case TokenKind::For:
        case TokenKind::RangedFor:
        case TokenKind::Return:
        case TokenKind::Continue:
        case TokenKind::Break:
        case TokenKind::Export:
        case TokenKind::Struct:
        case TokenKind::Enum:
        case TokenKind::Union:
        case TokenKind::Sum:
        case TokenKind::Has:
        case TokenKind::Lambda:
        case TokenKind::Supplant:
        case TokenKind::Match:
        case TokenKind::Switch:
        case TokenKind::Print:
        case TokenKind::True:
        case TokenKind::False:
        case TokenKind::And:
        case TokenKind::Or:
        case TokenKind::Sizeof:
        case TokenKind::Alignof:
        case TokenKind::CShort:
        case TokenKind::CUShort:
        case TokenKind::CInt:
        case TokenKind::CUInt:
        case TokenKind::CLong:
        case TokenKind::CULong:
        case TokenKind::CLongLong:
        case TokenKind::CULongLong:
        case TokenKind::PlusPlus:
        case TokenKind::MinusMinus:
        case TokenKind::StarStar:
        case TokenKind::PlusEq:
        case TokenKind::MinusEq:
        case TokenKind::StarEq:
        case TokenKind::SlashEq:
        case TokenKind::PercentEq:
        case TokenKind::AmpersandEq:
        case TokenKind::PipeEq:
        case TokenKind::CaretEq:
        case TokenKind::TildeEq:
        case TokenKind::LBrackEq:
        case TokenKind::Template:
        case TokenKind::Typeof:
        case TokenKind::BitAND:
        case TokenKind::BitOR:
        case TokenKind::BitXOR:
        case TokenKind::BitNOT:
            return true;
    }

    LCC_UNREACHABLE();
}

auto lcc::glint::ToString(Tk kind) -> std::string_view {
    switch (kind) {
        case Tk::Invalid: return "invalid";
        case Tk::Eof: return "EOF";
        case Tk::Apply: return "apply";
        case Tk::Ident: return "identifier";
        case Tk::Integer: return "integer";
        case Tk::Fractional: return "fractional";
        case Tk::String: return "string";
        case Tk::If: return "if";
        case Tk::Else: return "else";
        case Tk::While: return "while";
        case Tk::External: return "external";
        case Tk::Export: return "export";
        case Tk::Void: return "void";
        case Tk::Byte: return "byte";
        case Tk::Int: return "int";
        case Tk::UInt: return "uint";
        case Tk::Float: return "float";
        case Tk::ArbitraryInt: return "sized integer";
        case Tk::Has: return "has";
        case Tk::For: return "for";
        case Tk::RangedFor: return "for";
        case Tk::Return: return "return";
        case Tk::Continue: return "continue";
        case Tk::Break: return "break";
        case Tk::LParen: return "(";
        case Tk::RParen: return ")";
        case Tk::LBrack: return "[";
        case Tk::RBrack: return "]";
        case Tk::LBrace: return "{";
        case Tk::RBrace: return "}";
        case Tk::BangLBrace: return "!{";
        case Tk::Comma: return ",";
        case Tk::Colon: return ":";
        case Tk::Semicolon: return ";";
        case Tk::Dot: return ".";
        case Tk::Plus: return "+";
        case Tk::Minus: return "-";
        case Tk::Star: return "*";
        case Tk::Slash: return "/";
        case Tk::Percent: return "%";
        case Tk::Ampersand: return "&";
        case Tk::Pipe: return "|";
        case Tk::Caret: return "^";
        case Tk::Tilde: return "~";
        case Tk::Exclam: return "!";
        case Tk::At: return "@";
        case Tk::Hash: return "#";
        case Tk::Shl: return "<<";
        case Tk::Shr: return ">>";
        case Tk::Eq: return "=";
        case Tk::Ne: return "!=";
        case Tk::Lt: return "<";
        case Tk::Gt: return ">";
        case Tk::Le: return "<=";
        case Tk::Ge: return ">=";
        case Tk::ColonEq: return ":=";
        case Tk::ColonColon: return "::";
        case Tk::RightArrow: return "->";
        case Tk::Gensym: return "gensym";
        case Tk::MacroArg: return "macro arg";
        case Tk::Expression: return "expression";
        case Tk::Bool: return "bool";
        case Tk::True: return "true";
        case Tk::False: return "false";
        case Tk::And: return "and";
        case Tk::Or: return "or";
        case Tk::Sizeof: return "sizeof";
        case Tk::Alignof: return "alignof";
        case Tk::Struct: return "struct";
        case Tk::Enum: return "enum";
        case Tk::Union: return "union";
        case Tk::Sum: return "sum";
        case Tk::Lambda: return "lambda";
        case Tk::Supplant: return "supplant";
        case Tk::Match: return "match";
        case Tk::Switch: return "switch";
        case Tk::Print: return "print";
        case Tk::CShort: return "cshort";
        case Tk::CUShort: return "cushort";
        case Tk::CInt: return "cint";
        case Tk::CUInt: return "cuint";
        case Tk::CLong: return "clong";
        case Tk::CULong: return "culong";
        case Tk::CLongLong: return "clonglong";
        case Tk::CULongLong: return "culonglong";
        case Tk::PlusPlus: return "++";
        case Tk::MinusMinus: return "--";
        case Tk::StarStar: return "**";
        case Tk::PlusEq: return "+=";
        case Tk::MinusEq: return "-=";
        case Tk::StarEq: return "*=";
        case Tk::SlashEq: return "/=";
        case Tk::PercentEq: return "%=";
        case Tk::AmpersandEq: return "&=";
        case Tk::PipeEq: return "|=";
        case Tk::CaretEq: return "^=";
        case Tk::TildeEq: return "~=";
        case Tk::LBrackEq: return "[=";
        case Tk::ByteLiteral: return "byte literal";
        case Tk::Template: return "template";
        case Tk::Typeof: return "typeof";
        case TokenKind::BitAND: return "bitand";
        case TokenKind::BitOR: return "bitor";
        case TokenKind::BitXOR: return "bitxor";
        case TokenKind::BitNOT: return "bitnot";
    }

    return "<unknown>";
}

/// Convert a token back to the source it may have been lexed from.
auto lcc::glint::ToSource(
    const lcc::glint::GlintToken& t
) -> lcc::Result<std::string> {
    switch (t.kind) {
        case Tk::Invalid:
            return lcc::Diag::Error(
                "Cannot create source from invalid token {}",
                lcc::glint::ToString(t.kind)
            );

        case Tk::Ident: return t.text;
        case Tk::Integer: return fmt::format("{}", t.integer_value);
        case Tk::Fractional: return fmt::format("{}", t.fractional_value);
        case Tk::ByteLiteral: return fmt::format("`{}`", (char) t.integer_value);
        case Tk::String: return fmt::format("\"{}\"", t.text);
        case Tk::ArbitraryInt: {
            auto c = t.text[0] == 'u' ? 'u' : 's';
            return fmt::format("{}{}", c, t.integer_value);
        }
        case Tk::MacroArg: return fmt::format("${}", t.text);

        case Tk::Eof: return {""};
        case Tk::Apply: return {"apply"};
        case Tk::LParen: return {"("};
        case Tk::RParen: return {")"};
        case Tk::LBrack: return {"["};
        case Tk::RBrack: return {"]"};
        case Tk::LBrace: return {"{"};
        case Tk::RBrace: return {"}"};
        case Tk::BangLBrace: return {"!{"};
        case Tk::Comma: return {","};
        case Tk::Colon: return {":"};
        case Tk::Semicolon: return {";"};
        case Tk::Dot: return {"."};
        case Tk::Plus: return {"+"};
        case Tk::Minus: return {"-"};
        case Tk::Star: return {"*"};
        case Tk::Slash: return {"/"};
        case Tk::Percent: return {"%"};
        case Tk::Ampersand: return {"&"};
        case Tk::Pipe: return {"|"};
        case Tk::Caret: return {"^"};
        case Tk::Tilde: return {"~"};
        case Tk::Exclam: return {"!"};
        case Tk::At: return {"@"};
        case Tk::Hash: return {"#"};
        case Tk::Shl: return {"<<"};
        case Tk::Shr: return {">>"};
        case Tk::Eq: return {"="};
        case Tk::Ne: return {"!="};
        case Tk::Lt: return {"<"};
        case Tk::Gt: return {">"};
        case Tk::Le: return {"<="};
        case Tk::Ge: return {">="};
        case Tk::PlusPlus: return {"++"};
        case Tk::MinusMinus: return {"--"};
        case Tk::StarStar: return {"**"};
        case Tk::PlusEq: return {"+="};
        case Tk::MinusEq: return {"-="};
        case Tk::StarEq: return {"*="};
        case Tk::SlashEq: return {"/="};
        case Tk::PercentEq: return {"%="};
        case Tk::AmpersandEq: return {"&="};
        case Tk::PipeEq: return {"|="};
        case Tk::CaretEq: return {"^="};
        case Tk::TildeEq: return {"~="};
        case Tk::LBrackEq: return {"[="};
        case Tk::ColonEq: return {":="};
        case Tk::ColonColon: return {"::"};
        case Tk::RightArrow: return {"->"};
        case Tk::If: return {"if"};
        case Tk::Else: return {"else"};
        case Tk::While: return {"while"};
        case Tk::Void: return {"void"};
        case Tk::Byte: return {"byte"};
        case Tk::Bool: return {"bool"};
        case Tk::External: return {"external"};
        case Tk::True: return {"true"};
        case Tk::False: return {"false"};
        case Tk::And: return {"and"};
        case Tk::Or: return {"or"};
        case Tk::Int: return {"int"};
        case Tk::UInt: return {"uint"};
        case Tk::Float: return {"float"};
        case Tk::Sizeof: return {"sizeof"};
        case Tk::Alignof: return {"alignof"};
        case Tk::Has: return {"has"};
        case Tk::For: return {"cfor"};
        case Tk::RangedFor: return {"for"};
        case Tk::Return: return {"return"};
        case Tk::Continue: return {"return"};
        case Tk::Break: return {"return"};
        case Tk::Export: return {"export"};
        case Tk::Struct: return {"struct"};
        case Tk::Enum: return {"enum"};
        case Tk::Union: return {"union"};
        case Tk::Sum: return {"sum"};
        case Tk::Lambda: return {"lambda"};
        case Tk::Supplant: return {"supplant"};
        case Tk::Match: return {"match"};
        case Tk::Switch: return {"switch"};
        case Tk::Print: return {"print"};
        case Tk::Template: return {"template"};
        case Tk::Typeof: return {"typeof"};
        case Tk::CShort: return {"cshort"};
        case Tk::CUShort: return {"cushort"};
        case Tk::CInt: return {"cint"};
        case Tk::CUInt: return {"cuint"};
        case Tk::CLong: return {"clong"};
        case Tk::CULong: return {"culong"};
        case Tk::CLongLong: return {"clonglong"};
        case Tk::CULongLong: return {"culonglong"};
        case TokenKind::BitAND: return {"bit&"};
        case TokenKind::BitOR: return {"bit|"};
        case TokenKind::BitXOR: return {"bit^"};
        case TokenKind::BitNOT: return {"bit~"};

        // Er, I'm not sure you can have a gensym token in the input, as they are
        // only ever created by the lexer itself (by parsing an identifier in
        // macro context that was found in the definitions of the expanding
        // macro...).
        // If we were to try to turn it back into source, might we need the
        // originating macro and it's list of defines?
        case Tk::Gensym:
            return lcc::Diag::Error(
                "Cannot convert gensym token into source, seeing as the user should never be able to create these themselves."
            );

        case Tk::Expression:
            return lcc::Diag::Error(
                "Cannot convert expression token into source, seeing as we can't (yet) turn AST node's back to source code."
            );
    }
    LCC_UNREACHABLE();
}
