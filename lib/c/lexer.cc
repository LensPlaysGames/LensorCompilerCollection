#include <c/lexer.hh>

namespace cc = lcc::c;

namespace {
lcc::StringMap<cc::TokenKind> c89_keywords{
    {"auto", cc::TokenKind::Auto},
    {"break", cc::TokenKind::Break},
    {"case", cc::TokenKind::Case},
    {"char", cc::TokenKind::Char},
    {"const", cc::TokenKind::Const},
    {"continue", cc::TokenKind::Continue},
    {"default", cc::TokenKind::Default},
    {"do", cc::TokenKind::Do},
    {"double", cc::TokenKind::Double},
    {"else", cc::TokenKind::Else},
    {"enum", cc::TokenKind::Enum},
    {"extern", cc::TokenKind::Extern},
    {"float", cc::TokenKind::Float},
    {"for", cc::TokenKind::For},
    {"goto", cc::TokenKind::Goto},
    {"if", cc::TokenKind::If},
    {"int", cc::TokenKind::Int},
    {"long", cc::TokenKind::Long},
    {"register", cc::TokenKind::Register},
    {"return", cc::TokenKind::Return},
    {"short", cc::TokenKind::Short},
    {"signed", cc::TokenKind::Signed},
    {"sizeof", cc::TokenKind::Sizeof},
    {"static", cc::TokenKind::Static},
    {"struct", cc::TokenKind::Struct},
    {"switch", cc::TokenKind::Switch},
    {"typedef", cc::TokenKind::Typedef},
    {"union", cc::TokenKind::Union},
    {"unsigned", cc::TokenKind::Unsigned},
    {"void", cc::TokenKind::Void},
    {"volatile", cc::TokenKind::Volatile},
    {"while", cc::TokenKind::While},
};

lcc::StringMap<cc::TokenKind> c99_keywords{
    {"_Bool", cc::TokenKind::Bool_},
    {"_Complex", cc::TokenKind::Complex_},
    {"_Imaginary", cc::TokenKind::Imaginary_},
    {"inline", cc::TokenKind::Inline},
    {"restrict", cc::TokenKind::Restrict},
};

lcc::StringMap<cc::TokenKind> c11_keywords{
    {"_Alignas", cc::TokenKind::Alignas_},
    {"_Alignof", cc::TokenKind::Alignof_},
    {"_Atomic", cc::TokenKind::Atomic_},
    {"_Generic", cc::TokenKind::Generic_},
    {"_Noreturn", cc::TokenKind::Noreturn_},
    {"_Static_assert", cc::TokenKind::StaticAssert_},
    {"_Thread_local", cc::TokenKind::ThreadLocal_},
};

lcc::StringMap<cc::TokenKind> c23_keywords{
    {"alignas", cc::TokenKind::Alignas},
    {"alignof", cc::TokenKind::Alignof},
    {"bool", cc::TokenKind::Bool},
    {"constexpr", cc::TokenKind::Constexpr},
    {"false", cc::TokenKind::False},
    {"nullptr", cc::TokenKind::Nullptr},
    {"static_assert", cc::TokenKind::StaticAssert},
    {"thread_local", cc::TokenKind::ThreadLocal},
    {"true", cc::TokenKind::True},
    {"typeof", cc::TokenKind::Typeof},
    {"typeof_unqual", cc::TokenKind::TypeofUnqual},
    {"_Decimal128", cc::TokenKind::Decimal128_},
    {"_Decimal32", cc::TokenKind::Decimal32_},
    {"_Decimal64", cc::TokenKind::Decimal64_},
};

lcc::StringMap<cc::TokenKind> gnu_ext_keywords{
    {"__alignof", cc::TokenKind::GNU__alignof},
    {"__alignof__", cc::TokenKind::GNU__alignof__},
    {"__asm", cc::TokenKind::GNU__asm},
    {"__asm__", cc::TokenKind::GNU__asm__},
    {"__attribute", cc::TokenKind::GNU__attribute},
    {"__attribute__", cc::TokenKind::GNU__attribute__},
    {"__builtin_offsetof", cc::TokenKind::GNU__builtin_offsetof},
    {"__builtin_va_arg", cc::TokenKind::GNU__builtin_va_arg},
    {"__complex", cc::TokenKind::GNU__complex},
    {"__complex__", cc::TokenKind::GNU__complex__},
    {"__const", cc::TokenKind::GNU__const},
    {"__extension__", cc::TokenKind::GNU__extension__},
    {"__func__", cc::TokenKind::GNU__func__},
    {"__FUNCTION__", cc::TokenKind::GNU__FUNCTION__},
    {"__imag", cc::TokenKind::GNU__imag},
    {"__imag__", cc::TokenKind::GNU__imag__},
    {"__inline", cc::TokenKind::GNU__inline},
    {"__inline__", cc::TokenKind::GNU__inline__},
    {"__label__", cc::TokenKind::GNU__label__},
    {"__null", cc::TokenKind::GNU__null},
    {"__PRETTY_FUNCTION__", cc::TokenKind::GNU__PRETTY_FUNCTION__},
    {"__real", cc::TokenKind::GNU__real},
    {"__real__", cc::TokenKind::GNU__real__},
    {"__restrict", cc::TokenKind::GNU__restrict},
    {"__restrict__", cc::TokenKind::GNU__restrict__},
    {"__signed", cc::TokenKind::GNU__signed},
    {"__signed__", cc::TokenKind::GNU__signed__},
    {"__thread", cc::TokenKind::GNU__thread},
    {"__typeof", cc::TokenKind::GNU__typeof},
    {"__volatile", cc::TokenKind::GNU__volatile},
    {"__volatile__", cc::TokenKind::GNU__volatile__},
};
};

void cc::Lexer::ReadTokenNoPreprocess(CToken& token) {
    token.kind = TokenKind::Invalid;
    token.location.len = 0;
    token.location.file_id = (u16) _file->file_id();
    token.text.clear();
    token.integer_value = 0;
    token.float_value = 0;
    token.artificial = false;
    token.integer_kind = LitIntegerKind::Int;
    token.float_kind = LitFloatKind::Double;
    token.macro_arg_index = -1;

    /*

    Things to be kept in mind when reading tokens:

    1.  Remove all comments as they are encountered:
        This is more formally "Replace all C comments with a single space".

        Comments are allowed to contain newlines that split a preprocessor directive
        accross multiple lines. If a preprocessor directive is being parsed and
        a comment takes the lexer to another line, it should not stop parsing the
        preprocessor directive. I don't think this applies to line comments.

        An exception to this rule is the #include directive with '<' and '>' delimited file names.
        This means specifically that a comment is not recognized while within the "quoted" portion.

    2.  Delete Backslash+Newline absolutely everywhere it is seen.
        This can likely be done in the AdvanceChar() routine to be handled silently and automatically.
        The one exception is if we ever allow trigraphs. If trigraphs are enabled, the do not
        parse if broken up by Backslash+Newline and will have to be handled separately.

        Note that the behavior of Backslash+Newline is unclean in string literals, but is standard.

    3.  Expand pre-defined macro names with their expansions (when not within a string or character literal).
        This one is self explanatory and will probably just *happen* when we decide what an identifier is.

    4.  Preprocessor directives need to be checked correctly.
        If a '#' is the first non-comment, non-whitespace character of a line, it must always start a
        preprocessor directive. The preprocessor directive name is never expanded and must be an identifier.

    5.  Unterminated comments in #include'd files report errors and do not carry over.
        This behavior is true of most of #include behavior, but I noted it here as a reminder that
        lexing should be very #include aware and stop parsing tokens at the end of an #include'd file.

        Include guards can be optimized by remembering which files are entirely contained within
        #ifndef/#endif and skipping them if the symbol is defined.

    6.  Parsing #define bodies does not invoke the macro expansion handler.
        Macro expansion does not happen in preprocessor defines. When a macro is expanded,
        only then is its body checked for more expansions. Notes will need to be taken on when and
        how exactly a macro is marked as not expandable during this expansion process.

    7.  Macro arguments are separated by commas.
        If ever you need a comma in your argument, it can only exist when enclosed in parenthesis.
        Other delimiters are ignored for this purpose, so square and curly braces do not affect
        this behavior.

        Macro argument lists must not have whitespace between the macro name and the opening parenthesis
        of the list of arguments, but using that macro does *not* require those spaces. I assume the
        easiest way to solve this is to look up an identifier, and only if it's a macro with arguments
        do we scan forward until a non-whitespace character is encountered to see if it's being expanded
        or not. If it is not, it does not expand at all and is used as a regular identifier.

    */

    EatWhitespace();
    token.location.pos = CurrentOffset();

    u64 integer_value = 0;

    switch (CurrentChar()) {
        case '\n': {
            LCC_ASSERT(IsInPreprocessor());
            AdvanceChar();
            token.kind = TokenKind::EndOfLine;
        } break;

        // Grouping Delimiters
        case '(': {
            AdvanceChar();
            token.kind = TokenKind::OpenParen;
        } break;

        case ')': {
            AdvanceChar();
            token.kind = TokenKind::CloseParen;
        } break;

        case '[': {
            AdvanceChar();
            token.kind = TokenKind::OpenBracket;
        } break;

        case ']': {
            AdvanceChar();
            token.kind = TokenKind::CloseBracket;
        } break;

        case '{': {
            AdvanceChar();
            token.kind = TokenKind::OpenBrace;
        } break;

        case '}': {
            AdvanceChar();
            token.kind = TokenKind::CloseBrace;
        } break;

        // Other Delimiters
        case '.': {
            if (IsDigit(PeekCharSkipEscapedNewline())) {
                goto lex_constant_real_at_dot;
            }

            AdvanceChar();
            if (CurrentChar() == '.' and PeekCharSkipEscapedNewline() == '.') {
                AdvanceChar(); // the second dot
                AdvanceChar(); // the third dot
                token.kind = TokenKind::TripleDot;
            } else token.kind = TokenKind::Dot;
        } break;

        case ',': {
            AdvanceChar();
            token.kind = TokenKind::Comma;
        } break;

        case ':': {
            AdvanceChar();
            token.kind = TokenKind::Colon;
        } break;

        case ';': {
            AdvanceChar();
            token.kind = TokenKind::SemiColon;
        } break;

        // Operators
        case '=': {
            AdvanceChar();
            if (CurrentChar() == '=') {
                AdvanceChar();
                token.kind = TokenKind::EqualEqual;
            } else token.kind = TokenKind::Equal;
        } break;

        case '+': {
            AdvanceChar();
            if (CurrentChar() == '=') {
                AdvanceChar();
                token.kind = TokenKind::PlusEqual;
            } else if (CurrentChar() == '+') {
                AdvanceChar();
                token.kind = TokenKind::PlusPlus;
            } else token.kind = TokenKind::Plus;
        } break;

        case '-': {
            AdvanceChar();
            if (CurrentChar() == '=') {
                AdvanceChar();
                token.kind = TokenKind::MinusEqual;
            } else if (CurrentChar() == '=') {
                AdvanceChar();
                token.kind = TokenKind::MinusMinus;
            } else if (CurrentChar() == '>') {
                AdvanceChar();
                token.kind = TokenKind::MinusGreater;
            } else token.kind = TokenKind::Minus;
        } break;

        case '*': {
            AdvanceChar();
            if (CurrentChar() == '=') {
                AdvanceChar();
                token.kind = TokenKind::StarEqual;
            } else token.kind = TokenKind::Star;
        } break;

        case '/': {
            AdvanceChar();
            if (CurrentChar() == '=') {
                AdvanceChar();
                token.kind = TokenKind::SlashEqual;
            } else token.kind = TokenKind::Slash;
        } break;

        case '%': {
            AdvanceChar();
            if (CurrentChar() == '=') {
                AdvanceChar();
                token.kind = TokenKind::PercentEqual;
            } else token.kind = TokenKind::Percent;
        } break;

        case '&': {
            AdvanceChar();
            if (CurrentChar() == '=') {
                AdvanceChar();
                token.kind = TokenKind::AmpersandEqual;
            } else if (CurrentChar() == '&') {
                AdvanceChar();
                token.kind = TokenKind::AmpersandAmpersand;
            } else token.kind = TokenKind::Ampersand;
        } break;

        case '|': {
            AdvanceChar();
            if (CurrentChar() == '=') {
                AdvanceChar();
                token.kind = TokenKind::PipeEqual;
            } else if (CurrentChar() == '|') {
                AdvanceChar();
                token.kind = TokenKind::PipePipe;
            } else token.kind = TokenKind::Pipe;
        } break;

        case '^': {
            AdvanceChar();
            if (CurrentChar() == '=') {
                AdvanceChar();
                token.kind = TokenKind::CaretEqual;
            } else token.kind = TokenKind::Caret;
        } break;

        case '<': {
            AdvanceChar();
            if (CurrentChar() == '=') {
                AdvanceChar();
                token.kind = TokenKind::LessEqual;
            } else if (CurrentChar() == '<') {
                AdvanceChar();
                if (CurrentChar() == '=') {
                    AdvanceChar();
                    token.kind = TokenKind::LessLessEqual;
                } else token.kind = TokenKind::LessLess;
            } else token.kind = TokenKind::Less;
        } break;

        case '>': {
            AdvanceChar();
            if (CurrentChar() == '=') {
                AdvanceChar();
                token.kind = TokenKind::GreaterEqual;
            } else if (CurrentChar() == '>') {
                AdvanceChar();
                if (CurrentChar() == '=') {
                    AdvanceChar();
                    token.kind = TokenKind::GreaterGreaterEqual;
                } else token.kind = TokenKind::GreaterGreater;
            } else token.kind = TokenKind::Greater;
        } break;

        case '~': {
            AdvanceChar();
            token.kind = TokenKind::Tilde;
        } break;

        case '!': {
            AdvanceChar();
            if (CurrentChar() == '=') {
                AdvanceChar();
                token.kind = TokenKind::BangEqual;
            } else token.kind = TokenKind::Bang;
        } break;

        case '?': {
            AdvanceChar();
            token.kind = TokenKind::Question;
        } break;

        case '"': {
            LCC_ASSERT(false, "TODO C string literals");
        } break;

        case '\'': {
            LCC_ASSERT(false, "TODO C character literals");
        } break;

        // clang-format off
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9': { // clang-format on
            {
                int integer_radix = 10;
                bool integer_too_large = false;

                if (CurrentChar() == '0') {
                    char peek_char = PeekCharSkipEscapedNewline();
                    if (peek_char == 'x' or peek_char == 'X') {
                        AdvanceChar(); // the zero
                        AdvanceChar(); // the x
                        integer_radix = 16;
                    } else {
                        integer_radix = 8;
                    }
                }

                bool errored_on_digit = false;
                while (IsDigit(CurrentChar()) and (integer_radix == 16 ? IsDigitInBase(CurrentChar(), integer_radix) : true)) {
                    if (not IsDigitInBase(CurrentChar(), integer_radix)) {
                        if (IsDigit(CurrentChar())) {
                            if (not errored_on_digit) {
                                LCC_ASSERT(integer_radix == 8);

                                errored_on_digit = true;
                                integer_value = 0;
                                integer_too_large = false;

                                Error("Invalid digit '{}' in octal literal", CurrentChar());
                                continue;
                            }
                        } else break;
                    }

                    if (not errored_on_digit) {
                        int digit_value = CurrentChar() - '0';
                        if ((std::numeric_limits<u64>::max() - (u64) digit_value) / (u64) integer_radix < integer_value)
                            integer_too_large = true;
                        if (not integer_too_large)
                            integer_value = integer_value * (u64) integer_radix + (u64) digit_value;
                    }

                    AdvanceChar();
                }
            }

            if (CurrentChar() != '.') {
                token.kind = TokenKind::LitInt;
                token.integer_value = integer_value;

                if (IsAlpha(CurrentChar())) {
                    LCC_ASSERT(false, "TODO C integer literal suffixes");
                }

                break;
            }

        lex_constant_real_at_dot:;
            LCC_ASSERT(false, "TODO C real literals");
        } break;

        case '$': {
            if (c_context()->opts.ext.gnu_idents) {
                goto lex_identifier;
            }

            goto default_case;
        }

        // clang-format off
        case 'a': case 'b': case 'c': case 'd': case 'e':
        case 'f': case 'g': case 'h': case 'i': case 'j':
        case 'k': case 'l': case 'm': case 'n': case 'o':
        case 'p': case 'q': case 'r': case 's': case 't':
        case 'u': case 'v': case 'w': case 'x': case 'y':
        case 'z':

        case 'A': case 'B': case 'C': case 'D': case 'E':
        case 'F': case 'G': case 'H': case 'I': case 'J':
        case 'K': case 'L': case 'M': case 'N': case 'O':
        case 'P': case 'Q': case 'R': case 'S': case 'T':
        case 'U': case 'V': case 'W': case 'X': case 'Y':
        case 'Z':

        case '_': { // clang-format on
        lex_identifier:;
            std::string identifier_text{};

            // NOTE(local): we can have a fast path without unicode, and a slower path once we identify them
            while (IsAlphaNumeric(CurrentChar()) or CurrentChar() == '_') {
                identifier_text.push_back(CurrentChar());
                AdvanceChar();
            }

            while (SkipBackslashWithNewline()) {}
            if (CurrentChar() == '\\' and (PeekCharSkipEscapedNewline() == 'u' or PeekCharSkipEscapedNewline() == 'U')) {
                LCC_ASSERT(false, "TODO fancy C identifiers");
            }

            token.kind = TokenKind::Ident;
            token.text = std::move(identifier_text);
        } break;

        default: {
        default_case:;
            if (IsAtEndOfFile()) {
                token.kind = TokenKind::EndOfFile;
            } else {
                Error("Invalid character in C source");
                AdvanceChar();
            }
        } break;
    }

    token.location.len = (u16) (CurrentOffset() - token.location.pos);
}

void cc::Lexer::ReadToken(CToken& token) {
    EatWhitespace();
    if (IsAtStartOfLine() and CurrentChar() == '#') {
        HandlePreprocessorDirective();
        LCC_ASSERT(not IsInPreprocessor());
    }

    ReadTokenNoPreprocess(token);
    // TODO(local): handle macro expansions n stuff

    if (token.kind == TokenKind::Ident) {
        /// If an identifier makes it this far, it didn't get eaten by the preprocessor.
        /// Check if it's a keyword and transform it if so.

        auto LookupKeyword = [](const std::string& image, const StringMap<TokenKind>& kw_map) {
            if (auto it = kw_map.find(image); it != kw_map.end()) {
                return it->second;
            }
            return TokenKind::Ident;
        };

        if (+c_context()->opts.std >= +StandardVersion::C89)
            token.kind = LookupKeyword(token.text, c89_keywords);

        if (token.kind == TokenKind::Ident and +c_context()->opts.std >= +StandardVersion::C99)
            token.kind = LookupKeyword(token.text, c99_keywords);

        if (token.kind == TokenKind::Ident and +c_context()->opts.std >= +StandardVersion::C11)
            token.kind = LookupKeyword(token.text, c11_keywords);

        if (token.kind == TokenKind::Ident and +c_context()->opts.std >= +StandardVersion::C23)
            token.kind = LookupKeyword(token.text, c23_keywords);

        // TODO(local): extension keywords
    }
}

void cc::Lexer::HandlePreprocessorDirective() {
    LCC_ASSERT(not IsInPreprocessor());
    LCC_ASSERT(IsAtStartOfLine() and CurrentChar() == '#');

    is_in_preprocessor = true;
    AdvanceChar(); // skip '#'

    CToken token;
    ReadTokenNoPreprocess(token);

    switch (token.kind) {
        default: {
        invalid_preprocessing_directive:;
            Error(token.location, "Invalid preprocessing directive");
            SkipToEndOfPreprocessorDirective(token);
        } break;

            // case TokenKind::LitInt: // line directive???

        case TokenKind::Ident: {
            goto invalid_preprocessing_directive;
            std::string ident_value = token.text;

            if (ident_value == "define")
                HandleDefineDirective(token);
            else goto invalid_preprocessing_directive;
        } break;
    }

    LCC_ASSERT(IsInPreprocessor());
    is_in_preprocessor = false;
}

void cc::Lexer::SkipToEndOfPreprocessorDirective(CToken current_token) {
    LCC_ASSERT(IsInPreprocessor());
    while (not IsAtEndOfFile() and current_token.kind != TokenKind::EndOfLine)
        ReadTokenNoPreprocess(current_token);
}

void cc::Lexer::HandleDefineDirective(const CToken& define_token) {
    LCC_ASSERT(IsInPreprocessor());
    LCC_ASSERT(define_token.kind == TokenKind::Ident and define_token.text == "define");

    CToken token;
    ReadTokenNoPreprocess(token);

    if (token.kind == TokenKind::EndOfLine) {
        Error(token.location, "Macro name missing");
        return; // we already hit the TokenKind::EndOfLine, nothing more to do
    }

    if (token.kind != TokenKind::Ident) {
        Error(token.location, "Macro name must be an identifier");
        SkipToEndOfPreprocessorDirective(token);
        return;
    }

    LCC_ASSERT(token.kind == TokenKind::Ident);
    std::string macro_name = token.text;

    bool macro_has_arguments = false;
    std::vector<std::string> macro_args{};
    std::vector<CToken> macro_body{};

    if (CurrentChar() == '(') {
        /// This is a macro with arguments.
        macro_has_arguments = true;
        AdvanceChar(); // skip the '('

        for (;;) {
            ReadTokenNoPreprocess(token);
            if (token.kind == TokenKind::EndOfLine or token.kind == TokenKind::EndOfFile) {
                Error(token.location, "Expected ')' in macro parameter list");
                SkipToEndOfPreprocessorDirective(token);
                return;
            }

            if (token.kind != TokenKind::Ident) {
                Error(token.location, "Invalid token in macro parameter list");
                SkipToEndOfPreprocessorDirective(token);
                return;
            }

            LCC_ASSERT(token.kind == TokenKind::Ident);
            macro_args.push_back(token.text);

            ReadTokenNoPreprocess(token);
            if (token.kind == TokenKind::CloseParen)
                break;
            else if (token.kind != TokenKind::Comma) {
                Error(token.location, "Expected comma in macro parameter list");
                SkipToEndOfPreprocessorDirective(token);
                return;
            }
        }
    }

    while (not IsAtEndOfFile()) {
        ReadTokenNoPreprocess(token);
        if (token.kind == TokenKind::EndOfLine)
            break;

        if (token.kind == TokenKind::Ident and macro_has_arguments) {
            LCC_ASSERT(token.macro_arg_index == -1);
            for (usz i = 0; i < macro_args.size(); i++) {
                if (macro_args[i] == token.text) {
                    token.macro_arg_index = (isz) i;
                    break;
                }
            }
        }

        macro_body.push_back(token);
    }

    MacroDef macro_def{
        macro_name,
        macro_has_arguments,
        std::move(macro_args),
        std::move(macro_body)};
    macro_defs.emplace(macro_name, macro_def);
}

void cc::Lexer::EatWhitespace() {
    while (not IsAtEndOfFile() and not IsSpace(CurrentChar())) {
        if (IsInPreprocessor() and CurrentChar() == '\n')
            break;

        AdvanceChar();
    }
}
