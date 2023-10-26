#include <cstdlib>
#include <intercept/lexer.hh>
#include <intercept/parser.hh>
#include <lcc/utils/macros.hh>

using Tk = lcc::intercept::TokenKind;

/// All keywords.
namespace {
lcc::StringMap<Tk> keywords{
    {"if", Tk::If},
    {"else", Tk::Else},
    {"while", Tk::While},
    {"do", Tk::Do},
    {"then", Tk::Then},
    {"extern", Tk::Extern},
    {"static", Tk::Static},
    {"as", Tk::As},
    {"as!", Tk::AsBang},
    {"type", Tk::Type},
    {"void", Tk::Void},
    {"bool", Tk::Bool},
    {"byte", Tk::Byte},
    {"int", Tk::IntKw},
    {"for", Tk::For},
    {"return", Tk::Return},
    {"export", Tk::Export},
    {"struct", Tk::Struct},
    {"lambda", Tk::Lambda},
};
} // namespace

auto lcc::intercept::Lexer::LookAhead(usz n) -> Token* {
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

void lcc::intercept::Lexer::NextToken() {
    /// If we have lookahead tokens, and we’re not looking
    /// ahead, use those first.
    if (not looking_ahead and not lookahead_tokens.empty()) {
        tok = std::move(lookahead_tokens.front());
        lookahead_tokens.pop_front();
        return;
    }

    /// Pop empty macro expansions off the expansion stack.
    std::erase_if(macro_expansion_stack, [](MacroExpansion& x){ return x.done(); });

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

    switch (lastc) {
        /// EOF.
        case 0: {
            tok.kind = TokenKind::Eof;
        } break;

        /// EOF.
        case '\\': {
            /// Yeet backslash;
            NextChar();

            /// Get identifier
            tempset raw_mode = true;
            NextToken();

            /// Convert it to text.
            if (tok.kind != TokenKind::Ident) {
                switch (tok.kind) {
                    case TokenKind::MacroArg: {
                        // Prepend dollar sign.
                        tok.text = fmt::format("${}", tok.text);
                    } break;
                    case TokenKind::String: {
                        // Wrap in double quotes.
                        tok.text = fmt::format("\"{}\"", tok.text);
                    } break;
                    case TokenKind::Number: {
                        // Convert number to string
                        tok.text = fmt::format("{}", tok.integer_value);
                    } break;
                    case TokenKind::ArbitraryInt: {
                        tok.text = fmt::format("{}{}", tok.text[0], tok.integer_value);
                    } break;
                    default: {
                        tok.text = ToString(tok.kind);
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
                    Error("Expected identifier following '$' to name macro argument");
                }

                std::string name = tok.text;

                // Parse token category term or whatever (sets integer token member)
                if (lastc == ':') {
                    // Yeet ':'
                    NextChar();
                    // Get selector identifier.
                    NextToken();
                    if (tok.kind != TokenKind::Ident) {
                        Error("Expected identifier following ':' in named macro argument");
                    }

                    auto selector = MacroArgumentSelector::Token;
                    if (tok.text == "expr") selector = MacroArgumentSelector::Expr;
                    else if (tok.text == "expr_once") selector = MacroArgumentSelector::ExprOnce;
                    else if (tok.text == "token") {
                    } else Error("Unrecognised macro argument selector identifier");
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
        } break;

        case '-': {
            NextChar();
            if (IsDigit(lastc)) {
                NextNumber();
                tok.integer_value = -tok.integer_value;

                /// The character after a number must be a whitespace or delimiter.
                if (IsAlpha(lastc)) Error("Invalid integer literal");
            } else {
                tok.kind = TokenKind::Minus;
            }
        } break;

        case '*': {
            tok.kind = TokenKind::Star;
            NextChar();
        } break;

        case '/': {
            tok.kind = TokenKind::Slash;
            NextChar();
        } break;

        case '%': {
            tok.kind = TokenKind::Percent;
            NextChar();
        } break;

        case '&': {
            tok.kind = TokenKind::Ampersand;
            NextChar();
        } break;

        case '|': {
            tok.kind = TokenKind::Pipe;
            NextChar();
        } break;

        case '^': {
            tok.kind = TokenKind::Caret;
            NextChar();
        } break;

        case '~': {
            tok.kind = TokenKind::Tilde;
            NextChar();
        } break;

        case '!': {
            NextChar();
            if (lastc == '=') {
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

            if (IsDigit(lastc)) {
                NextNumber();
                /// The character after a number must be a whitespace or delimiter.
                if (IsAlpha(lastc)) Error("Invalid integer literal");
                break;
            }

            Error("Invalid token");
        } break;
    }

    tok.location.len = (u16) (CurrentOffset() - tok.location.pos);
}

void lcc::intercept::Lexer::NextIdentifier() {
    tok.text.clear();

    /// Note(Sirraide): Istg if anyone gets the genius idea
    /// of extracting a substring instead of appending character
    /// by character, DON’T. There is a REASON why NextChar()
    /// exists. Character != byte in the source file.
    do {
        tok.text += lastc;
        NextChar();
    } while (IsIdentContinue(lastc));
    tok.kind = TokenKind::Ident;
}

void lcc::intercept::Lexer::HandleIdentifier() {
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
    if (
        tok.text.size() > 1 and
        (tok.text[0] == 's' or tok.text[0] == 'i' or tok.text[0] == 'u') and
        IsDigit(tok.text[1])
    ) {
        const char* cstr = tok.text.c_str();

        /// Convert the number.
        char* end;
        errno = 0;
        tok.integer_value = (u64) std::strtoull(cstr + 1, &end, 10);
        if (errno == ERANGE) Error("Bit width of integer is too large.");

        /// If the identifier is something like `s64iam`, it's simply an identifier.
        if (end != cstr + tok.text.size()) return;
        tok.kind = TokenKind::ArbitraryInt;
    }
}

void lcc::intercept::Lexer::NextString() {
    char delim = lastc;
    NextChar();

    tok.text.clear();

    if (delim == '\'') {
        while (lastc != delim) {
            if (lastc == 0) Error("Unterminated string literal");
            tok.text += lastc;
            NextChar();
        }
    } else {
        LCC_ASSERT(delim == '"');
        while (lastc != delim) {
            if (lastc == 0) Error("Unterminated string literal");
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
                    default: Error("Invalid escape sequence");
                }
            } else {
                tok.text += lastc;
            }
            NextChar();
        }
    }

    if (lastc != delim) Error("Unterminated string literal");
    tok.kind = TokenKind::String;
    NextChar();
}

void lcc::intercept::Lexer::NextNumber() {
    static const auto IsBinary = [](char c) { return c == '0' || c == '1'; };
    static const auto IsOctal = [](char c) { return IsDigit(c) and c < '8'; };

    /// Helper that actually parses the number.
    const auto ParseNumber = [&](std::string_view name, auto&& IsValidDigit, int base) {
        /// Yeet prefix.
        if (base != 10) NextChar();

        /// Lex digits.
        while (IsValidDigit(lastc)) {
            tok.text += lastc;
            NextChar();
        }

        /// We need at least one digit.
        tok.location.len = (u16) (CurrentOffset() - tok.location.pos);
        if (tok.text.empty()) Error("Expected at least one {} digit", name);

        /// Actually parse the number.
        const char* cstr = tok.text.c_str();

        /// Convert the number.
        char* end;
        errno = 0;
        tok.integer_value = (u64) std::strtoull(cstr, &end, base);
        if (errno == ERANGE) Error("Bit width of integer is too large.");
        if (end != cstr + tok.text.size()) Error("Invalid integer literal");
    };

    /// Record the start of the number.
    tok.text.clear();

    tok.integer_value = 0;
    tok.kind = TokenKind::Number;

    /// At least one leading zero.
    if (lastc == '0') {
        /// Discard the zero.
        NextChar();

        /// Another zero is an error.
        if (lastc == '0') Error("Leading zeroes are not allowed in decimal literals. Use 0o/0O for octal literals.");
        else if (lastc == 'b' or lastc == 'B') ParseNumber("binary", IsBinary, 2);
        else if (lastc == 'o' or lastc == 'O') ParseNumber("octal", IsOctal, 8);
        else if (lastc == 'x' or lastc == 'X') ParseNumber("hexadecimal", IsHexDigit, 16);

        /// If the next character is a space or delimiter, then this is a literal 0.
        if (IsSpace(lastc) or !IsAlpha(lastc)) return;

        /// Anything else is an error.
        Error("Invalid integer literal");
    }

    /// Any other digit means we have a decimal number.
    ParseNumber("decimal", IsDigit, 10);
}

void lcc::intercept::Lexer::ExpandMacro(Macro& m) {
    bool error_reported = false;
    auto loc = tok.location;
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
                    auto parser = static_cast<Parser*>(this);
                    auto start = tok.location;
                    auto expr = parser->ParseExpr();

                    /// Just set an int if parsing failed.
                    if (not expr) {
                        Token t;
                        t.kind = TokenKind::Number;
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

                default: Diag::ICE("Unhandled macro argument selector kind");
            }
        }

        /// Otherwise, make sure the tokens match.
        if (tok != param_tok and not error_reported) {
            error_reported = true;
            Error("Ill-formed macro invocation");
        }
    }

    macro_expansion_stack.emplace_back(*this, m, std::move(bound_toks), loc);
    raw_mode = false;
    NextToken();
}

void lcc::intercept::Lexer::HandleMacroDefinition() {
    /// Check if we’re at EOF.
    const auto AtEof = [&] {
        if (tok.kind == Tk::Eof) {
            Error("Unexpected end of file while parsing macro argument list");
            return true;
        }

        return false;
    };

    /// Check if we’re at a non-artificial identifier with
    /// a specific value.
    const auto AtMacroKw = [&](auto&&... names) {
        return tok.kind == Tk::Ident and
               not tok.artificial and
               ((tok.text == names) or ...);
    };

    /// At this point, the parser state is at the "macro" token.
    raw_mode = true;
    NextToken();

    /// Lex macro name.
    if (tok.kind != Tk::Ident) Error("Expected macro name after 'macro'");
    auto name = tok.text;

    /// Check that the macro isn’t already defined.
    if (rgs::find_if(macros, [&](auto&& m) { return m.name == name; }) != macros.end())
        Error("Macro '{}' is already defined", name);

    /// Lex parameter token list.
    auto& macro = macros.emplace_back(name);
    for (;;) {
        NextToken();
        if (AtEof()) return;
        if (AtMacroKw("emits", "defines")) break;

        /// If the token is a macro arg, make sure we don’t have a duplicate name.
        if (tok.kind == Tk::MacroArg) {
            auto it = rgs::find_if(
                macro.parameters,
                [&](auto&& t) { return t.kind == Tk::MacroArg and t.text == tok.text; }
            );

            if (it != macro.parameters.end()) Error("Duplicate macro argument name '{}'", tok.text);
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
                Error("Expected identifier in macro definition list");
                continue;
            }

            /// Check for duplicates.
            if (rgs::find(macro.definitions, tok.text) != macro.definitions.end()) {
                Error("Duplicate macro definition '{}'", tok.text);
                continue;
            }

            /// Add the definition.
            macro.definitions.push_back(tok.text);
            if (tok.kind == Tk::Comma) NextToken();
        }
    }

    /// Parse output list.
    for (;;) {
        NextToken();
        if (AtEof()) return;
        if (AtMacroKw("endmacro")) break;

        /// If the next token is a macro arg, make sure it exists.
        if (tok.kind == Tk::MacroArg) {
            auto arg = rgs::find_if(
                macro.parameters,
                [&](auto&& t) { return t.kind == Tk::MacroArg and t.text == tok.text; }
            );

            if (arg == macro.parameters.end())
                Error("Undefined macro argument '{}'", tok.text);
        }

        /// Otherwise, if it is a gensym, create a gensym token for it.
        else if (tok.kind == Tk::Ident and not macro.definitions.empty()) {
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

    /// Yeet 'endmacro'.
    raw_mode = false;
    NextToken();
}

lcc::intercept::Lexer::MacroExpansion::MacroExpansion(
    Lexer& l,
    Macro& m,
    StringMap<Token> args,
    Location loc
) : m(&m), it(m.expansion.begin()), bound_arguments(std::move(args)), location(loc) {
    for (usz i = 0; i < m.definitions.size(); i++)
        gensyms.push_back(fmt::format(".{}", l.gensym_counter++));
}

auto lcc::intercept::Lexer::MacroExpansion::operator++() -> Token {
    Token ret;
    LCC_ASSERT(not done());

    /// If the token is a macro arg, get the bound argument.
    if (it->kind == TokenKind::MacroArg) {
        auto arg = bound_arguments.find(it->text);
        LCC_ASSERT(arg != bound_arguments.end(), "Unbound macro argument '{}'", it->text);
        it++;
        ret = arg->second;
    }

    /// Otherwise, return a copy of the token.
    else { ret = *it++; }

    /// If the token is a gensym, get its value.
    if (ret.kind == Tk::Gensym) {
        ret.kind = Tk::Ident;
        ret.text = gensyms[ret.integer_value];
    }

    /// Mark the token as non-artificial, because, for example,
    /// if we are inserting "endmacro", then we want the inserted
    /// identifier to *not* be artificial so we actually end up
    /// closing a macro definition.
    ret.artificial = false;
    return ret;
}

bool lcc::intercept::InterceptToken::operator==(const InterceptToken& rhs) const {
    if (kind != rhs.kind) return false;
    switch (kind) {
        case TokenKind::Ident:
        case TokenKind::String:
        case TokenKind::Gensym:
        case TokenKind::MacroArg:
            return text == rhs.text;

        case TokenKind::Number:
            return integer_value == rhs.integer_value;

        case TokenKind::ArbitraryInt:
            return integer_value == rhs.integer_value and text == rhs.text;

        case TokenKind::Expression:
            return expression == rhs.expression;

        case TokenKind::Invalid:
        case TokenKind::Eof:
        case TokenKind::LParen:
        case TokenKind::RParen:
        case TokenKind::LBrack:
        case TokenKind::RBrack:
        case TokenKind::LBrace:
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
        case TokenKind::If:
        case TokenKind::Else:
        case TokenKind::While:
        case TokenKind::Do:
        case TokenKind::Then:
        case TokenKind::Extern:
        case TokenKind::Static:
        case TokenKind::As:
        case TokenKind::AsBang:
        case TokenKind::Type:
        case TokenKind::Void:
        case TokenKind::Byte:
        case TokenKind::Bool:
        case TokenKind::IntKw:
        case TokenKind::For:
        case TokenKind::Return:
        case TokenKind::Export:
        case TokenKind::Struct:
        case TokenKind::Lambda:
            return true;
    }

    LCC_UNREACHABLE();
}

std::string_view lcc::intercept::ToString(Tk kind) {
    switch (kind) {
        case Tk::Invalid: return "invalid";
        case Tk::Eof: return "EOF";
        case Tk::Ident: return "identifier";
        case Tk::Number: return "number";
        case Tk::String: return "string";
        case Tk::If: return "if";
        case Tk::Else: return "else";
        case Tk::While: return "while";
        case Tk::Do: return "do";
        case Tk::Then: return "then";
        case Tk::Extern: return "extern";
        case Tk::Static: return "static";
        case Tk::As: return "as";
        case Tk::AsBang: return "as!";
        case Tk::Export: return "export";
        case Tk::Type: return "type";
        case Tk::Void: return "void";
        case Tk::Byte: return "byte";
        case Tk::IntKw: return "int";
        case Tk::ArbitraryInt: return "sized integer";
        case Tk::For: return "for";
        case Tk::Return: return "return";
        case Tk::LParen: return "(";
        case Tk::RParen: return ")";
        case Tk::LBrack: return "[";
        case Tk::RBrack: return "]";
        case Tk::LBrace: return "{";
        case Tk::RBrace: return "}";
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
        case Tk::Gensym: return "gensym";
        case Tk::MacroArg: return "macro arg";
        case Tk::Expression: return "expression";
        case Tk::Bool: return "bool";
        case Tk::Struct: return "struct";
        case Tk::Lambda: return "lambda";
    }

    return "<unknown>";
}
