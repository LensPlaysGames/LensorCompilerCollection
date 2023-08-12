#include "intercept/parser.hh"

#include <cstdlib>
#include <intercept/lexer.hh>

namespace lcc::intercept {
/// All keywords.
const struct {
    std::string kw;
    TokenKind kind;
} keywords[12] = {
    {"if", TokenKind::If},
    {"else", TokenKind::Else},
    {"while", TokenKind::While},
    {"ext", TokenKind::Extern},
    {"as", TokenKind::As},
    {"type", TokenKind::Type},
    {"void", TokenKind::Void},
    {"byte", TokenKind::Byte},
    {"integer", TokenKind::IntegerKw},
    {"for", TokenKind::For},
    {"return", TokenKind::Return},
    {"export", TokenKind::Export}};

static const char* TokenKindToString(TokenKind kind) {
    switch (kind) {
        default: return "invalid";
        case TokenKind::Eof: return "EOF";
        case TokenKind::Ident: return "identifier";
        case TokenKind::Number: return "number";
        case TokenKind::String: return "string";
        case TokenKind::If: return "if";
        case TokenKind::Else: return "else";
        case TokenKind::While: return "while";
        case TokenKind::Extern: return "ext";
        case TokenKind::As: return "as";
        case TokenKind::Export: return "export";
        case TokenKind::Type: return "type";
        case TokenKind::Void: return "void";
        case TokenKind::Byte: return "byte";
        case TokenKind::IntegerKw: return "integer";
        case TokenKind::ArbitraryInt: return "arbitrary_integer";
        case TokenKind::For: return "for";
        case TokenKind::Return: return "return";
        case TokenKind::LParen: return "\"(\"";
        case TokenKind::RParen: return "\")\"";
        case TokenKind::LBrack: return "\"[\"";
        case TokenKind::RBrack: return "\"]\"";
        case TokenKind::LBrace: return "\"{\"";
        case TokenKind::RBrace: return "\"}\"";
        case TokenKind::Comma: return "\",\"";
        case TokenKind::Colon: return "\":\"";
        case TokenKind::Semicolon: return "\";\"";
        case TokenKind::Dot: return "\".\"";
        case TokenKind::Plus: return "\"+\"";
        case TokenKind::Minus: return "\"-\"";
        case TokenKind::Star: return "\"*\"";
        case TokenKind::Slash: return "\"/\"";
        case TokenKind::Percent: return "\"%\"";
        case TokenKind::Ampersand: return "\"&\"";
        case TokenKind::Pipe: return "\"|\"";
        case TokenKind::Caret: return "\"^\"";
        case TokenKind::Tilde: return "\"~\"";
        case TokenKind::Exclam: return "\"!\"";
        case TokenKind::At: return "\"@\"";
        case TokenKind::Hash: return "\"#\"";
        case TokenKind::Shl: return "\"<<\"";
        case TokenKind::Shr: return "\">>\"";
        case TokenKind::Eq: return "\"=\"";
        case TokenKind::Ne: return "\"!=\"";
        case TokenKind::Lt: return "\"<\"";
        case TokenKind::Gt: return "\">\"";
        case TokenKind::Le: return "\"<=\"";
        case TokenKind::Ge: return "\">=\"";
        case TokenKind::ColonEq: return "\":=\"";
        case TokenKind::ColonColon: return "\"::\"";
        case TokenKind::ColonGt: return "\":>\"";
        case TokenKind::Gensym: return "gensym";
        case TokenKind::MacroArg: return "macro_arg";
        case TokenKind::Expression: return "ast_node";
    }
}

void Lexer::NextToken() {
    // Pop empty macro expansions off of the expansion stack.
    while (not macro_expansion_stack.empty()) {
        auto expansion = &macro_expansion_stack.back();
        auto expandee = &macros[expansion->macro_index];
        if (expansion->expansion_index >= expandee->expansion.size()) {
            macro_expansion_stack.pop_back();
        }
    }

    // Iff there are macro expansions to handle, get tokens from there
    // instead of from the file.
    if (not macro_expansion_stack.empty()) {
        auto expansion = &macro_expansion_stack.back();
        auto expandee = &macros[expansion->macro_index];
        auto macro_expansion_token = &expandee->expansion[expansion->expansion_index];
        if (macro_expansion_token->kind == TokenKind::MacroArg) {
            LCC_ASSERT(
                expansion->bound_arguments.size(),
                "Macro argument \"{}\" encountered, but none are defined for this macro",
                macro_expansion_token->text
            );
            // Get it from bound arguments
            auto found = rgs::find_if(
                expansion->bound_arguments,
                [&](auto&& t) { return t.name == macro_expansion_token->text; }
            );
            LCC_ASSERT(
                found != expansion->bound_arguments.end(),
                "Macro argument \"{}\" does not exist (lexer screwed up!)",
                macro_expansion_token->text
            );
            tok = found->token;
        } else tok = *macro_expansion_token;
        ++expansion->expansion_index;

        // Set artificial to false, because, for example, if we are inserting
        // "endmacro", then we want the inserted identifier to *not* be
        // artificial and be treated *seriously*.
        tok.artificial = false;
        // macros within macros within macros within ...
        if (tok.kind == TokenKind::Ident && tok.text == "macro")
            NextMacro();

        return;
    }

    tok.artificial = false;

    if (!lastc) {
        tok.kind = TokenKind::Eof;
        return;
    }

    tok.kind = TokenKind::Invalid;

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
            // Yeet backslash;
            NextChar();
            // Get identifier
            bool in_raw_mode = raw_mode;
            raw_mode = true;
            NextToken();
            raw_mode = in_raw_mode;
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
                        tok.text = TokenKindToString(tok.kind);
                    } break;
                }
            }

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
                    if (tok.text == "expr")
                        selector = MacroArgumentSelector::Expr;
                    else if (tok.text == "expr_once")
                        selector = MacroArgumentSelector::ExprOnce;
                    else if (tok.text == "token")
                        ;
                    else Error("Unrecognised macro argument selector identifier");

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
            } else if (lastc == '>') {
                tok.kind = TokenKind::ColonGt;
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
                while (lastc && lastc != '\n') NextChar();
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
                if (IsAlpha(lastc))
                    Error("Invalid integer literal");
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
                if (IsAlpha(lastc))
                    Error("Invalid integer literal");
                break;
            }

            Error("Invalid token");
        } break;
    }

    tok.location.len = (u16) (CurrentOffset() - tok.location.pos);
}

void Lexer::NextIdentifier() {
    auto startOffset = CurrentOffset();

    NextChar();
    while (IsIdentContinue(lastc))
        NextChar();

    tok.kind = TokenKind::Ident;
    tok.text = GetSubstring(startOffset, CurrentOffset());
}

void Lexer::HandleIdentifier() {
    if (!raw_mode && !tok.artificial && tok.text == "macro") {
        NextMacro();
        return;
    }

    auto found_macro = std::find_if(
        macros.begin(),
        macros.end(),
        [&](const auto& m) { return m.name == tok.text; }
    );

    if (found_macro != macros.end()) {
        ExpandMacro(&*found_macro);
        return;
    }

    for (size_t i = 0; i < sizeof keywords / sizeof *keywords; i++) {
        if (keywords[i].kw == tok.text) {
            tok.kind = keywords[i].kind;
            return;
        }
    }

    // Try and parse a number just after encountering `s` or `u` at the
    // beginning of an identifier.
    if (tok.text.size() > 1 && (tok.text[0] == 's' || tok.text[0] == 'i' || tok.text[0] == 'u') && IsDigit(tok.text[1])) {
        const char* cstr = tok.text.c_str();

        /// Convert the number.
        char* end;
        errno = 0;
        tok.integer_value = (u64) std::strtoull(cstr + 1, &end, 10);
        if (errno == ERANGE) Error("Bit width of integer is too large.");
        // If the identifier is something like `s64iam`, it's simply an identifier.
        if (end != cstr + tok.text.size()) return;

        tok.kind = TokenKind::ArbitraryInt;
    }
}

void Lexer::ExpandMacro(Macro* m) {
    MacroExpansion expansion = {
        (usz) (m - macros.data()),
    };
    expansion.location.pos = tok.location.pos;

    raw_mode = true;

    for (const auto& param_tok : m->parameters) {
        NextToken();

        if (param_tok.kind == TokenKind::MacroArg) {
            switch ((MacroArgumentSelector) param_tok.integer_value) {
                case MacroArgumentSelector::Token: {
                    NamedToken bound_arg = {
                        param_tok.text,
                        tok,
                    };
                    expansion.bound_arguments.push_back(bound_arg);
                } break;

                case MacroArgumentSelector::ExprOnce:
                case MacroArgumentSelector::Expr: {
                    u32 beg = tok.location.pos;

                    auto parser = static_cast<Parser*>(this);
                    auto expr = parser->ParseExpr();
                    if (!expr) break;

                    Location location;
                    location.pos = beg;
                    location.len = tok.location.len;
                    location.file_id = tok.location.file_id;

                    InterceptToken expr_token;
                    expr_token.kind = TokenKind::Expression;
                    expr_token.location = location;
                    expr_token.expression = *expr;
                    expr_token.eval_once = (MacroArgumentSelector) param_tok.integer_value == MacroArgumentSelector::ExprOnce;

                    NamedToken bound_arg = {
                        param_tok.text,
                        expr_token,
                    };

                    expansion.bound_arguments.push_back(bound_arg);
                } break;

                default: Diag::ICE("Unhandled macro argument selector kind");
            }

            continue;
        }

        // if (!token_equals(&p->tok, param_tok))
        //     ERR("Ill-formed macro invocation");
    }

    auto end_pos = tok.location.pos + tok.location.len;
    expansion.location.len = (u16) (end_pos - expansion.location.pos);

    for (usz i = 0; i < m->gensym_count; ++i)
        expansion.gensyms.push_back(gensym(gensym_counter++));

    macro_expansion_stack.push_back(expansion);
    raw_mode = false;
    
    NextToken();
}
} // namespace lcc::intercept
