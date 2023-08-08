#include <intercept/lexer.hh>

namespace lcc::intercept {
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
        case TokenKind::Ext: return "ext";
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
    }
}
} // namespace lcc::intercept
