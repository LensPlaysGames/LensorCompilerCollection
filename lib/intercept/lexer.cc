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
        case TokenKind::SemiColon: return "\";\"";
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
        case TokenKind::SyntaxNode: return "ast_node";
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
                macro_expansion_token->text_value
            );
            // Get it from bound arguments
            auto found = rgs::find_if(
                expansion->bound_arguments,
                [&](auto&& t) { return t.name == macro_expansion_token->text_value; }
            );
            LCC_ASSERT(
                found != expansion->bound_arguments.end(),
                "Macro argument \"{}\" does not exist (lexer screwed up!)",
                macro_expansion_token->text_value
            );
            current_token = found->token;
        } else current_token = *macro_expansion_token;
        ++expansion->expansion_index;

        // Set artificial to false, because, for example, if we are inserting
        // "endmacro", then we want the inserted identifier to *not* be
        // artificial and be treated *seriously*.
        current_token.artificial = false;
        // macros within macros within macros within ...
        if (current_token.kind == TokenKind::Ident && current_token.text_value == "macro")
            NextMacro();

        return;
    }

    current_token.artificial = false;

    if (!lastc) {
        current_token.kind = TokenKind::Eof;
        return;
    }

    current_token.kind = TokenKind::Invalid;

    /// Skip whitespace.
    while (IsSpace(lastc)) NextChar();

    current_token.location.pos = CurrentOffset();

    switch (lastc) {
        /// EOF.
        case 0: {
            current_token.kind = TokenKind::Eof;
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
            if (current_token.kind != TokenKind::Ident) {
                switch (current_token.kind) {
                    case TokenKind::MacroArg: {
                        // Prepend dollar sign.
                        current_token.text_value = fmt::format("${}", current_token.text_value);
                    } break;
                    case TokenKind::String: {
                        // Wrap in double quotes.
                        current_token.text_value = fmt::format("\"{}\"", current_token.text_value);
                    } break;
                    case TokenKind::Number: {
                        // Convert number to string
                        current_token.text_value = fmt::format("{}", current_token.integer_value);
                    } break;
                    case TokenKind::ArbitraryInt: {
                        current_token.text_value = fmt::format("{}{}", current_token.text_value[0], current_token.integer_value);
                    } break;
                    default: {
                        current_token.text_value = TokenKindToString(current_token.kind);
                    } break;
                }
            }

            current_token.kind = TokenKind::Ident;
            current_token.artificial = true;
        } break;

        case '$': {
            if (raw_mode) {
                // Yeet '$';
                NextChar();
                // Get name of macro argument (identifier)
                NextToken();

                if (current_token.kind != TokenKind::Ident) {
                    Error("Expected identifier following '$' to name macro argument");
                }

                std::string name = current_token.text_value;

                // Parse token category term or whatever (sets integer token member)
                if (lastc == ':') {
                    // Yeet ':'
                    NextChar();
                    // Get selector identifier.
                    NextToken();
                    if (current_token.kind != TokenKind::Ident) {
                        Error("Expected identifier following ':' in named macro argument");
                    }

                    auto selector = MacroArgumentSelector::Token;
                    if (current_token.text_value == "expr")
                        selector = MacroArgumentSelector::Expr;
                    else if (current_token.text_value == "expr_once")
                        selector = MacroArgumentSelector::ExprOnce;
                    else if (current_token.text_value == "token")
                        ;
                    else Error("Unrecognised macro argument selector identifier");

                    current_token.integer_value = (u64) selector;
                }

                current_token.text_value = std::move(name);
                current_token.kind = TokenKind::MacroArg;
            } else {
                NextIdentifier();
                HandleIdentifier();
            }
        } break;

        case '(': {
            current_token.kind = TokenKind::LParen;
            NextChar();
        } break;

        case ')': {
            current_token.kind = TokenKind::RParen;
            NextChar();
        } break;

        case '[': {
            current_token.kind = TokenKind::LBrack;
            NextChar();
        } break;

        case ']': {
            current_token.kind = TokenKind::RBrack;
            NextChar();
        } break;

        case '{': {
            current_token.kind = TokenKind::LBrace;
            NextChar();
        } break;

        case '}': {
            current_token.kind = TokenKind::RBrace;
            NextChar();
        } break;

        case ',': {
            current_token.kind = TokenKind::Comma;
            NextChar();
        } break;

        case '@': {
            current_token.kind = TokenKind::At;
            NextChar();
        } break;

        case ':': {
            NextChar();
            if (lastc == '=') {
                current_token.kind = TokenKind::ColonEq;
                NextChar();
            } else if (lastc == ':') {
                current_token.kind = TokenKind::ColonColon;
                NextChar();
            } else if (lastc == '>') {
                current_token.kind = TokenKind::ColonGt;
                NextChar();
            } else {
                current_token.kind = TokenKind::Colon;
            }
        } break;
    }
}
} // namespace lcc::intercept
