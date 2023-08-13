#include <laye/lexer.hh>

void lcc::laye::Lexer::ReadToken(LayeToken& token)
{
    token = {};
    token.location.pos = CurrentOffset();
    token.location.file_id = (u16)FileId();

    switch (lastc) {
        case '+': {
            NextChar();
            if (lastc == '=') {
                NextChar();
                tok.kind = TokenKind::PlusEqual;
            }
            else tok.kind = TokenKind::Plus;
        } break;
        
        case '-': {
            NextChar();
            if (lastc == '=') {
                NextChar();
                tok.kind = TokenKind::MinusEqual;
            }
            else tok.kind = TokenKind::Minus;
        } break;

        case '*': {
            NextChar();
            if (lastc == '=') {
                NextChar();
                tok.kind = TokenKind::StarEqual;
            }
            else tok.kind = TokenKind::Star;
        } break;

        case '/': {
            NextChar();
            if (lastc == '=') {
                NextChar();
                tok.kind = TokenKind::SlashEqual;
            }
            else tok.kind = TokenKind::Slash;
        } break;

        case '%': {
            NextChar();
            if (lastc == '=') {
                NextChar();
                tok.kind = TokenKind::PercentEqual;
            }
            else tok.kind = TokenKind::Percent;
        } break;

        case '&': {
            NextChar();
            if (lastc == '=') {
                NextChar();
                tok.kind = TokenKind::AmpersandEqual;
            }
            else tok.kind = TokenKind::Ampersand;
        } break;

        case '|': {
            NextChar();
            if (lastc == '=') {
                NextChar();
                tok.kind = TokenKind::PipeEqual;
            }
            else tok.kind = TokenKind::Pipe;
        } break;

        case '~': {
            NextChar();
            if (lastc == '=') {
                NextChar();
                tok.kind = TokenKind::TildeEqual;
            }
            else tok.kind = TokenKind::Tilde;
        } break;

        case '=': {
            NextChar();
            if (lastc == '=') {
                NextChar();
                tok.kind = TokenKind::EqualEqual;
            }
            else if (lastc == '>') {
                NextChar();
                tok.kind = TokenKind::EqualGreater;
            }
            else tok.kind = TokenKind::Equal;
        } break;

        case '!': {
            NextChar();
            if (lastc == '=') {
                NextChar();
                tok.kind = TokenKind::BangEqual;
            }
            else tok.kind = TokenKind::Bang;
        } break;

        case '<': {
            NextChar();
            if (lastc == '=') {
                NextChar();
                tok.kind = TokenKind::LessEqual;
            }
            else if (lastc == '<') {
                NextChar();
                if (lastc == '=') {
                    NextChar();
                    tok.kind = TokenKind::LessLessEqual;
                }
                else tok.kind = TokenKind::LessLess;
            }
            else tok.kind = TokenKind::Less;
        } break;

        case '>': {
            NextChar();
            if (lastc == '=') {
                NextChar();
                tok.kind = TokenKind::GreaterEqual;
            }
            else if (lastc == '>') {
                NextChar();
                if (lastc == '=') {
                    NextChar();
                    tok.kind = TokenKind::GreaterGreaterEqual;
                }
                else tok.kind = TokenKind::GreaterGreater;
            }
            else tok.kind = TokenKind::Greater;
        } break;

        case ':': {
            NextChar();
            if (lastc == ':') {
                NextChar();
                tok.kind = TokenKind::ColonColon;
            }
            else tok.kind = TokenKind::Colon;
        } break;

        case ';': {
            NextChar();
            tok.kind = TokenKind::SemiColon;
        } break;

        case ',': {
            NextChar();
            tok.kind = TokenKind::Comma;
        } break;

        case '.': {
            NextChar();
            tok.kind = TokenKind::Dot;
        } break;

        case '?': {
            NextChar();
            tok.kind = TokenKind::Question;
        } break;

        case '(': {
            NextChar();
            tok.kind = TokenKind::OpenParen;
        } break;

        case ')': {
            NextChar();
            tok.kind = TokenKind::CloseParen;
        } break;

        case '[': {
            NextChar();
            tok.kind = TokenKind::OpenBracket;
        } break;

        case ']': {
            NextChar();
            tok.kind = TokenKind::CloseBracket;
        } break;

        case '{': {
            NextChar();
            tok.kind = TokenKind::OpenBrace;
        } break;

        case '}': {
            NextChar();
            tok.kind = TokenKind::CloseBrace;
        } break;

        case '"': {
            NextString();
        } break;

        case '\'': {
            NextRune();
        } break;

        default: {
            if (IsIdentStart(lastc) /* or IsDigit(lastc) */) {
                NextIdentifierOrNumber();
            }
            else {
                Error("Unknown character in Laye source '{}'", lastc);
            }
        } break;
    }

    token.location.len = (u16)(CurrentOffset() - token.location.pos);
}

void lcc::laye::Lexer::NextIdentifierOrNumber() {
    LCC_ASSERT(IsIdentStart(lastc));
}

void lcc::laye::Lexer::NextString() {
    LCC_ASSERT(lastc == '"');
}

void lcc::laye::Lexer::NextRune() {
    LCC_ASSERT(lastc == '\'');
}
