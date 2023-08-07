#include <laye/lexer.hh>

void lcc::laye::Lexer::ReadToken(LayeToken& token)
{
    token = LayeToken{};
    token.location.pos = CurrentOffset();
    token.location.file_id = (u16)FileId();

    switch (lastc) {
    }

    token.location.len = (u16)(CurrentOffset() - token.location.pos);
}
