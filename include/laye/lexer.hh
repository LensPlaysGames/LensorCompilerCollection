#ifndef LAYE_LEXER_HH
#define LAYE_LEXER_HH

#include <laye/ast.hh>
#include <lcc/context.hh>
#include <lcc/syntax/lexer.hh>
#include <lcc/utils.hh>

namespace lcc::laye {
class Lexer : public syntax::Lexer<LayeToken> {
public:
    Lexer(Context* context, File* file)
        : syntax::Lexer<LayeToken>(context, file) { }
    
    void ReadToken(LayeToken& token);
};
}

#endif // LAYE_LEXER_HH
