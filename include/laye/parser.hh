#ifndef LAYE_PARSER_HH
#define LAYE_PARSER_HH

#include <laye/ast.hh>
#include <laye/lexer.hh>
#include <lcc/utils.hh>

namespace lcc::laye {
class Parser {
    Lexer lexer;
    LayeToken current_token{};

    Parser(Context* context, File* file)
        : lexer(Lexer{context, file}) { }

public:
    static std::unique_ptr<Module> Parse(Context* context, File* file);
};
}

#endif // LAYE_PARSER_HH
