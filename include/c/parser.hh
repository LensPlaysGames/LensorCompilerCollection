#ifndef C_PARSER_HH
#define C_PARSER_HH

#include <c/ast.hh>
#include <c/lexer.hh>
#include <lcc/utils.hh>

namespace lcc::c {
class Parser {
    CContext* _context;
    File* _file;

    Lexer _lexer;

public:
    static auto Parse(CContext* context, File& file) -> TranslationUnit*;

private:
    Parser(CContext* context, File& file)
        : _context(context), _file(&file), _lexer(Lexer{context, &file}) {}
};
}

#endif // C_PARSER_HH
