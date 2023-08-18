#ifndef C_LEXER_HH
#define C_LEXER_HH

#include <c/ast.hh>
#include <lcc/utils.hh>

namespace lcc::c {
class Lexer {
    Context* _context;
    File* _file;

public:
    Lexer(Context* context, File* file)
        : _context(context), _file(file) {}

private:
};
}

#endif // C_LEXER_HH
