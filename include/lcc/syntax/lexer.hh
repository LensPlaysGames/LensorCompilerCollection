#ifndef LCC_SYNTAX_LEXER_HH
#define LCC_SYNTAX_LEXER_HH

#include <lcc/file.hh>
#include <lcc/utils.hh>

namespace lcc::syntax {
template <typename TToken>
class Lexer {
    File* file;

    const char* curr{};
    const char* end{};

protected:
    char lastc = ' ';

    Lexer(File* file)
        : file(file), curr(file->data()), end(file->data() + file->size()) { NextChar(); }

    void NextChar();
};
} // namespace lcc::syntax

#endif // LCC_SYNTAX_LEXER_HH
