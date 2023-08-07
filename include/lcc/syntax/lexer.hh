#ifndef LCC_SYNTAX_LEXER_HH
#define LCC_SYNTAX_LEXER_HH

#include <lcc/file.hh>
#include <lcc/utils.hh>
#include <lcc/diags.hh>

namespace lcc::syntax {
template <typename TToken>
class Lexer {
    File* file;

    const char* curr{};
    const char* end{};

protected:
    TToken current_token{};
    Context* context{};
    char lastc = ' ';

    Lexer(Context* context, File* file)
        : context(context), file(file), curr(file->data()), end(file->data() + file->size()) { NextChar(); }

    auto FileId() const { return file->file_id(); }
    void NextChar();
    auto CurrentOffset() const -> u32 { return curr - file->data() - 1; }

    template <typename... Args>
    Diag Error(fmt::format_string<Args...> fmt, Args&&... args) { return Diag::Error(context, current_token.location, fmt, std::forward<Args...>(args...)); }

    static bool IsSpace(char c) { return c == ' ' or c == '\t' or c == '\n' or c == '\r' or c == '\f' or c == '\v'; }
};
} // namespace lcc::syntax

#endif // LCC_SYNTAX_LEXER_HH
