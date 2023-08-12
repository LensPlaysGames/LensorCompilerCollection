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

    /// Issue an error.
    template <typename... Args>
    Diag Error(Location where, fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag::Error(context, where, fmt, std::forward<Args>(args)...);
    }

    static bool IsIdentStart(char c) { return IsAlphaNumeric(c) or c == '_'; }
    static bool IsIdentContinue(char c) { return IsAlphaNumeric(c) or c == '_'; }
};
}

#endif // LAYE_LEXER_HH
