#ifndef INTERCEPT_LEXER_HH
#define INTERCEPT_LEXER_HH

#include <intercept/ast.hh>
#include <lcc/context.hh>
#include <lcc/syntax/lexer.hh>
#include <lcc/utils.hh>

namespace lcc::intercept {
class Lexer : public syntax::Lexer<InterceptToken> {
    enum struct MacroArgumentSelector {
        Token,
        Expr,
        ExprOnce,
    };

    struct NamedToken {
        std::string_view name{};
        InterceptToken token{};
    };

    struct Macro {
        std::string name{};
        std::vector<InterceptToken> parameters{};
        std::vector<InterceptToken> expansion{};
        Location location{};
        usz gensym_count = 0;
    };

    struct MacroExpansion {
        usz macro_index = 0;
        usz expansion_index = 0;
        std::vector<NamedToken> bound_arguments{};
        Location location{};
        std::vector<std::string> gensyms{};
    };

    std::vector<Macro> macros{};
    std::vector<MacroExpansion> macro_expansion_stack{};
    bool raw_mode = false;

    void NextIdentifier();
    void HandleIdentifier();
    void NextString();
    void ParseNumber(int base);
    void NextNumber();
    void NextMacro();
    void ExpandMacro(Macro* m);

protected:
    Lexer(Context* context, File* file)
        : syntax::Lexer<InterceptToken>(context, file) { NextToken(); }

    void NextToken();
};
} // namespace lcc::intercept

#endif // INTERCEPT_LEXER_HH
