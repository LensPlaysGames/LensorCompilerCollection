#ifndef INTERCEPT_LEXER_HH
#define INTERCEPT_LEXER_HH

#include <intercept/ast.hh>
#include <lcc/context.hh>
#include <lcc/syntax/lexer.hh>
#include <lcc/utils.hh>

namespace lcc::intercept {
class Lexer : public syntax::Lexer<InterceptToken> {
    using Token = InterceptToken;

    enum struct MacroArgumentSelector {
        Token,
        Expr,
        ExprOnce,
    };

    struct Macro {
        std::string name{};
        std::vector<std::string> definitions{};
        std::vector<Token> expansion{};
        std::vector<Token> parameters{};
        Location location{};
    };

    class MacroExpansion {
        Macro* m;
        decltype(m->expansion.begin()) it;
        StringMap<Token> bound_arguments{};
        Location location{};
        std::vector<std::string> gensyms{};

    public:
        MacroExpansion(Lexer& l, Macro& m, StringMap<Token> args, Location loc);

        /// Check if the macro is done expanding.
        bool done() const { return it == m->expansion.end(); }

        /// Get the next token from the expansion.
        auto operator++() -> Token;
    };

    std::deque<Token> lookahead_tokens{};
    std::deque<Macro> macros{};
    std::vector<MacroExpansion> macro_expansion_stack{};
    bool raw_mode = false;
    usz gensym_counter = 0;

    void NextIdentifier();
    void HandleIdentifier();
    void NextString();
    void NextNumber();

    void ExpandMacro(Macro& m);
    void HandleMacroDefinition();

protected:
    Lexer(Context* context, File* file)
        : syntax::Lexer<Token>(context, file) { NextToken(); }

    /// Note: Calling LookAhead() invalidates the addresses of
    /// previous lookaheads, if this requires more tokens to be
    /// parsed.
    ///
    /// Lookahead tokens are 1-based. LookAhead(0) returns the
    /// current token.
    auto LookAhead(usz n) -> Token*;

    void NextToken();

    static bool IsIdentStart(char c) { return IsAlpha(c) or c == '_'; }
    static bool IsIdentContinue(char c) {
        /// Note: '!' is *not* a start character so `!foo` still gets
        /// parsed as `!` + `foo`, but `foo!` gets parsed as one token.
        return IsAlphaNumeric(c) or c == '_' or c == '!';
    }
};
} // namespace lcc::intercept

#endif // INTERCEPT_LEXER_HH
