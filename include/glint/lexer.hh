#ifndef GLINT_LEXER_HH
#define GLINT_LEXER_HH

#include <lcc/context.hh>
#include <lcc/syntax/lexer.hh>
#include <lcc/utils.hh>

#include <glint/ast.hh>
#include <glint/error_ids.hh>

#include <deque>
#include <string>
#include <string_view>
#include <vector>

namespace lcc::glint {
class Lexer : public syntax::Lexer<GlintToken> {
    using Token = GlintToken;

    using syntax::Lexer<GlintToken>::Error;
    using syntax::Lexer<GlintToken>::Warning;
    using syntax::Lexer<GlintToken>::Note;

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
        MacroExpansion(Lexer&, Macro&, StringMap<Token> args, Location);

        /// Check if the macro is done expanding.
        auto done() const -> bool { return it == m->expansion.end(); }

        /// Get the next token from the expansion.
        auto operator++() -> Token;
    };

    std::deque<Token> lookahead_tokens{};
    std::deque<Macro> macros{};
    std::vector<MacroExpansion> macro_expansion_stack{};
    bool raw_mode = false;
    bool looking_ahead = false;
    usz gensym_counter = 0;

    void NextIdentifier();
    void HandleIdentifier();
    void NextString();

    // NOTE: If you make this 0-9, a-f, A-F, or x, /you're gonna have a bad time/.
    static constexpr u32 DigitSeparator = '\'';
    void NextNumber();

    void ExpandMacro(Macro& m);
    void HandleMacroDefinition();

protected:
    Lexer(Context* ctx, std::string_view source)
        : syntax::Lexer<Token>(ctx, source) { NextToken(); }
    Lexer(Context* ctx, File* file)
        : syntax::Lexer<Token>(ctx, file) { NextToken(); }

    /// Note: Calling LookAhead() invalidates the addresses of
    /// previous lookaheads, if this requires more tokens to be
    /// parsed.
    ///
    /// LookAhead(0) returns the current token.
    [[nodiscard]]
    auto LookAhead(usz n) -> Token*;

    void NextToken();

    template <typename... Args>
    [[deprecated("Please provide a diagnostic ID")]]
    auto Error(fmt::format_string<Args...> fmt, Args&&... args) -> Diag {
        return Diag::Error(context, tok.location, fmt, std::forward<Args>(args)...);
    }

    template <typename... Args>
    [[deprecated("Please provide a diagnostic ID")]]
    auto Warning(fmt::format_string<Args...> fmt, Args&&... args) -> Diag {
        return Diag::Warning(context, tok.location, fmt, std::forward<Args>(args)...);
    }

    template <typename... Args>
    [[deprecated("Please provide a diagnostic ID")]]
    auto Note(fmt::format_string<Args...> fmt, Args&&... args) -> Diag {
        return Diag::Note(context, tok.location, fmt, std::forward<Args>(args)...);
    }

    template <typename... Args>
    auto Error(enum ErrorId id, fmt::format_string<Args...> fmt, Args&&... args) -> Diag {
        return Diag::Error(context, tok.location, error_id_strings.at(+id).second, fmt, std::forward<Args>(args)...);
    }

    template <typename... Args>
    auto Warning(enum ErrorId id, fmt::format_string<Args...> fmt, Args&&... args) -> Diag {
        return Diag::Warning(context, tok.location, error_id_strings.at(+id).second, fmt, std::forward<Args>(args)...);
    }

    template <typename... Args>
    auto Note(enum ErrorId id, fmt::format_string<Args...> fmt, Args&&... args) -> Diag {
        return Diag::Note(context, tok.location, error_id_strings.at(+id).second, fmt, std::forward<Args>(args)...);
    }

public:
    [[nodiscard]]
    static auto IsIdentStart(u32 c) -> bool {
        if (IsSpace(c) or IsNonCharacter(c))
            return false;

        return c > 127 or IsAlpha(c)
            or c == '$' or c == '_'
            or c == '?';
    }
    // Search Terms: delimiter
    [[nodiscard]]
    static auto IsIdentContinue(u32 c) -> bool {
        /// Note: '!' is *not* a start character so `!foo` still gets
        /// parsed as `!` + `foo`, but `foo!` gets parsed as one token.

        // What it isn't
        if (IsSpace(c) or IsNonCharacter(c))
            return false;

        // What it is
        return c > 127 or IsAlphaNumeric(c)
            or c == '!' or c == '$'
            or c == '&' or c == '+'
            or c == '-' or c == '/'
            or c == '?' or c == '@'
            or c == '^' or c == '_'
            or c == '|' or c == '~';
    }
};
} // namespace lcc::glint

#endif // GLINT_LEXER_HH
