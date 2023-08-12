#ifndef LAYE_PARSER_HH
#define LAYE_PARSER_HH

#include <laye/ast.hh>
#include <laye/lexer.hh>
#include <lcc/utils.hh>

namespace lcc::laye {
class Parser {
    Lexer lexer;
    Context* context;
    Module* module;

    LayeToken tok{};
    std::vector<LayeToken> look_ahead{};
    std::vector<Scope*> scope_stack{};

public:
    static std::unique_ptr<Module> Parse(Context* context, File* file);

private:
    friend struct ScopeRAII;

    /// RAII helper for pushing and popping scopes.
    struct ScopeRAII {
        Parser* parser;
        Scope* scope;

        ScopeRAII(Parser* parser)
            : parser(parser), scope(new(*parser->module) Scope(parser->CurrScope())) {
            parser->scope_stack.push_back(scope);
        }

        ScopeRAII(const ScopeRAII&) = delete;
        ScopeRAII operator=(const ScopeRAII&) = delete;

        ScopeRAII(ScopeRAII&& other) noexcept
            : parser(other.parser), scope(other.scope) {
            other.scope = nullptr;
        }

        ScopeRAII& operator=(ScopeRAII&& other) noexcept {
            if (this == &other) return *this;
            parser = other.parser;
            scope = other.scope;
            other.scope = nullptr;
            return *this;
        }

        ~ScopeRAII() {
            if (scope) parser->scope_stack.pop_back();
        }
    };

    // NOTE(local): could maybe implement infinite speculative parsing
    //  through something like the following?
    //
    // bool is_speculative = false;
    // usz speculative_peek_index = 0;
    //
    // where we could even remove `is_speculative` in favor of just checking
    //  if `speculative_peek_index != 0`.
    // In speculative parsing mode, whenever a new token is requested, instead
    //  `PeekToken` is called with a successive value for `speculative_peek_index`
    //  each time, and the speculative parse must be committed to clear the peek queue
    //  and return to normal parsing.
    // This would allow us to *try* to parse something and roll it back if it fails
    //  without having to actually implement roll-back at a lexer level or by
    //  altering the lexer state manually.
    //
    // Usage could look like the following
    //
    // EnterSpeculativeMode(); // asserts that we aren't already in speculative mode
    // auto maybeTypeResult = ParseType();
    // if (maybeTypeResult /* and At(TokenKind::Ident) */) {
    //   CommittSpeculativeMode(); // flushes all peeked tokens, sets the parser to the
    //                            //  next real token
    //   // continue parsing from here
    // } else {
    //   RollbackSpeculativeParse();
    //   // continue parsing from somewhere else
    // }
    //
    // where that API can be wrapped in RAII type to enable easy scoping if desired.
    //
    // The downside to this is still that allocated types need to be freed properly,
    //  if memory bloat is a concern at least. Sytax types are allocated and stored
    //  within the syntax module to be disposed of at a later time, but this would
    //  require us to think about ahead-of-time freeing of teh syntax tree.
    //
    // So basically I don't think this is worth doing unless I absolutely can't
    //  manage to get unambiguous syntax nodes resolve in sema to work.

    Parser(Context* context, File* file, Module* module)
        : lexer(Lexer{context, file}), context(context), module(module) {}

    /// Get the current scope.
    auto CurrScope() -> Scope* { return scope_stack.back(); }

    /// Read the next token into tok.
    auto NextToken() {
        if (not look_ahead.empty()) {
            tok = look_ahead[0];
            // pop from the front of the vector
            look_ahead.erase(look_ahead.begin());
        } else lexer.ReadToken(tok);
        return tok;
    }

    /// Check if we’re at one of a set of tokens.
    [[nodiscard]] auto At(auto... tks) { return ((tok.kind == tks) or ...); }

    /// Like At(), but consume the token if it matches.
    bool Consume(TokenKind tk) {
        if (At(tk)) {
            NextToken();
            return true;
        }
        return false;
    }

    auto PeekToken(usz ahead = 1) {
        LCC_ASSERT(ahead >= 1, "Peek look-ahead indexing starts at 1.");
        while (look_ahead.size() < ahead) {
            LayeToken aheadToken{};
            lexer.ReadToken(aheadToken);
            look_ahead.push_back(aheadToken);
        }

        return look_ahead[ahead - 1];
    }

    /// Synchronise on semicolons and braces.
    void Synchronise();

    /// Issue a note.
    template <typename... Args>
    Diag Note(Location where, fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag::Note(context, where, fmt, std::forward<Args>(args)...);
    }

    /// Issue a warning.
    template <typename... Args>
    Diag Warning(Location where, fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag::Warning(context, where, fmt, std::forward<Args>(args)...);
    }

    /// Issue an error.
    template <typename... Args>
    Diag Error(Location where, fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag::Error(context, where, fmt, std::forward<Args>(args)...);
    }

    static isz BinaryOperatorPrecedence(TokenKind tokenKind);
};
} // namespace lcc::laye

#endif // LAYE_PARSER_HH
