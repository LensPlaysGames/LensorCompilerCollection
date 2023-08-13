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

    int speculative_parse_stack = 0;
    usz speculative_look_ahead = 0;

public:
    static std::unique_ptr<Module> Parse(Context* context, File* file);

private:
    friend struct ScopeRAII;

    /// RAII helper for pushing and popping scopes.
    struct ScopeRAII {
        Parser* parser;
        Scope* scope;

        ScopeRAII(Parser* parser)
            : parser(parser), scope(new(*parser) Scope(parser->CurrScope())) {
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

    /// RAII helper for pushing and popping speculative parse states.
    struct SpeculativeRAII {
        Parser* parser;
        bool active;
        LayeToken tok{};

        SpeculativeRAII(Parser* parser)
            : parser(parser), active(true) {
            if (parser->speculative_parse_stack == 0)
                tok = parser->tok;
            parser->speculative_parse_stack++;
        }

        SpeculativeRAII(const SpeculativeRAII&) = delete;
        SpeculativeRAII operator=(const SpeculativeRAII&) = delete;

        SpeculativeRAII(SpeculativeRAII&& other) noexcept
            : parser(other.parser), active(other.active), tok(other.tok) {
            other.active = false;
        }

        SpeculativeRAII& operator=(SpeculativeRAII&& other) noexcept {
            if (this == &other) return *this;
            parser = other.parser;
            active = other.active;
            tok = other.tok;
            other.active = false;
            return *this;
        }

        ~SpeculativeRAII() {
            if (active) {
                LCC_ASSERT(parser->speculative_parse_stack > 0);
                parser->speculative_parse_stack--;
                if (parser->speculative_parse_stack <= 0) {
                    LCC_ASSERT(tok.kind != TokenKind::Invalid);
                    parser->tok = tok;
                    parser->speculative_look_ahead = 0;
                }
            }
        }
    };

    auto EnterScope() { return ScopeRAII(this); }
    auto EnterSpeculativeParse() { return SpeculativeRAII(this); }

    Parser(Context* context, File* file, Module* module)
        : lexer(Lexer{context, file}), context(context), module(module) {}

    /// Get the current scope.
    auto CurrScope() -> Scope* { return scope_stack.back(); }

    /// True if any speculative parse state is enabled, false otherwise.
    bool IsInSpeculativeParse() const { return speculative_parse_stack > 0; }

    auto PeekToken(usz ahead = 1) {
        LCC_ASSERT(ahead >= 1, "Peek look-ahead indexing starts at 1.");
        while (look_ahead.size() < ahead) {
            LayeToken aheadToken{};
            lexer.ReadToken(aheadToken);
            look_ahead.push_back(aheadToken);
        }

        return look_ahead[ahead - 1];
    }

    /// Read the next token into tok.
    auto NextToken() {
        if (IsInSpeculativeParse()) {
            speculative_look_ahead++;
            return PeekToken(speculative_look_ahead);
        }

        if (not look_ahead.empty()) {
            tok = look_ahead[0];
            // pop from the front of the vector
            look_ahead.erase(look_ahead.begin());
        } else lexer.ReadToken(tok);
        return tok;
    }

    bool IsAtEof() const {
        if (IsInSpeculativeParse()) {
            LCC_ASSERT(speculative_look_ahead <= look_ahead.size());
            return look_ahead[speculative_look_ahead - 1].kind == TokenKind::Eof;
        }

        return tok.kind == TokenKind::Eof;
    }

    /// Check if we’re at one of a set of tokens.
    [[nodiscard]] auto At(auto... tks) { return ((tok.kind == tks) or ...); }

    /// Like At(), but consume the token if it matches.
    bool Consume(auto... tks) {
        if (At(tks...)) {
            NextToken();
            return true;
        }
        return false;
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

    Type* TryParseType(bool allocate);
    bool SpeculativeParseType() { return TryParseType(false); }
    Type* ParseType() { return TryParseType(true); }

    static isz BinaryOperatorPrecedence(TokenKind tokenKind);

    friend Scope;
    friend Statement;
    friend Expr;
};
} // namespace lcc::laye

#endif // LAYE_PARSER_HH
