#ifndef LAYE_PARSER_HH
#define LAYE_PARSER_HH

#include <laye/ast.hh>
#include <laye/lexer.hh>
#include <lcc/utils.hh>

namespace lcc::laye {
class Parser {
    Lexer lexer;
    File* file;
    Context* context;
    Module* module;

    LayeToken tok{};
    std::vector<LayeToken> look_ahead{};
    std::vector<Scope*> scope_stack{};

    int speculative_parse_stack = 0;
    usz speculative_look_ahead = 0;

public:
    static std::unique_ptr<Module> Parse(Context* context, File& file);

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
        : lexer(Lexer{context, file}), file(file), context(context), module(module) {}

    /// Get the current scope.
    auto CurrScope() -> Scope* { return scope_stack.back(); }
    auto CurrLocation() { return tok.location; }
    auto GetLocation(u32 start) const { return Location{
        start,
        (u16) (tok.location.pos - start),
        (u16) file->file_id()};
    }

    /// True if any speculative parse state is enabled, false otherwise.
    bool IsInSpeculativeParse() const { return speculative_parse_stack > 0; }

    auto PeekToken(usz ahead = 1, bool include_spec = true) {
        LCC_ASSERT(ahead >= 1, "Peek look-ahead indexing starts at 1.");
        if (include_spec) ahead += speculative_look_ahead;
        
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
            tok = PeekToken(speculative_look_ahead, false);
        } else if (not look_ahead.empty()) {
            tok = look_ahead[0];
            // pop from the front of the vector
            look_ahead.erase(look_ahead.begin());
        } else lexer.ReadToken(tok);
        return tok;
    }

    /// Check if we’re at one of a set of tokens.
    [[nodiscard]] auto At(auto... tks) { return ((tok.kind == tks) or ...); }
    [[nodiscard]] auto PeekAt(usz ahead, auto... tks) { return ((PeekToken(ahead).kind == tks) or ...); }

    /// Like At(), but consume the token if it matches.
    bool Consume(auto... tks) {
        if (At(tks...)) {
            NextToken();
            return true;
        }
        return false;
    }

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

    /// Issue an error.
    template <typename... Args>
    Diag Error(fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag::Error(context, CurrLocation(), fmt, std::forward<Args>(args)...);
    }

    void ExpectSemiColon() {
        if (not Consume(TokenKind::SemiColon)) {
            Error(CurrLocation(), "Expected ';'");
        }
    }

    /// Synchronise on semicolons and braces.
    void Synchronise();

    auto ParseTopLevel() -> Result<Decl*>;

    auto TryParseNameOrPath(
        bool allocate,
        std::function<Expr*(Location location, std::string name)> name_ctor,
        std::function<Expr*(PathKind path_kind, std::vector<std::string> names, std::vector<Location> locations)> path_ctor
    ) -> Result<Expr*>;

    auto TryParseDecl() -> Result<Decl*>;
    auto ParseDecl() -> Result<Decl*>;
    auto ParseDeclOrStatement() -> Result<std::variant<Statement*, Decl*>>;

    auto TryParseTemplateParams(bool allocate) -> Result<std::vector<TemplateParam>>;
    bool SpeculativeParseTemplateParams() {
        LCC_ASSERT(IsInSpeculativeParse());
        return TryParseTemplateParams(false).is_value();
    }
    auto MaybeParseTemplateParams() {
        LCC_ASSERT(not IsInSpeculativeParse());
        return TryParseTemplateParams(true);
    }

    auto ParseImportDecl(bool is_export) -> Result<ImportHeader*>;

    auto TryParseTypeContinue(Type* type, bool allocate, bool allowFunctions = true) -> Result<Type*>;
    auto TryParseType(bool allocate, bool allowFunctions = true) -> Result<Type*>;
    bool SpeculativeParseType() {
        LCC_ASSERT(IsInSpeculativeParse());
        return TryParseType(false).is_value();
    }
    auto ParseType() {
        LCC_ASSERT(not IsInSpeculativeParse());
        return TryParseType(true);
    }

    auto ParseExpr() -> Result<Expr*>;

    static isz BinaryOperatorPrecedence(TokenKind tokenKind);

    static OperatorKind UnaryOperatorKind(TokenKind tokenKind);
    static OperatorKind BinaryOperatorKind(TokenKind tokenKind);

    friend Scope;
    friend Statement;
    friend Expr;
};
} // namespace lcc::laye

#endif // LAYE_PARSER_HH
