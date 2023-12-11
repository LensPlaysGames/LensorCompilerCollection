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
    LayeContext* laye_context;
    Module* module;

    LayeToken tok{};
    Location last_location{};

    std::vector<LayeToken> look_ahead{};
    std::vector<Scope*> scope_stack{};

    int speculative_parse_stack = 0;
    usz speculative_look_ahead = 0;
    
    usz template_parse_stack = 0;

    bool will_attempt_to_parse_template_args = false;

public:
    static auto Parse(LayeContext* laye_context, File& file) -> Module*;

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

        LayeToken tok;
        Location last_location;
        usz speculative_look_ahead;

        SpeculativeRAII(Parser* parser)
            : parser(parser), active(true), tok(parser->tok), last_location(parser->last_location), speculative_look_ahead(parser->speculative_look_ahead) {
            parser->speculative_parse_stack++;
        }

        SpeculativeRAII(const SpeculativeRAII&) = delete;
        SpeculativeRAII operator=(const SpeculativeRAII&) = delete;

        SpeculativeRAII(SpeculativeRAII&& other) noexcept
            : parser(other.parser), active(other.active), tok(other.tok), last_location(other.last_location), speculative_look_ahead(other.speculative_look_ahead) {
            other.active = false;
        }

        SpeculativeRAII& operator=(SpeculativeRAII&& other) noexcept {
            if (this == &other) return *this;
            parser = other.parser;
            active = other.active;
            tok = other.tok;
            last_location = other.last_location;
            speculative_look_ahead = other.speculative_look_ahead;
            other.active = false;
            return *this;
        }

        ~SpeculativeRAII() {
            if (active) {
                LCC_ASSERT(parser->speculative_parse_stack > 0);
                parser->speculative_parse_stack--;

                LCC_ASSERT(tok.kind != TokenKind::Invalid);
                parser->tok = tok;
                parser->speculative_look_ahead = speculative_look_ahead;
            }
        }
    };

    /// RAII helper for pushing and popping template parse states.
    struct TemplateRAII {
        Parser* parser;
        bool active;

        TemplateRAII(Parser* parser)
            : parser(parser), active(true) {
            parser->template_parse_stack++;
        }

        TemplateRAII(const TemplateRAII&) = delete;
        TemplateRAII operator=(const TemplateRAII&) = delete;

        TemplateRAII(TemplateRAII&& other) noexcept
            : parser(other.parser), active(other.active) {
            other.active = false;
        }

        TemplateRAII& operator=(TemplateRAII&& other) noexcept {
            if (this == &other) return *this;
            parser = other.parser;
            active = other.active;
            other.active = false;
            return *this;
        }

        ~TemplateRAII() {
            if (active) {
                LCC_ASSERT(parser->template_parse_stack > 0);
                parser->template_parse_stack--;
            }
        }
    };

    auto EnterScope() { return ScopeRAII(this); }

    ///
    /// Notes on properly implementing speculative parsing routines:
    ///
    /// The simplest way to implement a speculative parsing routine is with
    /// three separate functions: One which takes in the flag of whether or
    /// not speculative parsing is requested, one which calls that routine with
    /// speculative parsing set to true, and one which calls that routine with
    /// speculative parsing set to false. The latter two functions are merely for
    /// convenience. The actual parsing routine should return a Result<T*>, where T*
    /// is the type that should be parsed by this routine when speculative parsing
    /// is not enabled.
    ///     The semantics of these speculative parsing routines that should be obeyed
    /// are as follows:
    ///     - In the event of a fatal error during speculative parsing, one in which
    ///       it is reasonably assumed that the parse would outright fail (and is not
    ///       a trivially ignored error), then that error should be returned to the caller
    ///       through the Result<T*> type.
    ///     - In the event of a non-fatal error during speculative parsing, that error
    ///       should simply not be generated (or be suppressed, these are semantically
    ///       similar cases) and the parse should continue. Only when a speculative parse
    ///       has completed or failed fatally should the routine return. When completing
    ///       without fatal errors, a `Result<T*>:Null()` result should be returned to indicate
    ///       the lack of fatal errors and indicate that a full parse is acceptable.
    ///     - When parsing without a speculative parse mode enabled, parsing should continue
    ///       routinely as any other parsing routine would.
    ///

    auto EnterSpeculativeParse() { return SpeculativeRAII(this); }
    auto EnterTemplateParse() { return TemplateRAII(this); }

    Parser(LayeContext* laye_context, File* file, Module* module)
        : lexer(Lexer{laye_context->context(), file}), file(file), context(laye_context->context()), laye_context(laye_context), module(module) {}

    /// Get the current scope.
    auto CurrScope() -> Scope* { return scope_stack.back(); }
    auto CurrLocation() { return tok.location; }
    auto GetLocation(Location start) const { return Location{start, last_location}; }
    bool IsLocationImmediatelyFollowing() const { return tok.location.pos == last_location.pos + last_location.len; }

    /// True if any speculative parse state is enabled, false otherwise.
    bool IsInSpeculativeParse() const { return speculative_parse_stack > 0; }
    bool IsInTemplateParse() const { return template_parse_stack > 0; }

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
        last_location = tok.location;
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

    bool ConsumeTemplateClose() {
        LCC_ASSERT(IsInTemplateParse());

        if (Consume(TokenKind::Greater)) return true;
        if (not At(TokenKind::GreaterGreater)) return false;

        tok.kind = TokenKind::Greater;
        tok.location.pos++;
        tok.location.len--;
        return true;
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

    auto TryParseTemplateArguments(bool allocate) -> Result<std::vector<Expr*>>;
    auto SpeculativeParseTemplateArguments() -> bool {
        LCC_ASSERT(IsInSpeculativeParse());
        auto result = TryParseTemplateArguments(false);
        if (result) return true;
        auto diag = result.diag();
        diag.suppress();
        return false;
    }
    auto ParseTemplateArguments() {
        LCC_ASSERT(not IsInSpeculativeParse());
        return TryParseTemplateArguments(true);
    }

    auto TryParseNameOrPath(
        bool allocate,
        std::function<Expr*(Location location, std::string name, std::vector<Expr*> template_args)> name_ctor,
        std::function<Expr*(PathKind path_kind, std::vector<std::string> names, std::vector<Location> locations, std::vector<Expr*> template_args)> path_ctor
    ) -> Result<Expr*>;

    auto TryParseDecl() -> Result<Decl*>;
    auto ParseDecl() -> Result<Decl*>;
    auto ParseDeclOrStatement() -> Result<Statement*>;
    auto ParseStatement(bool consumeSemi = true) -> Result<Statement*>;
    auto ParseBlockStatement(ScopeRAII sc) -> Result<BlockStatement*>;

    auto TryParseTemplateParams(bool allocate) -> Result<std::vector<NamedDecl*>>;
    bool SpeculativeParseTemplateParams() {
        LCC_ASSERT(IsInSpeculativeParse());
        auto result = TryParseTemplateParams(false);
        if (result) return true;
        auto diag = result.diag();
        diag.suppress();
        return false;
    }
    auto MaybeParseTemplateParams() {
        LCC_ASSERT(not IsInSpeculativeParse());
        return TryParseTemplateParams(true);
    }

    auto ParseImportDecl(bool is_export) -> Result<ImportHeader*>;

    auto TryParseTypeContinue(Type* type, bool allocate, bool allow_functions = true) -> Result<Type*>;
    auto TryParseType(bool allocate, bool allow_functions = true) -> Result<Type*>;
    bool SpeculativeParseType() {
        LCC_ASSERT(IsInSpeculativeParse());
        auto result = TryParseType(false);
        if (result) return true;
        auto diag = result.diag();
        diag.suppress();
        return false;
    }
    auto ParseType() {
        LCC_ASSERT(not IsInSpeculativeParse());
        return TryParseType(true);
    }

    auto ParseStruct(std::vector<DeclModifier> mods = {}) -> Result<StructDecl*>;
    auto ParseAlias(std::vector<DeclModifier> mods = {}, bool is_strict = false) -> Result<AliasDecl*>;

    auto ParseConstructorBody() -> Result<std::vector<CtorFieldInit>>;
    auto ParsePrimaryExprContinue(Expr* expr) -> Result<Expr*>;
    auto ParsePrimaryIdentExprContinue(Expr* expr) -> Result<Expr*>;
    auto ParsePrimaryExpr() -> Result<Expr*>;
    auto ParseBinaryExpr(Expr* lhs, int precedence = 0) -> Result<Expr*>;
    auto ParseExpr() -> Result<Expr*>;

    bool IsBinaryOperatorWithPrecedence(int precedence, int& next_precedence);

    static OperatorKind AssignOperatorKind(TokenKind tokenKind);
    static OperatorKind BinaryOperatorKind(TokenKind tokenKind);

    friend Scope;
    friend SemaNode;
};
} // namespace lcc::laye

#endif // LAYE_PARSER_HH