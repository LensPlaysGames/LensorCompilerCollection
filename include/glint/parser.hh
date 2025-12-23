#ifndef GLINT_PARSER_HH
#define GLINT_PARSER_HH

#include <lcc/context.hh>
#include <lcc/diags.hh>
#include <lcc/syntax/lexer.hh>
#include <lcc/utils.hh>
#include <lcc/utils/result.hh>

#include <glint/ast.hh>
#include <glint/lexer.hh>

#include <functional>
#include <memory>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace lcc::glint {
class Parser : public Lexer {
    using Tk = TokenKind;
    using ExprResult = Result<Expr*>;

    std::vector<Scope*> scope_stack{};
    Module* mod{};

    /// The function we’re currently inside of.
    FuncDecl* curr_func{};

public:
    [[nodiscard]]
    static auto Parse(Context* context, std::string_view source) -> std::unique_ptr<Module>;
    [[nodiscard]]
    static auto Parse(Context* context, File& file) -> std::unique_ptr<Module>;

    /// @param starting_scope The parser's scope stack will be constructed from
    ///                       this scope and any and all of it's parent scopes.
    [[nodiscard]]
    static auto ParseFreestanding(
        Module&,
        Context*,
        File&,
        Scope* starting_scope
    ) -> Result<std::vector<lcc::glint::Expr*>>;

    // Don't do syntactic analysis, just lexical.
    [[nodiscard]]
    static auto GetTokens(Context* context, File& file) -> std::vector<GlintToken>;

private:
    static constexpr usz PrefixOperatorPrecedence = 10000;

    /// RAII helper for pushing and popping scopes.
    struct ScopeRAII {
        Parser* parser;
        Scope* scope;

        explicit ScopeRAII(Parser* parser_, Scope* parent = nullptr)
            : parser(parser_), scope(new (*parser->mod) Scope(parent ? parent : parser->CurrScope())) {
            LCC_ASSERT(parser);
            LCC_ASSERT(scope);
            parser->scope_stack.push_back(scope);
            scope->location() = parser->tok.location;
        }

        ScopeRAII(const ScopeRAII&) = delete;
        auto operator=(const ScopeRAII&) -> ScopeRAII = delete;

        ScopeRAII(ScopeRAII&& other) noexcept
            : parser(other.parser), scope(other.scope) {
            other.scope = nullptr;
        }

        auto operator=(ScopeRAII&& other) noexcept -> ScopeRAII& {
            if (this == &other) return *this;
            parser = other.parser;
            scope = other.scope;
            other.scope = nullptr;
            return *this;
        }

        ~ScopeRAII() {
            // The only way scope member is nullptr is if this has been moved from.
            if (scope) {
                scope->location() = {scope->location(), parser->tok.location};
                parser->scope_stack.pop_back();
            }
        }
    };

    Parser(Context* ctx, File* file) : Lexer(ctx, file) {}
    Parser(Context* ctx, std::string_view source) : Lexer(ctx, source) {}

    /// Check if we’re at one of a set of tokens.
    [[nodiscard]]
    static auto Is(GlintToken* tk, auto... tks) -> bool {
        return ((tk->kind == tks) or ...);
    }

    [[nodiscard]]
    auto At(auto... tks) -> bool {
        return Is(&tok, tks...);
    }

    /// Check if we’re at the start of an expression.
    auto AtStartOfExpression() -> bool;

    /// Like At(), but consume the token if it matches.
    auto Consume(auto... tks) -> bool {
        static_assert(
            sizeof...(tks),
            "\n\n"
            "    Consume() does nothing when called with no arguments.\n"
            "    You probably meant to call NextToken().\n"
        );
        if (At(tks...)) {
            NextToken();
            return true;
        }
        return false;
    }

    enum class ExpressionSeparator {
        Invalid = 0,
        Soft,
        Hard

    };
    auto AtExpressionSeparator(
        ExpressionSeparator filter = ExpressionSeparator::Invalid
    ) -> ExpressionSeparator {
        using E = ExpressionSeparator;
        if (
            // not want_to_use_filter and consumed
            (filter == E::Invalid and At(Tk::Comma))
            // want_to_use_filter and filter_matches and consumed
            or (filter != E::Invalid and filter == E::Soft and At(Tk::Comma))
        ) return E::Soft;

        if (
            (filter == E::Invalid and At(Tk::Semicolon))
            or (filter != E::Invalid and filter == E::Soft and At(Tk::Semicolon))
        ) return E::Hard;

        return E::Invalid;
    };
    auto ConsumeExpressionSeparator(
        ExpressionSeparator filter = ExpressionSeparator::Invalid
    ) -> ExpressionSeparator {
        using E = ExpressionSeparator;
        // if (
        //     (not want_to_use_filter and consume)
        //     or (want_to_use_filter and filter_matches and consume)
        // )

        if (
            // not want_to_use_filter and consumed
            (filter == E::Invalid and Consume(Tk::Comma))
            // want_to_use_filter and filter_matches and consumed
            or (filter != E::Invalid and filter == E::Soft and Consume(Tk::Comma))
        ) return E::Soft;

        if (
            // not want_to_use_filter and consumed
            (filter == E::Invalid and Consume(Tk::Semicolon))
            // want_to_use_filter and filter_matches and consumed
            or (filter != E::Invalid and filter == E::Hard and Consume(Tk::Semicolon))
        ) return E::Hard;

        return E::Invalid;
    };

    /// Get the current scope.
    auto CurrScope() -> Scope* { return scope_stack.back(); }

    /// Get the scope to use for declarations.
    auto DeclScope(bool for_local_var = false) -> Scope* {
        /// Local variables always go in the current scope since
        /// the global scope is *never* the current scope.
        if (for_local_var) return CurrScope();

        /// Globals at the top-level go in the global scope.
        return CurrScope() == TopLevelScope() ? GlobalScope() : CurrScope();
    }

    /// Issue an error.
    template <typename... Args>
    Diag Error(Location where, fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag::Error(context, where, fmt, std::forward<Args>(args)...);
    }

    template <typename... Args>
    Diag Error(fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag::Error(context, tok.location, fmt, std::forward<Args>(args)...);
    }

    /// Issue a warning.
    template <typename... Args>
    Diag Warning(Location where, fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag::Warning(context, where, fmt, std::forward<Args>(args)...);
    }

    template <typename... Args>
    Diag Warning(fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag::Warning(context, tok.location, fmt, std::forward<Args>(args)...);
    }

    template <typename... Args>
    Diag Note(Location where, fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag::Note(context, where, fmt, std::forward<Args>(args)...);
    }

    template <typename... Args>
    Diag Note(fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag::Note(context, tok.location, fmt, std::forward<Args>(args)...);
    }

    // Use this when you need a bunch of expressions until you get a closing
    // delimiter (like a bunch of expressions before a rbrace, or a rparen).
    // Does not care if the expressions are separated by hard separators.
    auto ParseExpressionsUntil(TokenKind until) -> Result<std::vector<Expr*>>;

    // Use this when you need a bunch of expressions until you get a hard
    // expression separator (like for parsing call arguments).
    auto ParseExpressionList() -> Result<std::vector<Expr*>>;

    auto ParseBlock() -> Result<BlockExpr*>;
    auto ParseBlock(ScopeRAII sc) -> Result<BlockExpr*>;
    auto ParseDecl() -> Result<Decl*>;
    auto ParseDeclRest(std::string ident, Location location, bool is_extern) -> Result<Decl*>;
    auto ParseExpr(isz current_precedence = 0) -> ExprResult;
    auto ParseExprInNewScope() -> ExprResult;
    auto ParseForExpr() -> Result<ForExpr*>;
    auto ParseRangedForExpr() -> Result<ForExpr*>;
    auto ParseFuncAttrs() -> Result<FuncType::Attributes>;
    auto ParseFuncBody(bool is_extern) -> Result<std::pair<Expr*, Scope*>>;
    auto ParseFuncDecl(std::string name, FuncType* type, bool is_extern) -> Result<FuncDecl*>;
    auto ParseFuncSig(Type* return_type) -> Result<FuncType*>;
    auto ParseIdentExpr() -> Result<Expr*>;
    auto ParseIfExpr() -> Result<IfExpr*>;
    auto ParsePreamble(File* f) -> Result<std::unique_ptr<Module>>;
    auto ParseStructType() -> Result<std::variant<StructType*, TemplatedStructType*>>;
    auto ParseStructMembers() -> Result<std::vector<StructType::Member>>;
    auto ParseEnumType() -> Result<EnumType*>;
    auto ParseUnionType() -> Result<UnionType*>;
    auto ParseSumType() -> Result<SumType*>;
    void ParseTopLevel();
    auto ParseType(isz current_precedence = 0) -> Result<Type*>;
    auto ParseWhileExpr() -> Result<WhileExpr*>;
    auto ParseTemplateParameters() -> Result<std::vector<TemplateExpr::Param>>;

    // Synchronise is normally used when a syntax error occurs, and attempts
    // to get to a position that is known to be a good enough place to start
    // parsing from again. For Glint, this means the hard expression separator
    // ';', or the closing brace of a block.
    void Synchronise();

    /// Get the global scope.
    auto GlobalScope() -> Scope* { return scope_stack[0]; }

    /// Get the scope for local top-level variables.
    /// This is different from the global scope, as a Glint source file is
    /// entirely within `main`.
    auto TopLevelScope() -> Scope* { return scope_stack[1]; }

    // Doesn't do any parsing, just collects tokens.
    [[nodiscard]]
    std::vector<GlintToken> JustGetTokens();

    friend Scope;
    friend ScopeRAII;
    friend Lexer;
};

} // namespace lcc::glint

#endif // GLINT_PARSER_HH
