#ifndef INTERCEPT_PARSER_HH
#define INTERCEPT_PARSER_HH

#include <intercept/ast.hh>
#include <intercept/lexer.hh>
#include <lcc/context.hh>
#include <lcc/syntax/lexer.hh>
#include <lcc/utils.hh>
#include <lcc/utils/result.hh>

namespace lcc::intercept {
class Parser : public Lexer {
    using Tk = TokenKind;
    using ExprResult = Result<Expr*>;

    std::vector<Scope*> scope_stack{};
    std::unique_ptr<Module> mod{};

public:
    static std::unique_ptr<Module> Parse(Context* context, File& file);

private:
    friend struct ScopeRAII;

    /// RAII helper for pushing and popping scopes.
    struct ScopeRAII {
        Parser* parser;
        Scope* scope;

        ScopeRAII(Parser* parser)
            : parser(parser), scope(new(*parser->mod) Scope(parser->CurrScope())) {
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

    Parser(Context* context, File* file)
        : Lexer(context, file) {
        mod = std::make_unique<Module>(file);
    }

    /// Check if we’re at one of a set of tokens.
    [[nodiscard]] auto At(auto... tks) { return ((tok.kind == tks) or ...); }

    /// Check if we’re at the start of an expression.
    bool AtStartOfExpression() { return MayStartAnExpression(tok.kind); }

    /// Like At(), but consume the token if it matches.
    bool Consume(TokenKind tk) {
        if (At(tk)) {
            NextToken();
            return true;
        }
        return false;
    }

    /// Get the current scope.
    auto CurrScope() -> Scope* { return scope_stack.back(); }

    /// Discard semicolons until we reach a non-semicolon token.
    void DiscardSemicolons() {
        while (At(Tk::Semicolon)) NextToken();
    }

    /// Issue an error.
    template <typename... Args>
    Diag Error(Location where, fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag::Error(context, where, fmt, std::forward<Args>(args)...);
    }

    /// Issue an error at the location of the current token.
    using Lexer::Error;

    /// Check if a token kind may start an expression.
    bool MayStartAnExpression(Tk kind);

    auto ParseBlock() -> Result<BlockExpr*>;
    auto ParseCallExpr(Expr* callee) -> Result<CallExpr*>;
    auto ParseDecl(std::string ident, Location location) -> ExprResult;
    auto ParseDeclRest(std::string ident, Location location) -> ExprResult;
    auto ParseExpr(isz current_precedence = 0) -> ExprResult;
    auto ParseExprInNewScope() -> ExprResult;
    auto ParseFunctionAttributes() -> Result<std::vector<Attribute>>;
    auto ParseFunctionBody(Type* function_type, std::vector<VarDecl*>& param_decls, std::span<Attribute> attribs) -> Result<BlockExpr*>;
    auto ParseIdentExpr() -> Result<NamedRefExpr*>;
    auto ParseIfExpr() -> Result<IfExpr*>;
    auto ParseParamDecl() -> Result<FuncTypeParam>;
    auto ParsePreamble() -> Result<void>;
    auto ParseStructMember() -> Result<StructMember>;
    void ParseTopLevel();
    auto ParseType() -> Result<Type*>;
    auto ParseTypeDerived(Type* baseType) -> Result<Type*>;
    auto ParseTypeExpression(Type* type) -> ExprResult;
    auto ParseWhileExpr() -> Result<WhileExpr*>;

    /// Synchronise on semicolons and braces.
    void Synchronise();

    void EnsureHygenicDeclarationIfWithinMacro(std::string_view ident, Location location);

    static isz BinaryOperatorPrecedence(TokenKind tokenKind);
    static isz IsRightAssociative(TokenKind tokenKind);

    static void ApplyFunctionAttributes(Type* func, std::span<Attribute> attribs);
    static void ApplyStructAttributes(Type* func, std::span<Attribute> attribs);
};
} // namespace lcc::intercept

#endif // INTERCEPT_PARSER_HH
