#ifndef INTERCEPT_PARSER_HH
#define INTERCEPT_PARSER_HH

#include <intercept/ast.hh>
#include <lcc/context.hh>
#include <lcc/syntax/lexer.hh>
#include <lcc/utils.hh>

namespace lcc::intercept {
class Parser : public syntax::Lexer<InterceptToken> {
    InterceptToken current_token;

    std::vector<Scope*> scope_stack;

public:
    static Module* Parse(Context* context, File& file);

private:
    void NextChar();
    void NextIdentifier();
    void HandleIdentifier();
    void NextString();
    void ParseNumber(int base);
    void NextNumber();
    void NextToken();
    void NextMacro();
    void ExpandMacro(Macro* m);

    auto CurrScope() const { return scope_stack.back(); }

    void Consume(TokenKind tokenKind);

    auto ParseExpr(isz current_precedence = 0) -> Expr*;
    auto ParseBlock(bool create_new_scope) -> BlockExpr*;
    auto ParseIfExpr() -> IfExpr*;
    auto ParseWhileExpr() -> WhileExpr*;
    auto ParseCallExpr() -> CallExpr*;
    auto ParseFunctionAttributes() -> std::vector<Attribute>;
    auto ParseFunctionBody(Type* function_type, std::vector<VarDecl*>& param_decls, std::span<Attribute> attribs) -> BlockExpr*;
    auto ParseTypeExpression(Type* type) -> Expr*;
    auto ParseParamDecl() -> FuncTypeParam;
    auto ParseStructMember() -> StructMember;
    auto ParseTypeDerived(Type* baseType) -> Type*;
    auto ParseType() -> Type*;
    auto ParseDecl(std::string ident, Location location) -> Expr*;
    auto ParseDeclRest(std::string ident, Location location) -> Expr*;
    auto ParseIdentExpr() -> NamedRefExpr*;

    void EnsureHygenicDeclarationIfWithinMacro(std::string_view ident, Location location);

    static isz BinaryOperatorPrecedence(TokenKind tokenKind);
    static isz IsRightAssociative(TokenKind tokenKind);

    static void ApplyFunctionAttributes(Type* func, std::span<Attribute> attribs);
    static void ApplyStructAttributes(Type* func, std::span<Attribute> attribs);
};
} // namespace lcc::intercept

#endif // INTERCEPT_PARSER_HH
