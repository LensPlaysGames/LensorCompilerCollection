#include <intercept/parser.hh>
#include <lcc/utils/macros.hh>
#include <lcc/utils/rtti.hh>

/// Cute trick for monad binding.
#define bind *this->*&Parser::
namespace {
auto operator->*(lcc::intercept::Parser& p, auto member_function) {
    return [p = std::addressof(p), member_function](auto&&... args) {
        return std::invoke(member_function, p, std::forward<decltype(args)>(args)...);
    };
}
}

auto lcc::intercept::Parser::Parse(Context* context, File& file) -> std::unique_ptr<Module> {
    Parser parser(context, &file);

    /// Parse preamble. This also creates the module.
    if (not parser.ParsePreamble()) return {};

    /// Parse file.
    parser.ParseTopLevel();

    /// Return nullptr on error.
    return context->has_error() ? std::unique_ptr<Module>{} : std::move(parser.mod);
}

/// <expr-block> ::= "{" { <expression> } "}"
auto lcc::intercept::Parser::ParseBlock() -> Result<BlockExpr*> {
    ScopeRAII sc{this};
    auto loc = tok.location;
    LCC_ASSERT(Consume(Tk::LBrace), "ParseBlock called while not at '{{'");

    /// Parse expressions.
    std::vector<Expr*> exprs;
    while (not At(Tk::RBrace)) {
        auto expr = ParseExpr();
        if (not expr) return expr.diag();
        exprs.push_back(expr.value());
    }

    /// Yeet "}".
    if (not Consume(Tk::RBrace)) return Error("Expected }}");
    return new (*mod) BlockExpr(std::move(exprs), loc);
}

/// <expr-call> ::= <expression> "(" { <expression> [ "," ] } ")"
auto lcc::intercept::Parser::ParseCallExpr(Expr* callee) -> Result<CallExpr*> {
    /// Yeet "(".
    auto loc = tok.location;
    LCC_ASSERT(Consume(Tk::LParen), "ParseCallExpr called while not at '('");

    /// Parse args.
    std::vector<Expr*> args;
    while (not At(Tk::RParen)) {
        if (auto arg = ParseExpr(); not arg) return arg.diag();
        else args.push_back(arg.value());
        Consume(Tk::Comma);
    }

    /// Yeet ")".
    if (not Consume(Tk::RParen)) return Error("Expected )");
    return new (*mod) CallExpr(callee, std::move(args), loc);
}

/// See grammar.bnf for a list of productions handled by this rule.
/// <expression> ::= ...
auto lcc::intercept::Parser::ParseExpr(isz current_precedence) -> ExprResult {
    auto lhs = ExprResult::Null();

    /// See below.
    const auto start_token = tok.kind;

    /// Parse the LHS.
    switch (tok.kind) {
        default: return Error("Expected expression");
        case Tk::Gensym: LCC_ASSERT(false, "Gensym token in parser");

        /// AST node bound by macro.
        case Tk::Expression:
            LCC_ASSERT(tok.expression);
            lhs = tok.eval_once ? tok.expression : Expr::Clone(*mod, tok.expression);
            NextToken();
            break;

        /// TODO(Sirraide): Left off here.
    }

    /// Some places in the grammar are ambiguous and require us to
    /// know whether we’re at the start of an expression or not. To
    /// make sure we don’t accidentally forget to add something to
    /// MayStartAnExpression(), check that, if we have just parsed
    /// an expression, its first token is recognised as the start
    /// of an expression.
    LCC_ASSERT(
        MayStartAnExpression(start_token),
        "Add {} to lcc::intercept::Parser::MayStartAnExpression()",
        ToString(start_token)
    );
}

auto lcc::intercept::Parser::ParseExprInNewScope() -> ExprResult {
    ScopeRAII sc{this};
    return ParseExpr();
}

/// <expr-if> ::= IF <expression> <expression> [ ELSE <expression> ]
auto lcc::intercept::Parser::ParseIfExpr() -> Result<IfExpr*> {
    /// Yeet "if".
    auto loc = tok.location;
    LCC_ASSERT(Consume(Tk::If), "ParseIf called while not at 'if'");

    /// Parse condition, then, and else.
    auto cond = ParseExpr();
    auto then = ParseExprInNewScope();
    auto else_ = ExprResult::Null();
    if (Consume(Tk::Else)) else_ = ParseIfExpr();
    if (IsError(cond, then, else_)) return Diag();
    return new (*mod) IfExpr(cond.value(), then.value(), else_.value(), loc);
}

/// <preamble> ::= [ <module-declaration> ] { <import-declaration> | ";" }
auto lcc::intercept::Parser::ParsePreamble() -> Result<void> {
    /// Parse module name.
    if (At(Tk::Ident) and tok.text == "module" and not tok.artificial) {
        NextToken(); /// Yeet "module".
        if (not At(Tk::Ident)) return Error("Expected module name");
        mod->set_logical_module(tok.text);
        NextToken(); /// Yeet module name.
    }

    DiscardSemicolons();

    /// Parse imports.
    while (At(Tk::Ident) and tok.text == "import" and not tok.artificial) {
        NextToken(); /// Yeet "import".
        if (not At(Tk::Ident)) return Error("Expected module name after import");

        /// Add the module to be loaded later.
        mod->add_import(tok.text);
        NextToken(); /// Yeet module name.
        DiscardSemicolons();
    }

    return {};
}

/// <file> ::= <preamble> { <expression> | ";" }
void lcc::intercept::Parser::ParseTopLevel() {
    for (;;) {
        DiscardSemicolons();

        /// Stop if we’re at end of file.
        if (At(Tk::Eof)) break;

        /// Parse a top-level expression.
        auto expr = ParseExpr();
        if (expr) mod->add_top_level_expr(expr);

        /// Synchronise on semicolons and braces in case of an error.
        else Synchronise();
    }
}

/// <expr-while> ::= WHILE <expression> <expression>
auto lcc::intercept::Parser::ParseWhileExpr() -> Result<WhileExpr*> {
    /// Yeet "while".
    auto loc = tok.location;
    LCC_ASSERT(Consume(Tk::While), "ParseWhile called while not at 'while'");

    /// Parse condition and body.
    auto cond = ParseExpr();
    auto body = ParseExprInNewScope();
    if (IsError(cond, body)) return Diag();
    return new (*mod) WhileExpr(cond.value(), body.value(), loc);
}

void lcc::intercept::Parser::Synchronise() {
    while (not At(Tk::Semicolon, Tk::LBrace, Tk::RBrace, Tk::Eof)) NextToken();
    NextToken();
}
