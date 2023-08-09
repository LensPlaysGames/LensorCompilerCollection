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
    if (not parser.ParsePreamble(file)) return {};

    /// Parse file.
    parser.ParseTopLevel();

    /// Return nullptr on error.
    return context->has_error() ? std::unique_ptr<Module>{} : std::move(parser.mod);
}

/// <expr-block> ::= "{" { <expr> } "}"
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

/// <expr-call> ::= <expr> "(" { <expr> [ "," ] } ")"
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
/// <expr> ::= ...
auto lcc::intercept::Parser::ParseExpr(isz current_precedence) -> ExprResult {
    auto lhs = ExprResult::Null();

    /// Export a declaration.
    const auto Export = [&](ObjectDecl* decl) -> Result<ObjectDecl*> {
        decl->linkage(decl->linkage() == Linkage::Imported ? Linkage::Reexported : Linkage::Exported);
        mod->add_export(decl);
        return decl;
    };

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

        /// Exported declaration.
        case Tk::Export: {
            if (CurrScope() != GlobalScope())
                return Error("Exported declarations are only allowed at the top level");

            /// Parse and create the decl.
            NextToken();
            lhs = ParseDecl() >>= Export;
        } break;

        /// Expression that starts with an identifier.
        case Tk::Ident: lhs = ParseIdentExpr(); break;

        /// Builtin types.
        case Tk::ArbitraryInt:
        case Tk::Bool:
        case Tk::Byte:
        case Tk::IntegerKw:
        case Tk::Void:
            lhs = ParseType() >>= bind ParseTypeExpression;
            break;

        /// Better error message for this.
        case Tk::Else: return Error("Unexpected 'else'. Did you forget an 'if' somewhere?");

        /// Control expressions.
        case Tk::If: lhs = ParseIfExpr(); break;
        case Tk::For: lhs = ParseForExpr(); break;
        case Tk::While: lhs = ParseWhileExpr(); break;

        /// Return expression.
        case Tk::Return: {
            auto loc = tok.location;
            NextToken();

            /// If there is an expression, parse it; otherwise,
            /// this return expression has no operand.
            auto value = ExprResult::Null();
            if (AtStartOfExpression()) {
                value = ParseExpr();
                if (not value) return value.diag();
                loc = {loc, value.value()->location()};
            }

            lhs = new (*mod) ReturnExpr(value.value(), loc);
        } break;

        /// Block expression.
        case Tk::LBrace: lhs = ParseBlock(); break;

        /// Integer literal.
        case Tk::Number:
            lhs = new (*mod) IntegerLiteral(tok.integer_value, tok.location);
            NextToken();
            break;

        /// String literal.
        case Tk::String:
            lhs = new (*mod) StringLiteral(*mod, tok.text, tok.location);
            NextToken();
            break;

        /// Compound literal.
        case Tk::LBrack: {
            std::vector<Expr*> elements;
            NextToken(); /// Yeet "[".

            /// Parse the elements.
            while (not At(Tk::RBrack, Tk::Eof)) {
                auto element = ParseExpr();
                if (not element) return element.diag();
                elements.push_back(element.value());
                Consume(Tk::Comma);
            }

            /// Yeet "]".
            if (not Consume(Tk::RBrack)) return Error("Expected ]");
            lhs = new (*mod) CompoundLiteral(std::move(elements), tok.location);
        } break;

        /// Parenthesised expression or type.
        ///
        /// FIXME: We should allow types in parentheses if that is at
        ///        all possible. Come back to this once we know how to
        ///        handle the Tk::At case.
        case Tk::LParen: {
            NextToken();
            lhs = ParseExpr();
            if (not lhs) return lhs.diag();
            if (not Consume(Tk::RParen)) return Error("Expected )");
        } break;

        /// Ambiguous situation:
        ///  a := @foo()
        ///        ^
        /// Possible productions:
        ///   1. foo is a type, this is a lambda
        ///   2. foo is a function; this is a call+dereference.
        ///
        /// Parentheses do not help:
        ///  a := @(foo())
        ///         ^
        /// Possible productions:
        ///   1. foo is a type, this is a function pointer type.
        ///   2. foo is a function, this is a call+parens+dereference.
        ///
        /// TODO: Revisit this case once we know what expressions can
        ///       start with types.
        case Tk::At: LCC_ASSERT(false, "TODO");
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

    /// If the LHS is a diag, return.
    if (not lhs) return lhs.diag();

    /// LHS must not be null if we get here.
    LCC_ASSERT(
        lhs.value(),
        "Someone forgot to assign to `lhs` when parsing expr starting with {}",
        ToString(start_token)
    );
}

auto lcc::intercept::Parser::ParseExprInNewScope() -> ExprResult {
    ScopeRAII sc{this};
    return ParseExpr();
}

/// <expr-for> ::= FOR <expr> [ "," ] <expr> [ "," ] <expr> <expr>
auto lcc::intercept::Parser::ParseForExpr() -> Result<ForExpr*> {
    auto loc = tok.location;
    LCC_ASSERT(Consume(Tk::For), "ParseForExpr called while not at 'for'");

    /// Parse init, cond, increment, and body.
    ScopeRAII sc{this};
    auto init = ParseExpr();
    Consume(Tk::Comma);
    auto cond = ParseExpr();
    Consume(Tk::Comma);
    auto increment = ParseExpr();
    auto body = ParseExpr();

    /// Check for errors and create the expression.
    if (IsError(init, cond, increment, body)) return Diag();
    return new (*mod) ForExpr(*init, *cond, *increment, *body, loc);
}

/// <expr-if> ::= IF <expr> <expr> [ ELSE <expr> ]
auto lcc::intercept::Parser::ParseIfExpr() -> Result<IfExpr*> {
    /// Yeet "if".
    auto loc = tok.location;
    LCC_ASSERT(Consume(Tk::If), "ParseIf called while not at 'if'");

    /// Parse condition, then, and else.
    auto cond = ParseExpr();
    auto then = ParseExprInNewScope();
    auto else_ = ExprResult::Null();
    if (Consume(Tk::Else)) else_ = ParseExpr();
    if (IsError(cond, then, else_)) return Diag();
    return new (*mod) IfExpr(cond.value(), then.value(), else_.value(), loc);
}

/// <preamble> ::= [ <module-declaration> ] { <import-declaration> | ";" }
auto lcc::intercept::Parser::ParsePreamble(File& f) -> Result<void> {
    /// Parse module name and create the module.
    if (At(Tk::Ident) and tok.text == "module" and not tok.artificial) {
        NextToken(); /// Yeet "module".
        if (not At(Tk::Ident)) return Error("Expected module name");
        mod = std::make_unique<Module>(&f, tok.text, true);
        NextToken(); /// Yeet module name.
    }

    /// Create an executable module instead.
    else {
        mod = std::make_unique<Module>(&f, "", false);
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

/// <file> ::= <preamble> { <expr> | ";" }
void lcc::intercept::Parser::ParseTopLevel() {
    /// Set up the rest of the parser state.
    curr_func = mod->top_level_func();
    scope_stack.push_back(new (*mod) Scope(nullptr));

    /// Parse the file.
    for (;;) {
        DiscardSemicolons();

        /// Stop if we’re at end of file.
        if (At(Tk::Eof)) break;

        /// Parse a top-level expression.
        auto expr = ParseExpr();
        if (expr) mod->add_top_level_expr(expr.value());

        /// Synchronise on semicolons and braces in case of an error.
        else Synchronise();
    }
}

/// <expr-while> ::= WHILE <expr> <expr>
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
