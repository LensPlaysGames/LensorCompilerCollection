#include <intercept/parser.hh>
#include <lcc/diags.hh>
#include <lcc/utils/macros.hh>
#include <lcc/utils/rtti.hh>

#define bind LCC_BIND

namespace intc = lcc::intercept;

namespace {
/// Get the binary precedence of a token.
/// TODO: User-defined operators.
constexpr inline lcc::isz CallPrecedence = 100'000;
constexpr auto BinaryOrPostfixPrecedence(intc::TokenKind t) -> lcc::isz {
    using Tk = intc::TokenKind;
    switch (t) {
        case Tk::Dot:
            return 1'000'000'000;

        /// Call and subscript have higher precedence than unary operators.
        /// Note: Unary operator precedence is 10'000.
        case Tk::LBrack:
            return CallPrecedence + 1;

        case Tk::As:
        case Tk::AsBang:
            return 1'000;

        case Tk::Star:
        case Tk::Slash:
        case Tk::Percent:
            return 600;

        case Tk::Plus:
        case Tk::Minus:
            return 500;

        case Tk::Shl:
        case Tk::Shr:
            return 400;

        case Tk::Ampersand:
        case Tk::Pipe:
        case Tk::Caret:
            return 300;

        case Tk::Eq:
        case Tk::Ne:
        case Tk::Lt:
        case Tk::Gt:
        case Tk::Le:
        case Tk::Ge:
            return 200;

        case Tk::ColonEq:
        case Tk::ColonColon:
            return 100;

        /// Not an operator.
        default: return -1;
    }
}

/// Check if an operator is right-associative.
/// TODO: User-defined operators.
constexpr bool IsRightAssociative(intc::TokenKind t) {
    using Tk = intc::TokenKind;
    switch (t) {
        case Tk::Star:
        case Tk::Slash:
        case Tk::Percent:
        case Tk::Plus:
        case Tk::Minus:
        case Tk::Shl:
        case Tk::Shr:
        case Tk::Ampersand:
        case Tk::Pipe:
        case Tk::Caret:
        case Tk::Eq:
        case Tk::Ne:
        case Tk::Lt:
        case Tk::Gt:
        case Tk::Le:
        case Tk::Ge:
            return false;

        case Tk::ColonEq:
        case Tk::ColonColon:
            return true;

        /// Not an operator.
        default: return false;
    }
}

constexpr bool MayStartAnExpression(intc::TokenKind kind) {
    using Tk = intc::TokenKind;
    switch (kind) {
        case Tk::LParen:
        case Tk::LBrack:
        case Tk::LBrace:
        case Tk::Plus:
        case Tk::Minus:
        case Tk::Ampersand:
        case Tk::Tilde:
        case Tk::Exclam:
        case Tk::At:
        case Tk::Ident:
        case Tk::Number:
        case Tk::String:
        case Tk::If:
        case Tk::While:
        case Tk::Extern:
        case Tk::Static:
        case Tk::For:
        case Tk::Return:
        case Tk::Export:
        case Tk::Struct:
        case Tk::Lambda:
        case Tk::Expression:
            return true;

        case Tk::Invalid:
        case Tk::Eof:
        case Tk::RParen:
        case Tk::RBrack:
        case Tk::RBrace:
        case Tk::Comma:
        case Tk::Colon:
        case Tk::Semicolon:
        case Tk::Dot:
        case Tk::Star:
        case Tk::Slash:
        case Tk::Percent:
        case Tk::Pipe:
        case Tk::Caret:
        case Tk::Hash:
        case Tk::Shl:
        case Tk::Shr:
        case Tk::Eq:
        case Tk::Ne:
        case Tk::Lt:
        case Tk::Gt:
        case Tk::Le:
        case Tk::Ge:
        case Tk::ColonEq:
        case Tk::ColonColon:
        case Tk::Else:
        case Tk::Do:
        case Tk::Then:
        case Tk::As:
        case Tk::AsBang:
        case Tk::Type:
        case Tk::Void:
        case Tk::Byte:
        case Tk::Bool:
        case Tk::IntKw:
        case Tk::ArbitraryInt:
        case Tk::Gensym:
        case Tk::MacroArg:
            return false;
    }

    LCC_UNREACHABLE();
}

/// \brief Get the precedence level of a type qualifier.
///
/// The precedence levels for type qualifiers are (higher
/// precedence binds more tightly):
///
///    Function < Reference < Array < Pointer
///
/// The reason is that this avoids the most parentheses
/// because, that way, the most common use cases don’t
/// need them.
///
/// Arrays of references are invalid, so &i32[10] can only
/// really be a reference to an array. This means that arrays
/// should bind more tightly than references.
///
/// Arrays of pointers, however, are common. Pointers to arrays
/// should not be too common as one can just use a reference
/// instead in most cases. Thus, pointers should bind more
/// tightly than arrays.
///
/// Furthermore, function pointers and references are *very*
/// uncommon, but functions frequently return pointers and
/// references, and even arrays, so functions should have the
/// lowest precedence.
///
/// Putting these all together yields the precedence hierarchy
/// described above.
///
/// Examples:
/// \code
///     @i32[10]  === (@i32)[10]    ;; Array of pointers.
///     &i32[10]  === &(i32[10])    ;; Reference to array.
///     @i32()    === (@i32)()      ;; Function returning pointer.
///     &i32()    === (&i32)()      ;; Function returning reference.
///
///     ;; Uncommon, but useful for illustration purposes.
///     @&i32[10] === @(&i32)[10]   ;; Array of pointers to references.
///     &@i32[10] === &((@i32)[10]) ;; Reference to array of pointers.
/// \endcode
constexpr lcc::isz TypeQualifierPrecedence(intc::TokenKind t) {
    using Tk = intc::TokenKind;
    switch (t) {
        case Tk::At: return 400;
        case Tk::LBrack: return 300;
        case Tk::Ampersand: return 200;
        case Tk::LParen: return 100;
        default: return -1;
    }
}
} // namespace

bool intc::Parser::AtStartOfExpression() { return MayStartAnExpression(tok.kind); }

auto intc::Parser::Declare(std::string name, ObjectDecl* decl) -> Result<Decl*> {
    Scope* var_scope{};
    if (decl->linkage() == Linkage::LocalVar) {
        var_scope = CurrScope() == GlobalScope() ? TopLevelScope() : CurrScope();
    } else {
        var_scope = CurrScope() == TopLevelScope() ? GlobalScope() : CurrScope();
    }

    return var_scope->declare(context, std::move(name), decl);
}

auto intc::Parser::Parse(Context* context, File& file) -> std::unique_ptr<Module> {
    Parser parser(context, &file);

    /// Parse preamble. This also creates the module.
    if (not parser.ParsePreamble(file)) return {};

    /// Parse file.
    parser.ParseTopLevel();

    /// Return nullptr on error.
    return context->has_error() ? std::unique_ptr<Module>{} : std::move(parser.mod);
}

/// Creates a new scope and parses a block in that scope.
auto intc::Parser::ParseBlock() -> Result<BlockExpr*> {
    return ParseBlock({this});
}

/// <expr-block> ::= "{" { <expr> ";" } "}"
auto intc::Parser::ParseBlock(
    /// The only purpose of this parameter is to open a new scope
    /// for this block. Do NOT remove it, even if it appears unused.
    [[maybe_unused]] ScopeRAII sc
) -> Result<BlockExpr*> {
    auto loc = tok.location;
    LCC_ASSERT(Consume(Tk::LBrace), "ParseBlock called while not at '{{'");

    /// Parse expressions.
    std::vector<Expr*> exprs;
    while (not At(Tk::RBrace)) {
        if (Consume(Tk::Semicolon)) continue;
        auto expr = ParseExpr();
        if (not Consume(Tk::Semicolon)) Error("Expected ;");
        if (not expr) return expr.diag();
        exprs.push_back(expr.value());
    }

    /// Yeet "}".
    if (not Consume(Tk::RBrace)) return Error("Expected }}");
    return new (*mod) BlockExpr(std::move(exprs), loc);
}

/// Parse an object or type declaration.
///
/// <expr-decl>     ::= <storage-specs> IDENTIFIER <decl-rest>
/// <storage-specs> ::= [ EXPORT ] [ EXTERN ] [ STATIC ]
auto intc::Parser::ParseDecl() -> Result<Decl*> {
    auto is_export = Consume(Tk::Export);
    auto is_extern = Consume(Tk::Extern);
    auto is_static = Consume(Tk::Static);
    auto loc = tok.location;
    auto text = tok.text;
    if (not Consume(Tk::Ident)) return Error("Expected declaration");
    auto decl = ParseDeclRest(text, loc, is_extern, is_static);

    /// Apply storage specifiers.
    if (is_export) {
        /// Export a declaration.
        const auto Export = [&](Decl* decl) -> Result<Decl*> {
            /// Set linkage to exported if this has linkage.
            if (auto obj = cast<ObjectDecl>(decl)) {
                obj->linkage(obj->linkage() == Linkage::Imported ? Linkage::Reexported : Linkage::Exported);

                /// If static was also specified, issue a warning if extern wasn’t
                /// also specified, in which case we will have already issued a
                /// warning.
                if (is_static and not is_extern) Diag::Warning(
                    context,
                    obj->location(),
                    "Static specifier has no effect on this declaration"
                );
            }

            /// Add the declaration to the module’s export list.
            mod->add_export(decl);
            return decl;
        };

        /// Exported declaration.
        if (CurrScope() != GlobalScope())
            Error("Exported declarations are only allowed at the top level");

        decl = decl >>= Export;
    }

    return decl;
}

/// Parse everything after the identifier in an object declaration.
///
/// <decl-function> ::= ":" <type-function> [ <function-body> ]
/// <decl-var>      ::= ":" <type> [ "=" <expr> ] | "::" <expr>
auto intc::Parser::ParseDeclRest(
    std::string ident,
    lcc::Location location,
    bool is_extern,
    bool is_static
) -> Result<Decl*> {
    /// Dispatch based on the declaration kind.
    switch (tok.kind) {
        default: return Error("Expected :, ::, or :> after identifier in declaration");

        /// Type or variable declaration.
        case Tk::Colon: {
            NextToken();

            /// If the next token is 'type' or 'struct', then this
            /// is a type declaration.
            if (At(Tk::Struct, Tk::Type)) {
                if (is_extern) Error("Type declarations cannot be extern");
                if (is_static) Error("Type declarations cannot be static");
                auto decl_name = ident; /// Copy required below.

                /// Struct declaration.
                if (At(Tk::Struct)) {
                    /// Parse the type.
                    auto decl = ParseType();
                    if (not decl) return decl.diag();

                    /// Type must be a struct type.
                    if (auto s = cast<StructType>(*decl)) {
                        return CurrScope()->declare(
                            context,
                            std::move(decl_name),
                            new (*mod) StructDecl(mod.get(), std::move(ident), s, location)
                        );
                    } else {
                        return Error("Declared type must be an unqualified struct type");
                    }
                }

                /// Type alias declaration.
                LCC_ASSERT(Consume(Tk::Type));

                /// Parse the type.
                if (not Consume(Tk::Eq)) return Error("Type alias declarations must have an initialiser");
                auto type = ParseType();
                if (not type) return type.diag();

                /// Create the type alias.
                return CurrScope()->declare(
                    context,
                    std::move(decl_name),
                    new (*mod) TypeAliasDecl(
                        std::move(ident),
                        *type,
                        Location{location, type->location()}
                    )
                );
            }

            /// Otherwise, this is a variable or function declaration.
            auto ty = ParseType();
            if (not ty) return ty.diag();

            /// If the type is a function type, then this is
            /// a function declaration.
            if (auto type = cast<FuncType>(*ty)) {
                if (is_static) Error("Function declarations cannot be static");
                return ParseFuncDecl(std::move(ident), type, is_extern);
            }

            /// Otherwise, it is a variable declaration. Parse
            /// the initialiser if there is one.
            auto init = ExprResult::Null();
            if (Consume(Tk::Eq)) {
                init = ParseExpr();
                if (not init) return init.diag();

                /// This declaration is syntactically well formed, so
                /// no need to synchronise. Keep parsing.
                if (is_extern) Error(location, "Extern declarations may not have an initialiser");
            }

            /// Create the variable.
            auto var = new (*mod) VarDecl(
                ident,
                *ty,
                *init,
                mod.get(),
                is_extern ? Linkage::Imported : (is_static ? Linkage::Internal : Linkage::LocalVar),
                location
            );

            /// Static is redundant if extern is given.
            if (is_static and is_extern) Diag::Warning(
                context,
                var->location(),
                "Static specifier has no effect on this declaration"
            );

            /// Add it to the current scope and return it.
            return Declare(std::move(ident), var);
        }

        /// Variable declaration with type inference.
        case Tk::ColonColon: {
            auto cc_loc = tok.location;
            NextToken();
            auto expr = ParseExpr();
            if (not expr) return expr.diag();

            /// Create the variable.
            auto var = new (*mod) VarDecl(
                ident,
                Type::Unknown,
                *expr,
                mod.get(),
                is_static ? Linkage::Internal : Linkage::LocalVar,
                location
            );

            /// Declarations that use type inference cannot be external or static.
            if (is_extern) Error(cc_loc, "Extern declarations must specify a type");
            return Declare(std::move(ident), var);
        }
    }
}

/// See grammar.bnf for a list of productions handled by this rule.
/// <expr> ::= ...
auto intc::Parser::ParseExpr(isz current_precedence, bool single_expression) -> ExprResult {
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

        /// Declaration.
        case Tk::Export:
        case Tk::Extern:
        case Tk::Static:
            lhs = ParseDecl();
            break;

        /// Expression that starts with an identifier.
        case Tk::Ident:
            lhs = ParseIdentExpr();
            break;

        /// Because of parsing problems, expressions must not start with a type.
        case Tk::ArbitraryInt:
        case Tk::Bool:
        case Tk::Byte:
        case Tk::IntKw:
        case Tk::Void:
        case Tk::Struct:
        case Tk::Type:
            return Error("Types are not allowed here. Did you forget a ':' or 'lambda'?");

        /// Better error message for this.
        case Tk::Else:
            return Error("Unexpected 'else'. Did you forget an 'if' somewhere?");

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
                auto element = ParseExpr(0, true);
                if (not element) return element.diag();
                elements.push_back(element.value());
                Consume(Tk::Comma);
            }

            /// Yeet "]".
            if (not Consume(Tk::RBrack)) return Error("Expected ]");
            lhs = new (*mod) CompoundLiteral(std::move(elements), tok.location);
        } break;

        /// Parenthesised expression.
        ///
        /// Note that parens around types are possible, but only in a
        /// context where we definitely expect a type.
        case Tk::LParen: {
            NextToken();
            lhs = ParseExpr();
            if (not lhs) return lhs.diag();
            if (not Consume(Tk::RParen)) return Error("Expected )");
        } break;

        /// Unary operators.
        case Tk::Ampersand:
        case Tk::At:
        case Tk::Minus:
        case Tk::Tilde:
        case Tk::Exclam: {
            auto loc = tok.location;
            auto op = tok.kind;
            NextToken();
            lhs = ParseExpr(PrefixOperatorPrecedence) >>= [&](Expr* operand) {
                return new (*mod) UnaryExpr(op, operand, false, loc);
            };
        } break;

        /// Lambda expression.
        case Tk::Lambda: {
            auto loc = tok.location;
            NextToken();
            auto ty = ParseType();
            if (ty or not is<FuncType>(*ty)) return Error("Type of lambda must be a function type");
            auto func_ty = cast<FuncType>(ty.value());
            lhs = ParseFuncDecl("", func_ty, false);
            lhs->location({loc, lhs->location()});
        } break;
    }

    /// Some places in the grammar are ambiguous and require us to
    /// know whether we’re at the start of an expression or not. To
    /// make sure we don’t accidentally forget to add something to
    /// MayStartAnExpression(), check that, if we have just parsed
    /// an expression, its first token is recognised as the start
    /// of an expression.
    LCC_ASSERT(
        MayStartAnExpression(start_token),
        "Add {} to MayStartAnExpression() in lib/intercept/parser.cc",
        ToString(start_token)
    );

    /// If the LHS is a diag, return.
    if (not lhs) return lhs.diag();

    /// LHS must not be null if we get here.
    LCC_ASSERT(
        lhs.value(), /// Not `is_value()` because we create a null value above.
        "Someone forgot to assign to `lhs` when parsing expr starting with {}",
        ToString(start_token)
    );

    /// The rules for operator precedence parsing are as follows:
    ///   - unary prefix operators are unambiguously handled up above;
    ///
    ///   - if the current token is a binary or postfix operator whose precedence is
    ///     higher than the current precedence, or higher than or equal to
    ///     the current precedence if the operator is right-associative, then
    ///     the current LHS is the LHS of that operator;
    ///
    ///   - otherwise, return the current LHS as its own expression.
    const auto ShouldKeepParsingOperators = [&] {
        if (lhs.is_diag()) return false;
        const auto prec = BinaryOrPostfixPrecedence(tok.kind);
        if (prec > current_precedence) return true;
        if (prec == current_precedence) return IsRightAssociative(tok.kind);
        return false;
    };

    /// Binary operator parse loop.
    while (ShouldKeepParsingOperators()) {
        /// Handle ‘operators’ that require special parsing.
        switch (tok.kind) {
            default: break;

            /// Subscript expression.
            case Tk::LBrack: {
                NextToken();
                auto index = ParseExpr();
                if (not index) return index.diag();
                lhs = new (*mod) BinaryExpr(Tk::LBrack, *lhs, *index, {lhs->location(), tok.location});
                if (not Consume(Tk::RBrack)) return Error("Expected ]");
                continue;
            }

            /// Cast operator. The RHS is a type.
            case Tk::As:
            case Tk::AsBang: {
                const auto cast_kind = tok.kind == Tk::As ? CastKind::SoftCast : CastKind::HardCast;
                NextToken();
                auto ty = ParseType();
                if (not ty) return ty.diag();
                lhs = new (*mod) CastExpr(*lhs, *ty, cast_kind, {lhs->location(), tok.location});
                continue;
            }

            /// The member access operator must be followed by an identifier.
            case Tk::Dot: {
                NextToken();
                if (not At(Tk::Ident)) return Error("Expected identifier after .");
                auto member = tok.text;
                auto loc = tok.location;
                NextToken();
                lhs = new (*mod) MemberAccessExpr(*lhs, std::move(member), loc);
                continue;
            }
        }

        /// Regular binary expression.
        auto op = tok.kind;
        NextToken();
        auto rhs = ParseExpr(BinaryOrPostfixPrecedence(op));
        if (not rhs) return rhs.diag();
        lhs = new (*mod) BinaryExpr(op, *lhs, *rhs, {lhs->location(), rhs->location()});
    }

    /// As a special case, "()" after an expression is always a call, even
    /// if we’re parsing single-expressions.
    if (At(Tk::LParen) and Is(LookAhead(1), Tk::RParen)) {
        lhs = new (*mod) CallExpr(
            lhs.value(),
            {},
            {lhs->location(), LookAhead(1)->location}
        );

        /// Yeet "()".
        NextToken();
        NextToken();
    }

    /// While we’re at the start of an expression, if we’re not parsing
    /// a single-expression, parse call arguments.
    // Exception: parsed expression (would-be callee) must be an identifier or
    // a lambda.
    else if (not single_expression and (lhs->kind() == Expr::Kind::NameRef or lhs->kind() == Expr::Kind::FuncDecl)) {
        std::vector<Expr*> args;

        /// Ignore unary operators that could also be binary operators.
        while (AtStartOfExpression() and not At(Tk::Minus, Tk::Plus, Tk::Ampersand)) {
            auto expr = ParseExpr(CallPrecedence, true);
            if (not expr) return expr.diag();
            args.push_back(expr.value());
        }

        /// If there are arguments, create a call expression.
        if (not args.empty()) {
            lhs = new (*mod) CallExpr(
                lhs.value(),
                std::move(args),
                {lhs->location(), tok.location}
            );
        }
    }

    return lhs;
}

auto intc::Parser::ParseExprInNewScope() -> ExprResult {
    ScopeRAII sc{this};
    return ParseExpr();
}

/// <expr-for> ::= FOR <expr> [ "," ] <expr> [ "," ] <expr> DO <expr>
auto intc::Parser::ParseForExpr() -> Result<ForExpr*> {
    auto loc = tok.location;
    LCC_ASSERT(Consume(Tk::For), "ParseForExpr called while not at 'for'");

    /// Parse init, cond, increment, and body.
    ScopeRAII sc{this};
    auto init = ParseExpr();
    if (not Consume(Tk::Comma)) Error("Expected ','");
    auto cond = ParseExpr();
    if (not Consume(Tk::Comma)) Error("Expected ','");
    auto increment = ParseExpr();
    if (not Consume(Tk::Do)) Error("Expected 'do'");
    auto body = ParseExpr();

    /// Check for errors and create the expression.
    if (IsError(init, cond, increment, body)) return Diag();
    return new (*mod) ForExpr(*init, *cond, *increment, *body, loc);
}

auto intc::Parser::ParseFuncAttrs() -> Result<FuncType::Attributes> {
    static const StringMap<FuncAttr> attrs_map{
        {"const", FuncAttr::Const},
        {"discardable", FuncAttr::Discardable},
        {"flatten", FuncAttr::Flatten},
        {"inline", FuncAttr::Inline},
        {"noinline", FuncAttr::NoInline},
        {"nomangle", FuncAttr::NoMangle},
        {"noreturn", FuncAttr::NoReturn},
        {"pure", FuncAttr::Pure},
        {"returns_twice", FuncAttr::ReturnsTwice},
        {"used", FuncAttr::Used},
        {"__noopt__", FuncAttr::NoOpt},
    };

    /// Parse attributes while we’re at an identifier.
    FuncType::Attributes attrs;
    for (;;) {
        if (not At(Tk::Ident)) return attrs;
        auto it = attrs_map.find(tok.text);
        if (it == attrs_map.end()) return attrs;
        if (attrs[it->second]) Diag::Warning(context, tok.location, "Duplicate attribute ignored");
        attrs[it->second] = true;
        NextToken();
    }
}

/// <function-body>  ::= "=" <expr> | <expr-block>
auto intc::Parser::ParseFuncBody(bool is_extern) -> Result<std::pair<Expr*, Scope*>> {
    /// If the declaration is external, but there still seems to be
    /// a function body, warn the the user that they might be trying
    /// to do something that doesn’t make sense.
    if (is_extern) {
        /// Equals sign is a hard error because it would have
        /// to be parsed as a declaration.
        if (At(Tk::Eq)) {
            return Error(
                tok.location,
                "External functions cannot have a body."
            );
        }

        /// Lbrace may just be a missing semicolon or weirdly
        /// placed block expression.
        if (At(Tk::LBrace)) {
            return Diag::Warning(
                context,
                tok.location,
                "External functions cannot have a body. If this '{{' is not "
                "supposed to start a function body, consider adding a ';' after "
                "the function type"
            );
        }

        /// Do not parse a body. This is one of the few places
        /// in the parser where we actually return a null result.
        ///
        /// Note that you should normally not do this. In this
        /// case, the caller is aware of this case, and function
        /// bodies are allowed to be null for extern functions,
        /// so it’s not a problem.
        return {};
    }

    /// Function body is in a new scope. Note that the scope of a function
    /// is a child of the global scope if we’re at the top level rather than
    /// of the global scope.
    ScopeRAII sc{this, CurrScope() == TopLevelScope() ? GlobalScope() : CurrScope()};
    sc.scope->set_function_scope();

    /// The body must either be a block expression or an equals sign
    /// followed by an expression.
    auto scope = sc.scope;
    auto expr = Consume(Tk::Eq) ? ParseExpr() : ParseBlock(std::move(sc));
    if (expr.is_diag()) return expr.diag();
    return std::pair{*expr, scope};
}

/// <type-signature> ::= "(" <param-decls> ")" <func-attrs>
/// <param-decls>    ::= { <param-decl> [ "," ]  }
/// <param-decl>     ::= [ IDENTIFIER { [ "," ] IDENTIFIER } ] ":" <type>
auto intc::Parser::ParseFuncSig(Type* return_type) -> Result<FuncType*> {
    LCC_ASSERT(Consume(Tk::LParen), "ParseFunctionSignature called while not at '('");

    /// Parse the parameter declarations.
    std::vector<FuncType::Param> parameters;
    while (not At(Tk::RParen)) {
        /// If we’re at a colon, this parameter is unnamed.
        if (Consume(Tk::Colon)) {
            auto type = ParseType();
            if (not type) return type.diag();
            parameters.emplace_back("", *type, type->location());

        }

        /// Otherwise, we have a comma-separated list of names.
        else {
            usz idx = parameters.size();
            if (not At(Tk::Ident)) return Error("Expected identifier or ':' in parameter declaration");
            do {
                parameters.emplace_back(tok.text, nullptr, tok.location);
                NextToken();
                Consume(Tk::Comma);
            } while (At(Tk::Ident));

            /// Parse the parameter type.
            if (not Consume(Tk::Colon)) return Error("Expected ':' in parameter declaration");
            auto type = ParseType();
            if (not type) return type.diag();

            /// Set the name for all the parameters.
            for (; idx < parameters.size(); ++idx) parameters[idx].type = *type;
        }

        /// Discard trailing comma.
        Consume(Tk::Comma);
    }

    /// Yeet ')'.
    if (not Consume(Tk::RParen)) return Error("Expected ')' in function signature");

    /// Parse attributes.
    auto attrs = ParseFuncAttrs();
    if (not attrs) return attrs.diag();

    /// Create the function type.
    return new (*mod) FuncType(
        std::move(parameters),
        return_type,
        std::move(*attrs),
        Location{tok.location, tok.location}
    );
}

auto intc::Parser::ParseIdentExpr() -> Result<Expr*> {
    auto loc = tok.location;
    auto text = tok.text;
    LCC_ASSERT(Consume(Tk::Ident), "ParseIdentExpr called while not at identifier");

    /// If the next token is ':' or '::', then this is a declaration.
    if (At(Tk::Colon, Tk::ColonColon)) return ParseDeclRest(std::move(text), loc, false, false);

    /// Otherwise, it’s just a name.
    return new (*mod) NameRefExpr(std::move(text), CurrScope(), loc);
}

/// <expr-if> ::= IF <expr> THEN <expr> [ ELSE <expr> ]
auto intc::Parser::ParseIfExpr() -> Result<IfExpr*> {
    /// Yeet "if".
    auto loc = tok.location;
    LCC_ASSERT(Consume(Tk::If), "ParseIf called while not at 'if'");

    /// Parse condition, then, and else.
    auto cond = ParseExpr();
    if (not Consume(Tk::Then)) Error("Expected 'then' after condition of if expression");
    auto then = ParseExprInNewScope();
    auto else_ = ExprResult::Null();
    if (Consume(Tk::Else)) else_ = ParseExpr();
    if (IsError(cond, then, else_)) return Diag();
    return new (*mod) IfExpr(cond.value(), then.value(), else_.value(), loc);
}

/// <preamble> ::= [ <module-declaration> ] { <import-declaration> | ";" }
auto intc::Parser::ParsePreamble(File& f) -> Result<void> {
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

    while (At(Tk::Semicolon)) NextToken();

    /// Parse imports.
    while (At(Tk::Ident) and tok.text == "import" and not tok.artificial) {
        NextToken(); /// Yeet "import".
        if (not At(Tk::Ident)) return Error("Expected module name after import");

        /// Add the module to be loaded later.
        mod->add_import(tok.text);
        NextToken(); /// Yeet module name.
        while (At(Tk::Semicolon)) NextToken();
    }

    return {};
}

/// <type-struct> ::= TYPE <struct-body>
/// <struct-body> ::= "{" { <member-decl> } "}"
/// <member-decl> ::= IDENTIFIER ":" <type> [ ";" ]
auto intc::Parser::ParseStructType() -> Result<StructType*> {
    auto loc = tok.location;
    LCC_ASSERT(Consume(Tk::Struct), "ParseStructType called while not at 'type'");
    if (not Consume(Tk::LBrace)) return Error("Expected '{{' after 'type' in struct declaration");

    /// Parse the struct body.
    std::vector<StructType::Member> members;
    while (not At(Tk::RBrace)) {
        /// Name.
        auto name = tok.text;
        auto start = tok.location;
        if (not Consume(Tk::Ident)) return Error("Expected member name in struct declaration");

        /// Type.
        if (not Consume(Tk::Colon)) return Error("Expected ':' in struct declaration");
        auto type = ParseType();
        if (not type) return type.diag();

        /// Add the member to the list.
        members.emplace_back(std::move(name), *type, Location{start, type->location()});
        Consume(Tk::Semicolon);
    }

    /// Yeet '}'.
    if (not Consume(Tk::RBrace)) return Error("Expected '}}' in struct declaration");

    /// Create the struct type.
    return new (*mod) StructType(std::move(members), Location{loc, tok.location});
}

/// <file> ::= <preamble> { [ <expr> ] ";" }
void intc::Parser::ParseTopLevel() {
    curr_func = mod->top_level_func();

    /// Create the global and top-level scope. The top-level scope is a bit of
    /// a weird one because it only contains the local variables of the top-level
    /// functions. Everything else at the top-level goes in the global scope.
    auto global = new (*mod) Scope(nullptr);
    auto top_level = new (*mod) Scope(global);

    /// Set up the rest of the parser state.
    scope_stack.push_back(global);
    scope_stack.push_back(top_level);
    curr_func->scope(TopLevelScope());

    /// Parse the file.
    for (;;) {
        if (Consume(Tk::Semicolon)) continue;

        /// Stop if we’re at end of file.
        if (At(Tk::Eof)) break;

        /// Parse a top-level expression.
        auto expr = ParseExpr();
        //Consume(Tk::Semicolon);
        if (not Consume(Tk::Semicolon)) {
            auto location = expr ? expr.value()->location() : Location{};
            if (expr->kind() == Expr::Kind::VarDecl) {
                auto var_decl = as<VarDecl>(expr.value());
                if (var_decl->init())
                    location = var_decl->init()->location();
                else location = var_decl->type()->location();
            }
            if (expr->kind() == Expr::Kind::FuncDecl and as<FuncDecl>(expr.value())->body())
                location = as<FuncDecl>(expr.value())->body()->location();

            // Limit location to length of one, discarding the beginning (fold right).
            if (location.len > 1) {
                location.pos += location.len - 1;
                location.len = 1;
            }

            Warning(location, "Expected ';'")
                .attach(false,
                        Diag::Note(context, tok.location, "Before this"));
        }
        if (expr) mod->add_top_level_expr(expr.value());

        /// Synchronise on semicolons and braces in case of an error.
        else Synchronise();
    }
}

/// Parse a type where a type is expected.
///
/// <type>           ::= <type-quals> <type-base> <type-rest> | "(" <type> ")"
/// <type-quals>     ::= { "@" | "&" }
/// <type-base>      ::= <type-struct> | <type-builtin> | IDENTIFIER | INT_TYPE
/// <type-struct>    ::= TYPE <struct-body>
/// <type-builtin>   ::= INTEGER | BYTE | BOOL | VOID
/// <type-rest>      ::= { <type-arr-sz> | <type-signature>  }
/// <type-arr-sz>    ::= "[" <expr> "]"
/// <type-signature> ::= "(" <param-decls> ")" <func-attrs>
/// <param-decls>    ::= { <param-decl> [ "," ]  }
/// <param-decl>     ::= [ IDENTIFIER ] ":" <type>
/// <func-attrs>     ::= /// All function attributes
auto intc::Parser::ParseType(isz current_precedence) -> Result<Type*> {
    /// Parse the base type.
    Type* ty{};
    auto location = tok.location;
    switch (tok.kind) {
        default: return Error("Expected type");

        /// Builtin types.
        case Tk::IntKw:
            ty = BuiltinType::Integer(*mod, tok.location);
            NextToken();
            break;

        case Tk::Byte:
            ty = BuiltinType::Byte(*mod, tok.location);
            NextToken();
            break;

        case Tk::Bool:
            ty = BuiltinType::Bool(*mod, tok.location);
            NextToken();
            break;

        case Tk::Void:
            ty = BuiltinType::Void(*mod, tok.location);
            NextToken();
            break;

        /// Named type.
        case Tk::Ident:
            ty = new (*mod) NamedType(tok.text, CurrScope(), tok.location);
            NextToken();
            break;

        /// Parenthesised type.
        case Tk::LParen:
            NextToken();
            if (auto type = ParseType(); not type) return type.diag();
            else ty = *type;
            if (not Consume(Tk::RParen)) return Error("Expected )");
            break;

        /// Integer type.
        case Tk::ArbitraryInt:
            ty = new (*mod) IntegerType(
                tok.integer_value,
                tok.text[0] != 'u',
                tok.location
            );

            NextToken();
            break;

        /// Structure type.
        case Tk::Struct:
            if (auto type = ParseStructType(); not type) return type.diag();
            else ty = *type;
            break;

        /// Pointer type.
        case Tk::At: {
            NextToken();
            auto type = ParseType(TypeQualifierPrecedence(Tk::At));
            if (not type) return type.diag();
            ty = new (*mod) PointerType(*type, tok.location);
        } break;

        /// Reference type.
        case Tk::Ampersand: {
            NextToken();
            auto type = ParseType(TypeQualifierPrecedence(Tk::Ampersand));
            if (not type) return type.diag();
            ty = new (*mod) ReferenceType(*type, tok.location);
        } break;
    }

    /// Parse trailing type qualifiers. These are obviously
    /// all left associative.
    while (TypeQualifierPrecedence(tok.kind) > current_precedence) {
        switch (tok.kind) {
            default: LCC_ASSERT(false, "Unhandled trailing type qualifier");

            /// An at sign or ampersand here are no longer part of the type.
            case Tk::At:
            case Tk::Ampersand:
                return ty;

            /// Function type.
            case Tk::LParen: {
                auto type = ParseFuncSig(ty);
                if (not type) return type.diag();
                ty = *type;
            } break;

            /// Array type.
            case Tk::LBrack: {
                NextToken();
                auto size = ParseExpr();
                if (not size) return size.diag();
                // The 1 is for the rbrack.
                location.len = u16(1 + tok.location.pos - location.pos);
                if (not Consume(Tk::RBrack)) return Error("Expected ]");
                ty = new (*mod) ArrayType(ty, *size, location);
            } break;
        }
    }

    /// Return the finished type.
    return ty;
}

/// <expr-while> ::= WHILE <expr> DO <expr>
auto intc::Parser::ParseWhileExpr() -> Result<WhileExpr*> {
    /// Yeet "while".
    auto loc = tok.location;
    LCC_ASSERT(Consume(Tk::While), "ParseWhile called while not at 'while'");

    /// Parse condition and body.
    auto cond = ParseExpr();
    if (not Consume(Tk::Do)) Error("Expected 'do' after condition of while expression");
    auto body = ParseExprInNewScope();
    if (IsError(cond, body)) return Diag();
    return new (*mod) WhileExpr(cond.value(), body.value(), loc);
}

void intc::Parser::Synchronise() {
    while (not At(Tk::Semicolon, Tk::LBrace, Tk::RBrace, Tk::Eof)) NextToken();
    NextToken();
}

/// Parse a function declaration.
///
/// \param name Function name. If empty, this is an anonymous function.
/// \param type The type of the function.
/// \param is_extern Whether this is an external function.
/// \return The decl or an error.
auto intc::Parser::ParseFuncDecl(
    std::string name,
    FuncType* type,
    bool is_extern
) -> Result<FuncDecl*> {
    /// Parse attributes and the function body.
    auto body = ParseFuncBody(is_extern);
    if (not body) return Diag();

    /// Create the function.
    auto func = new (*mod) FuncDecl(
        name,
        type,
        (*body).first,
        (*body).second,
        mod.get(),
        is_extern ? Linkage::Imported : Linkage::Internal,
        type->location()
    );

    /// If the function is anonymous, then this is a lambda.
    if (name.empty()) return func;

    /// Add it to the current scope and return it.
    return as<FuncDecl>(Declare(std::move(name), func));
}
