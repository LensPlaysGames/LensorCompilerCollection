#include <glint/ast.hh>
#include <glint/parser.hh>
#include <lcc/diags.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>
#include <lcc/utils/macros.hh>
#include <lcc/utils/rtti.hh>

#define bind LCC_BIND

namespace intc = lcc::glint;

namespace {
/// Get the binary precedence of a token.
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

        case Tk::And:
            return 150;
        case Tk::Or:
            return 145;

        case Tk::ColonEq:
        case Tk::ColonColon:
            return 100;

        /// Not an operator.
        default: return -1;
    }
}

/// Check if an operator is right-associative.
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
        case Tk::RightArrow:
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
        case Tk::BangLBrace:
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
        case Tk::External:
        case Tk::For:
        case Tk::Return:
        case Tk::Export:
        case Tk::Lambda:
        case Tk::Expression:
        case Tk::True:
        case Tk::False:
        case Tk::Colon:
        case Tk::Sizeof:
        case Tk::Alignof:
            return true;

        // Types
        case Tk::ArbitraryInt:
        case Tk::Byte:
        case Tk::Bool:
        case Tk::CShort:
        case Tk::CUShort:
        case Tk::CInt:
        case Tk::CUInt:
        case Tk::CLong:
        case Tk::CULong:
        case Tk::CLongLong:
        case Tk::CULongLong:
        case Tk::Int:
        case Tk::UInt:
        case Tk::Void:
            return true;

        case Tk::Struct:
        case Tk::Enum:
        case Tk::Invalid:
        case Tk::Eof:
        case Tk::RParen:
        case Tk::RBrack:
        case Tk::RBrace:
        case Tk::Comma:
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
        case Tk::RightArrow:
        case Tk::Else:
        case Tk::Gensym:
        case Tk::MacroArg:
        case Tk::And:
        case Tk::Or:
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
/// TODO: Update with new syntax, if this is still even needed or useful.
///     [i32.ptr 10]
///     [i32 10].ref
///     i32.ptr()
///     i32.ref()
///     [i32.ref.ptr 10]
///     [i32.ptr 10].ref
constexpr lcc::isz TypeQualifierPrecedence(intc::TokenKind t) {
    using Tk = intc::TokenKind;
    switch (t) {
        case Tk::At: return 400;
        case Tk::Dot: return 400;
        case Tk::LBrack: return 300;
        case Tk::Ampersand: return 200;
        case Tk::LParen: return 100;
        default: return -1;
    }
}
} // namespace

bool intc::Parser::AtStartOfExpression() { return MayStartAnExpression(tok.kind); }

auto intc::Parser::Parse(Context* context, std::string_view source) -> std::unique_ptr<Module> {
    Parser parser(context, source);

    /// Parse preamble. This also creates the module.
    if (not parser.ParsePreamble(nullptr)) return {};

    /// Parse Glint source.
    parser.ParseTopLevel();

    /// Return nullptr on error.
    if (context->has_error())
        return std::unique_ptr<Module>{};

    return std::move(parser.mod);
}

auto intc::Parser::Parse(Context* context, File& file) -> std::unique_ptr<Module> {
    Parser parser(context, &file);

    /// Parse preamble. This also creates the module.
    if (not parser.ParsePreamble(nullptr)) return {};

    /// Parse Glint source.
    parser.ParseTopLevel();

    /// Return nullptr on error.
    if (context->has_error())
        return std::unique_ptr<Module>{};

    return std::move(parser.mod);
}

/// Creates a new scope and parses a block in that scope.
auto intc::Parser::ParseBlock() -> Result<BlockExpr*> {
    return ParseBlock({this});
}

auto intc::Parser::ParseBlock(
    /// The only purpose of this parameter is to open a new scope
    /// for this block. Do NOT remove it, even if it appears unused.
    [[maybe_unused]] ScopeRAII sc
) -> Result<BlockExpr*> {
    auto loc = tok.location;
    LCC_ASSERT(Consume(Tk::LBrace), "ParseBlock called while not at '{{'");

    /// Parse expressions.
    std::vector<Expr*> exprs;
    while (not At(Tk::RBrace, Tk::Eof)) {
        if (Consume(Tk::Semicolon)) continue;
        auto expr = ParseExpr();
        if (not Consume(Tk::Semicolon)) {
            if (At(Tk::Eof)) {
                Warning("Expected ';' but got end of file");
            } else if (expr) {
                Location location{};
                if (expr.value()->location().is_valid())
                    location = expr.value()->location();

                // Attempt to get the location that is as close to where the semi-colon should be.
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

                // Error(location, "Expected ';'")
                Warning(location, "Expected ';'")
                    .attach(false, Diag::Note(context, tok.location, "Before this"));
            }
        }
        if (not expr) return expr.diag();
        exprs.push_back(expr.value());
    }

    /// Yeet "}".
    if (not Consume(Tk::RBrace)) return Error("Expected }}");
    return new (*mod) BlockExpr(std::move(exprs), loc);
}

/// Parse an object or type declaration.
auto intc::Parser::ParseDecl() -> Result<Decl*> {
    auto is_export = Consume(Tk::Export);
    auto is_extern = Consume(Tk::External);
    auto loc = tok.location;
    auto text = tok.text;
    if (not Consume(Tk::Ident)) return Error("Expected identifier to start declaration");

    // NOTE: The important bit.
    auto decl = ParseDeclRest(text, loc, is_extern);

    /// Apply storage specifiers.
    if (is_export) {
        /// Export a declaration.
        const auto Export = [&](Decl* decl) -> Result<Decl*> {
            /// Set linkage to exported if this has linkage.
            if (auto obj = cast<ObjectDecl>(decl))
                obj->linkage(obj->linkage() == Linkage::Imported ? Linkage::Reexported : Linkage::Exported);

            /// Add the declaration to the module’s export list.
            mod->add_export(decl);
            return decl;
        };

        /// Exported declaration.
        if (CurrScope() != TopLevelScope())
            Error("Exported declarations are only allowed at the top level");

        decl = decl >>= Export;
    }

    return decl;
}

/// Parse everything after the identifier in an object declaration.
/// NOTE: Will `move(ident)`
auto intc::Parser::ParseDeclRest(
    std::string ident,
    lcc::Location location,
    bool is_extern
) -> Result<Decl*> {
    switch (tok.kind) {
        default: return Error("Expected : or :: after identifier in declaration");

        /// Type or variable declaration
        case Tk::Colon: {
            NextToken();

            /// Type declaration
            if (At(Tk::Enum, Tk::Struct)) {
                if (is_extern) Error("Type declarations cannot be extern");
                // Copy required due to `move(ident)` below.
                auto decl_name = ident;

                /// Struct or enum declaration
                auto decl = ParseType();
                if (not decl) return decl.diag();
                return DeclScope()->declare(
                    context,
                    std::move(decl_name),
                    new (*mod) TypeDecl(
                        mod.get(),
                        std::move(ident),
                        as<DeclaredType>(*decl),
                        location
                    )
                );
            }

            /// Otherwise, this is a variable or function declaration.
            auto ty = ParseType();
            if (not ty) return ty.diag();

            /// If the type is a function type, then this is
            /// a function declaration.
            if (auto type = cast<FuncType>(*ty))
                return ParseFuncDecl(std::move(ident), type, is_extern);

            /// Otherwise, it is a variable declaration. Parse
            /// the initialiser if there is one.
            auto init = ExprResult::Null();
            if (Consume(Tk::Eq) or not At(Tk::Semicolon, Tk::Comma)) {
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
                is_extern ? Linkage::Imported : Linkage::LocalVar,
                location
            );

            /// Add it to the current scope and return it.
            return DeclScope(var->linkage() == Linkage::LocalVar)->declare(context, std::move(ident), var);
        }

        /// Variable declaration with required initializer (type inferenced).
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
                Linkage::LocalVar,
                location
            );

            // Declarations that use type inference cannot have certain storage specifiers.
            if (is_extern) Error(cc_loc, "External declarations must specify a type");
            return DeclScope(var->linkage() == Linkage::LocalVar)->declare(context, std::move(ident), var);
        }
    }
}

/// Parse an enum declaration.
auto intc::Parser::ParseEnumType() -> Result<EnumType*> {
    auto loc = tok.location;
    LCC_ASSERT(Consume(Tk::Enum), "ParseEnumType called while not at 'enum'");

    // Parse underlying type, if present.
    Type* underlying = Type::Int;
    if (Consume(Tk::LParen)) {
        auto ty = ParseType();
        if (not ty) return ty.diag();
        if (not Consume(Tk::RParen)) Error("Expected )");
        underlying = *ty;
    }

    // The body is optional (not an ill-formed program, just doesn't do
    // anything meaningful).
    if (not At(Tk::LBrace)) return new (*mod) EnumType(
        new (*mod) Scope(CurrScope()),
        underlying,
        {},
        loc
    );

    // Parse enum body (list of enumerator declarations).
    ScopeRAII sc{this};
    std::vector<EnumeratorDecl*> enumerators;
    NextToken();
    while (At(Tk::Ident)) {
        auto name = tok.text;
        auto enumerator_loc = tok.location;
        NextToken();

        // Parse optional value.
        Expr* value{};
        if (Consume(Tk::Eq, Tk::ColonEq, Tk::ColonColon)) {
            auto v = ParseExpr();
            if (v) value = *v;
        }

        // Add parsed enumerator to enum.
        enumerators.emplace_back(new (*mod) EnumeratorDecl(std::move(name), value, enumerator_loc));

        // Eat separators, if any.
        while (Consume(Tk::Comma, Tk::Semicolon))
            ;
    }

    if (not Consume(Tk::RBrace)) Error("Expected }}");
    return new (*mod)
        EnumType(
            sc.scope,
            underlying,
            std::move(enumerators),
            loc
        );
}

auto intc::Parser::ParseExpr(isz current_precedence, bool single_expression) -> ExprResult {
    auto lhs = ExprResult::Null();

    /// See below.
    const auto start_token = tok.kind;

    /// Parse the LHS.
    switch (tok.kind) {
        case Tk::Gensym: LCC_ASSERT(false, "Gensym token in parser: there is a bug in the lexer");

        /// AST node bound by macro.
        case Tk::Expression:
            LCC_ASSERT(tok.expression);
            lhs = tok.eval_once ? tok.expression : Expr::Clone(*mod, tok.expression);
            NextToken();
            break;

        /// Declaration.
        case Tk::Export:
        case Tk::External:
            lhs = ParseDecl();
            break;

        /// Expression that starts with an identifier.
        case Tk::Ident:
            lhs = ParseIdentExpr();
            break;

        case Tk::True:
            lhs = new (*mod) IntegerLiteral(aint(1), BuiltinType::Bool(*mod, tok.location), tok.location);
            NextToken();
            break;

        case Tk::False:
            lhs = new (*mod) IntegerLiteral(aint(0), BuiltinType::Bool(*mod, tok.location), tok.location);
            NextToken();
            break;

        case Tk::Sizeof: {
            auto loc = tok.location;
            // Yeet `sizeof`
            NextToken();
            lhs = ParseExpr();
            if (not lhs) return lhs.diag();
            lhs = new (*mod) SizeofExpr(*lhs, {loc, lhs->location()});
        } break;

        case Tk::Alignof: {
            auto loc = tok.location;
            // Yeet `alignof`
            NextToken();
            lhs = ParseExpr();
            if (not lhs) return lhs.diag();
            lhs = new (*mod) AlignofExpr(*lhs, {loc, lhs->location()});
        } break;

        // Expressions that start with a type.
        case Tk::ArbitraryInt:
        case Tk::Bool:
        case Tk::Byte:
        case Tk::CShort:
        case Tk::CUShort:
        case Tk::CInt:
        case Tk::CUInt:
        case Tk::CLong:
        case Tk::CULong:
        case Tk::CLongLong:
        case Tk::CULongLong:
        case Tk::Enum:
        case Tk::Int:
        case Tk::LBrack:
        case Tk::Struct:
        case Tk::UInt:
        case Tk::Void: {
            auto ty = ParseType();
            if (not ty) return ty.diag();
            lhs = new (*mod) TypeExpr(*ty, ty->location());
        } break;

        case Tk::Else:
            return Error("Unexpected 'else' (no if).");

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

        /// Parenthesised expression.
        case Tk::LParen: {
            NextToken();
            lhs = ParseExpr();
            if (not lhs) return lhs.diag();
            // If there is more syntax before the paren expression closes, this may be
            // a function call or type instantiation.
            if (not At(Tk::RParen)) {
                std::vector<Expr*> args;

                while (AtStartOfExpression()) {
                    auto expr = ParseExpr(CallPrecedence, true);
                    if (not expr) return expr.diag();
                    args.push_back(expr.value());
                }

                // Iff there are arguments, create a call expression.
                // Handles (5) and (2 + 3) just being "5", while also making (foo 5) a
                // call expression.
                if (not args.empty()) {
                    lhs = new (*mod) CallExpr(
                        lhs.value(),
                        std::move(args),
                        {lhs->location(), tok.location}
                    );
                }
            }

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

        // Lambda expression.
        case Tk::Lambda: {
            auto loc = tok.location;
            NextToken();
            auto ty = ParseType();
            // FIXME: Isn't this supposed to be if (*not* ty)?? or (ty and ...)?
            if (ty or not is<FuncType>(*ty)) return Error("Type of lambda must be a function type");
            auto func_ty = cast<FuncType>(ty.value());
            lhs = ParseFuncDecl("", func_ty, false);
            lhs->location({loc, lhs->location()});
        } break;

        // Explicit type expression (may remove).
        case TokenKind::Colon: {
            auto loc = tok.location;
            NextToken(); // yeet `:`
            auto ty = ParseType();
            if (not ty) return ty.diag();
            lhs = new (*mod) TypeExpr(*ty, {loc, ty->location()});
        } break;

        // Compound literal.
        case TokenKind::BangLBrace: {
            std::vector<Expr*> elements;
            NextToken(); /// Yeet "!{".

            /// Parse the elements.
            while (not At(Tk::RBrace, Tk::Eof)) {
                auto element = ParseExpr(0, true);
                if (not element) return element.diag();
                elements.push_back(element.value());
                Consume(Tk::Comma);
            }

            /// Yeet "}".
            if (not Consume(Tk::RBrace)) return Error("Expected }}");
            lhs = new (*mod) CompoundLiteral(std::move(elements), tok.location);
        } break;

        case TokenKind::Comma:
        case TokenKind::RParen:
        case TokenKind::RBrack:
        case TokenKind::RBrace:
        case TokenKind::Dot:
        case TokenKind::Plus:
        case TokenKind::Star:
        case TokenKind::Slash:
        case TokenKind::Percent:
        case TokenKind::Pipe:
        case TokenKind::Caret:
        case TokenKind::Hash:
        case TokenKind::Shl:
        case TokenKind::Shr:
        case TokenKind::Eq:
        case TokenKind::Ne:
        case TokenKind::Lt:
        case TokenKind::Gt:
        case TokenKind::Le:
        case TokenKind::Ge:
        case TokenKind::ColonEq:
        case TokenKind::ColonColon:
        case TokenKind::RightArrow:
        case TokenKind::And:
        case TokenKind::Or:
        case TokenKind::Invalid:
        case TokenKind::Eof:
            return Error("Expected expression, got {}", ToString(tok.kind));

        case TokenKind::MacroArg:
        case TokenKind::Semicolon:
            Diag::ICE("Unexpected token {} during parsing: likely lexer error", ToString(tok.kind));
    }

    /// Some places in the grammar are ambiguous and require us to
    /// know whether we’re at the start of an expression or not. To
    /// make sure we don’t accidentally forget to add something to
    /// MayStartAnExpression(), check that, if we have just parsed
    /// an expression, its first token is recognised as the start
    /// of an expression.
    LCC_ASSERT(
        MayStartAnExpression(start_token),
        "Add {} to MayStartAnExpression() in lib/glint/parser.cc",
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

    // If encountering a comma at this position, it is a "soft" expression
    // separator. So, we are done parsing whatever expression we were parsing,
    // as if it's a semi-colon. But, it won't cause a function call to stop
    // being parsed. Hope that makes sense.
    // FIXME: Use Consume?
    if (tok.kind == Tk::Comma) {
        NextToken();
        return lhs;
    }

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

            /// The member access operator must be followed by an identifier.
            case Tk::Dot: {
                NextToken();
                if (not At(Tk::Ident)) return Error("Expected identifier after .");
                auto member = tok.text;
                auto loc = tok.location;
                NextToken();
                lhs = new (*mod) MemberAccessExpr(*lhs, std::move(member), {lhs->location(), loc});
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
    // a lambda or a number.
    else if (
        not single_expression
        and (lhs->kind() == Expr::Kind::NameRef or lhs->kind() == Expr::Kind::Type or lhs->kind() == Expr::Kind::FuncDecl or lhs->kind() == Expr::Kind::IntegerLiteral)
    ) {
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

    /// Binary operator parse loop.
    // NOTE: Same as above
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

            /// The member access operator must be followed by an identifier.
            case Tk::Dot: {
                NextToken();
                if (not At(Tk::Ident)) return Error("Expected identifier after .");
                auto member = tok.text;
                auto loc = tok.location;
                NextToken();
                lhs = new (*mod) MemberAccessExpr(*lhs, std::move(member), {lhs->location(), loc});
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

    // Eat any amount of commas following an expression
    while (Consume(Tk::Comma))
        ;

    return lhs;
}

auto intc::Parser::ParseExprInNewScope() -> ExprResult {
    ScopeRAII sc{this};
    return ParseExpr();
}

auto intc::Parser::ParseForExpr() -> Result<ForExpr*> {
    auto loc = tok.location;
    LCC_ASSERT(Consume(Tk::For), "ParseForExpr called while not at 'for'");

    /// Parse init, cond, increment, and body.
    ScopeRAII sc{this};

    auto init = ParseExpr();
    if (not init) return init.diag();
    if (not Consume(Tk::Semicolon)) Error("Expected ; after 'for' initialisation expression");

    auto cond = ParseExpr();
    if (not cond) return cond.diag();
    if (not Consume(Tk::Semicolon)) Error("Expected ; after 'for' condition expression");

    auto increment = ParseExpr();
    if (not increment) return increment.diag();
    if (not Consume(Tk::Semicolon)) Error("Expected ; after 'for' increment expression");

    auto body = ParseExpr();
    if (not body) return body.diag();

    /// Check for errors and create the expression.
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
    auto expr = ExprResult::Null();
    if (Consume(Tk::Eq)) expr = ParseExpr();
    else if (At(Tk::LBrace)) expr = ParseBlock(std::move(sc));

    /// If the body isn't followed by either, it's not a valid function declaration
    else Error("Function declaration should be followed by `=` and an expression or a block");

    if (expr.is_diag()) return expr.diag();
    return std::pair{*expr, scope};
}

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
    if (At(Tk::Colon, Tk::ColonColon)) return ParseDeclRest(std::move(text), loc, false);

    /// Otherwise, it’s just a name.
    return new (*mod) NameRefExpr(std::move(text), CurrScope(), loc);
}

auto intc::Parser::ParseIfExpr() -> Result<IfExpr*> {
    /// Yeet "if".
    auto loc = tok.location;
    LCC_ASSERT(Consume(Tk::If), "ParseIf called while not at 'if'");

    /// Parse condition, then, and else.
    auto cond = ParseExpr();
    Consume(Tk::Comma);
    auto then = ParseExprInNewScope();
    Consume(Tk::Comma);
    auto else_ = ExprResult::Null();
    if (Consume(Tk::Else)) else_ = ParseExpr();
    if (IsError(cond, then, else_)) return Diag();
    return new (*mod) IfExpr(cond.value(), then.value(), else_.value(), loc);
}

auto intc::Parser::ParsePreamble(File* f) -> Result<void> {
    /// Parse module name and create the module.
    if (At(Tk::Ident) and tok.text == "module" and not tok.artificial) {
        NextToken(); /// Yeet "module".
        if (not At(Tk::Ident)) return Error("Expected module name");
        mod = std::make_unique<Module>(f, tok.text, true);
        NextToken(); /// Yeet module name.
    }

    /// Create an executable module instead.
    else {
        mod = std::make_unique<Module>(f, "", false);
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

auto intc::Parser::ParseStructType() -> Result<StructType*> {
    auto loc = tok.location;
    LCC_ASSERT(Consume(Tk::Struct), "ParseStructType called while not at 'struct'");
    if (not Consume(Tk::LBrace)) return Error("Expected '{{' after 'struct' in struct declaration");

    /// Parse the struct body.
    ScopeRAII sc{this};
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
    return new (*mod) StructType(sc.scope, std::move(members), Location{loc, tok.location});
}

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
        if (not Consume(Tk::Semicolon)) {
            if (At(Tk::Eof)) {
                Warning("Expected ';' but got end of file");
            } else if (expr) {
                Location location{};
                if (expr.value()->location().is_valid())
                    location = expr.value()->location();

                // Attempt to get the location that is as close to where the semi-colon should be.
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

                Error(location, "Expected ';'")

                    .attach(false, Diag::Note(context, tok.location, "Before this"));
            }
        }
        if (expr) mod->add_top_level_expr(expr.value());

        /// Synchronise on semicolons and braces in case of an error.
        else Synchronise();
    }
}

/// Parse a type where a type is expected.
auto intc::Parser::ParseType(isz current_precedence) -> Result<Type*> {
    /// Parse the base type.
    Type* ty{};
    auto location = tok.location;
    switch (tok.kind) {
        default: return Error("Expected type");

        /// Builtin types.
        case Tk::Int:
            ty = BuiltinType::Int(*mod, tok.location);
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

        case Tk::CShort:
            ty = FFIType::CShort(*mod, tok.location);
            NextToken();
            break;

        case Tk::CUShort:
            ty = FFIType::CUShort(*mod, tok.location);
            NextToken();
            break;

        case Tk::CInt:
            ty = FFIType::CInt(*mod, tok.location);
            NextToken();
            break;

        case Tk::CUInt:
            ty = FFIType::CUInt(*mod, tok.location);
            NextToken();
            break;

        case Tk::CLong:
            ty = FFIType::CLong(*mod, tok.location);
            NextToken();
            break;

        case Tk::CULong:
            ty = FFIType::CULong(*mod, tok.location);
            NextToken();
            break;

        case Tk::CLongLong:
            ty = FFIType::CLongLong(*mod, tok.location);
            NextToken();
            break;

        case Tk::CULongLong:
            ty = FFIType::CULongLong(*mod, tok.location);
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

        /// Enumeration type.
        case Tk::Enum:
            if (auto type = ParseEnumType(); not type) return type.diag();
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

        // Array type.
        case Tk::LBrack: {
            NextToken();
            auto type = ParseType(TypeQualifierPrecedence(Tk::LBrack));
            if (not type) return type.diag();
            if (Consume(Tk::RBrack)) {
                ty = new (*mod) DynamicArrayType(
                    *type,
                    nullptr,
                    location
                );
            } else {
                auto size_expr = ParseExpr();
                if (not size_expr) return size_expr.diag();
                // TODO: Better check for if expression is compile-time known
                if (size_expr->kind() != Expr::Kind::EvaluatedConstant and size_expr->kind() != Expr::Kind::IntegerLiteral) {
                    // DYNAMIC ARRAY because non-compile-time size expression
                    ty = new (*mod) DynamicArrayType(
                        *type,
                        *size_expr,
                        location
                    );
                } else ty = new (*mod) ArrayType(*type, *size_expr, type->location());
                if (not Consume(Tk::RBrack))
                    return Error("Expected ]");
            }
        } break;
    }

    /// Parse trailing type qualifiers. These are obviously
    /// all left associative.
    while (TypeQualifierPrecedence(tok.kind) > current_precedence) {
        switch (tok.kind) {
            default: LCC_ASSERT(false, "Unhandled trailing type qualifier");

            case Tk::Dot: {
                NextToken();
                if (tok.kind != Tk::Ident)
                    return Error("Expected IDENTIFIER after . following type {}, not {}", *ty, ToString(tok.kind));
                location.len = u16(tok.text.size() + tok.location.pos - location.pos);
                if (tok.text == "pptr") {
                    ty = new (*mod) PointerType(new (*mod) PointerType(ty, location));
                    // Eat "pptr"
                    NextToken();
                    break;
                }
                if (tok.text == "ptr") {
                    ty = new (*mod) PointerType(ty, location);
                    // Eat "ptr"
                    NextToken();
                    break;
                }
                if (tok.text == "ref") {
                    ty = new (*mod) ReferenceType(ty, location);
                    // Eat "ref"
                    NextToken();
                    break;
                }
                return Error("Unrecognized type member access {}.{}. Did you mean .ptr or .ref?", *ty, tok.text);
            } break;

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
        }
    }

    /// Return the finished type.
    return ty;
}

auto intc::Parser::ParseWhileExpr() -> Result<WhileExpr*> {
    /// Yeet "while".
    auto loc = tok.location;
    LCC_ASSERT(Consume(Tk::While), "ParseWhile called while not at 'while'");

    /// Parse condition and body.
    auto cond = ParseExpr();
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
)
    -> Result<FuncDecl*> {
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
    return as<FuncDecl>(DeclScope()->declare(context, std::move(name), func));
}
