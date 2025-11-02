#include <lcc/diags.hh>
#include <lcc/location.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>
#include <lcc/utils/macros.hh>
#include <lcc/utils/rtti.hh>

#include <glint/ast.hh>
#include <glint/parser.hh>

#include <memory>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace {
/// Get the binary precedence of a token.
constexpr inline lcc::isz CallPrecedence = 90;
constexpr auto BinaryOrPostfixPrecedence(lcc::glint::TokenKind t) -> lcc::isz {
    using Tk = lcc::glint::TokenKind;
    switch (t) {
        case Tk::Dot:
            return 1'000'000'000;

        /// Call and subscript have higher precedence than unary operators.
        /// Note: Unary operator precedence is 10'000.
        case Tk::LBrack:
            return 100'001;
            // return CallPrecedence + 1;

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
        case Tk::PlusEq:
        case Tk::MinusEq:
        case Tk::StarEq:
        case Tk::SlashEq:
        case Tk::PercentEq:
        case Tk::AmpersandEq:
        case Tk::PipeEq:
        case Tk::CaretEq:
        case Tk::TildeEq:
        case Tk::LBrackEq:
            return 100;

        // Not binary or postfix unary operators
        case Tk::Invalid:
        case Tk::Eof:
        case Tk::LParen:
        case Tk::RParen:
        case Tk::RBrack:
        case Tk::LBrace:
        case Tk::RBrace:
        case Tk::BangLBrace:
        case Tk::Comma:
        case Tk::Colon:
        case Tk::Semicolon:
        case Tk::Tilde:
        case Tk::Exclam:
        case Tk::At:
        case Tk::Hash:
        case Tk::PlusPlus:
        case Tk::MinusMinus:
        case Tk::StarStar:
        case Tk::RightArrow:
        case Tk::Ident:
        case Tk::Number:
        case Tk::String:
        case Tk::If:
        case Tk::Else:
        case Tk::While:
        case Tk::Void:
        case Tk::Byte:
        case Tk::Bool:
        case Tk::External:
        case Tk::True:
        case Tk::False:
        case Tk::Int:
        case Tk::UInt:
        case Tk::ArbitraryInt:
        case Tk::Sizeof:
        case Tk::Alignof:
        case Tk::Has:
        case Tk::For:
        case Tk::RangedFor:
        case Tk::Return:
        case Tk::Export:
        case Tk::Struct:
        case Tk::Enum:
        case Tk::Union:
        case Tk::Sum:
        case Tk::Lambda:
        case Tk::Supplant:
        case Tk::Match:
        case Tk::Print:
        case Tk::Template:
        case Tk::Typeof:
        case Tk::CShort:
        case Tk::CUShort:
        case Tk::CInt:
        case Tk::CUInt:
        case Tk::CLong:
        case Tk::CULong:
        case Tk::CLongLong:
        case Tk::CULongLong:
        case Tk::Gensym:
        case Tk::MacroArg:
        case Tk::Expression:
        case Tk::ByteLiteral:
            return -1;
    }
    LCC_UNREACHABLE();
}

/// Check if an operator is right-associative.
constexpr bool IsRightAssociative(lcc::glint::TokenKind t) {
    using Tk = lcc::glint::TokenKind;
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
        case Tk::PlusEq:
        case Tk::MinusEq:
        case Tk::StarEq:
        case Tk::SlashEq:
        case Tk::PercentEq:
        case Tk::AmpersandEq:
        case Tk::PipeEq:
        case Tk::CaretEq:
        case Tk::TildeEq:
        case Tk::LBrackEq:
            return true;

        /// Not an operator.
        default: return false;
    }
}

constexpr auto MayStartAnExpression(lcc::glint::TokenKind kind) -> bool {
    using Tk = lcc::glint::TokenKind;
    switch (kind) {
        // Self-evaluating
        case Tk::True:
        case Tk::False:
        case Tk::Number:
        case Tk::ByteLiteral:
        case Tk::String:
        // Regular Expressions
        case Tk::LParen:
        case Tk::LBrack:
        case Tk::LBrace:
        case Tk::BangLBrace:
        case Tk::Ident:
        case Tk::If:
        case Tk::While:
        case Tk::External:
        case Tk::For:
        case Tk::RangedFor:
        case Tk::Return:
        case Tk::Export:
        case Tk::Lambda:
        case Tk::Expression:
        case Tk::Colon:
        case Tk::Supplant:
        case Tk::Match:
        case Tk::Print:
        case Tk::Template:
        // Unary Prefix
        case Tk::Plus:
        case Tk::Minus:
        case Tk::PlusPlus:
        case Tk::MinusMinus:
        case Tk::Ampersand:
        case Tk::Tilde:
        case Tk::Exclam:
        case Tk::At:
        case Tk::Sizeof:
        case Tk::Alignof:
        case Tk::Has:
        case Tk::Typeof:
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
        case Tk::Struct:
        case Tk::Enum:
        case Tk::Union:
        case Tk::Sum:
            return true;

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
        case Tk::StarStar:
        case Tk::PlusEq:
        case Tk::MinusEq:
        case Tk::StarEq:
        case Tk::SlashEq:
        case Tk::PercentEq:
        case Tk::AmpersandEq:
        case Tk::PipeEq:
        case Tk::CaretEq:
        case Tk::TildeEq:
        case Tk::LBrackEq:
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
constexpr auto TypeQualifierPrecedence(lcc::glint::TokenKind t) -> lcc::isz {
    using Tk = lcc::glint::TokenKind;
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

auto lcc::glint::Parser::AtStartOfExpression() -> bool {
    return MayStartAnExpression(tok.kind);
}

/// Creates a new scope and parses a block in that scope.
auto lcc::glint::Parser::ParseBlock() -> Result<BlockExpr*> {
    return ParseBlock(ScopeRAII{this});
}

auto lcc::glint::Parser::ParseExpressionsUntil(lcc::glint::TokenKind until) -> Result<std::vector<Expr*>> {
    std::vector<Expr*> exprs;
    while (not At(until, Tk::Eof)) {
        if (+ConsumeExpressionSeparator()) continue;

        auto expr = ParseExpr();
        if (not At(until) and not +ConsumeExpressionSeparator(ExpressionSeparator::Hard)) {
            if (At(Tk::Eof)) {
                Warning("Expected ';' but got end of file");
            } else if (expr) {
                // Error(location, "Expected ';'")
                Warning(GetPastLocation(*expr), "Expected ';'")
                    .attach(Diag::Note(context, tok.location, "Before this"));
            }
        }
        if (not expr) return expr.diag();
        exprs.emplace_back(expr.value());
    }
    return exprs;
};

auto lcc::glint::Parser::ParseExpressionList() -> Result<std::vector<Expr*>> {
    std::vector<Expr*> exprs;
    while (not At(Tk::Semicolon, Tk::Eof)) {
        if (+ConsumeExpressionSeparator(ExpressionSeparator::Soft)) continue;

        auto expr = ParseExpr();
        ConsumeExpressionSeparator(ExpressionSeparator::Soft);
        if (not expr) return expr.diag();
        exprs.emplace_back(expr.value());
    }
    return exprs;
}

auto lcc::glint::Parser::ParseBlock(
    /// The only purpose of this parameter is to open a new scope
    /// for this block. Do NOT remove it, even if it appears unused.
    [[maybe_unused]] ScopeRAII sc
) -> Result<BlockExpr*> {
    auto loc = tok.location;
    LCC_ASSERT(
        Consume(Tk::LBrace),
        "ParseBlock called while not at '{{'"
    );

    /// Parse expressions.
    auto exprs = ParseExpressionsUntil(Tk::RBrace);
    if (not exprs) return exprs.diag();

    /// Yeet "}".
    if (not Consume(Tk::RBrace))
        Error("Expected }}");

    loc = {loc, tok.location};

    return new (*mod) BlockExpr(std::move(*exprs), loc);
}

/// Parse an object or type declaration.
auto lcc::glint::Parser::ParseDecl() -> Result<Decl*> {
    auto is_export = Consume(Tk::Export);
    auto is_external = Consume(Tk::External);
    auto loc = tok.location;
    auto text = tok.text;
    if (not Consume(Tk::Ident)) return Error("Expected identifier to start declaration");

    // NOTE: The important bit.
    auto decl = ParseDeclRest(text, loc, is_external);

    if (is_export) {
        // Export a declaration.
        const auto Export = [&](Decl* exported_decl) -> Result<Decl*> {
            // Set linkage to exported if this has linkage.
            if (auto obj = cast<ObjectDecl>(exported_decl))
                obj->linkage(obj->linkage() == Linkage::Imported ? Linkage::Reexported : Linkage::Exported);

            // Add the exported declaration to the module’s export list.
            mod->add_export(exported_decl);
            return exported_decl;
        };

        // NOTE: Why? Why can't we export a function defined three nested levels
        // down? Unless it's a closure, I don't see why this would be an issue
        // (other than design choice).
        if (CurrScope() != TopLevelScope())
            Error("Exported declarations are only allowed at the top level");

        // Currying operator; calls Export(decl) iff decl is valid.
        decl = decl >>= Export;
    }

    return decl;
}

/// Parse everything after the identifier in an object declaration.
/// NOTE: Will `move(ident)`
auto lcc::glint::Parser::ParseDeclRest(
    std::string ident,
    lcc::Location location,
    bool is_external
) -> Result<Decl*> {
    switch (tok.kind) {
        default: return Error("Expected : or :: after identifier in declaration");

        /// Type or variable declaration
        case Tk::Colon: {
            NextToken();

            /// Type declaration
            if (At(Tk::Enum, Tk::Struct, Tk::Union, Tk::Sum)) {
                if (is_external) Error("Type declarations cannot be made external; a linker does not know how to resolve Glint types.");
                // Copy required due to `move(ident)` below.
                auto decl_name = ident;

                // Struct or enum declaration
                auto decl = ParseType();
                if (not decl) return decl.diag();
                return DeclScope()->declare(
                    context,
                    std::move(decl_name),
                    new (*mod) TypeDecl(
                        mod,
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
                return ParseFuncDecl(std::move(ident), type, is_external);

            /// Otherwise, it is a variable declaration. Parse
            /// the initialiser if there is one.
            auto init = ExprResult::Null();
            if (Consume(Tk::Eq) or not +AtExpressionSeparator()) {
                init = ParseExpr();
                if (not init) return init.diag();

                /// This declaration is syntactically well formed, so
                /// no need to synchronise. Keep parsing.
                if (is_external) Error(location, "External declarations may not have an initialiser");
            }

            /// Create the variable.
            auto var = new (*mod) VarDecl(
                ident,
                *ty,
                *init,
                mod,
                is_external ? Linkage::Imported : Linkage::LocalVar,
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

            // Create the variable declaration.
            auto* var = new (*mod) VarDecl(
                ident,
                Type::Unknown,
                *expr,
                mod,
                Linkage::LocalVar,
                location
            );

            // Declarations that use type inference cannot have certain storage specifiers.
            // FIXME: Is this syntactic or semantic?
            if (is_external) Error(cc_loc, "Type-inferred declarations cannot be made external");
            return DeclScope(var->linkage() == Linkage::LocalVar)->declare(context, std::move(ident), var);
        }
    }
}

/// Parse an enum declaration.
auto lcc::glint::Parser::ParseEnumType() -> Result<EnumType*> {
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
        while (+ConsumeExpressionSeparator());
    }

    if (not Consume(Tk::RBrace))
        Error("Expected }}");

    return new (*mod)
        EnumType(
            sc.scope,
            underlying,
            std::move(enumerators),
            loc
        );
}

auto lcc::glint::Parser::ParseExpr(isz current_precedence, bool single_expression) -> ExprResult {
    if (+ConsumeExpressionSeparator())
        return Error(tok.location, "Empty expression probably has unintended consequences.");

    // NOTE: Using Null result is dangerous because if we return lhs without
    // first assigning /something/ to it, then it completely breaks.
    auto lhs = ExprResult::Null();

    /// TODO: Stop if there are no more expressions before the end of the file.
    // TODO: Return empty expression or something
    // if (At(Tk::Eof)) return empty;

    /// See below.
    const auto start_token = tok.kind;

    /// Parse the LHS.
    switch (tok.kind) {
        case Tk::Gensym:
            LCC_ASSERT(false, "Gensym token in parser: there is a bug in the lexer");

        /// AST node bound by macro.
        case Tk::Expression:
            LCC_ASSERT(tok.expression);
            if (tok.eval_once)
                lhs = tok.expression;
            else lhs = Expr::Clone(*mod, context, tok.expression);
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

        case Tk::ByteLiteral:
            lhs = new (*mod) IntegerLiteral(
                aint(tok.integer_value),
                BuiltinType::Byte(*mod, tok.location),
                tok.location
            );
            NextToken();
            break;

        case Tk::True:
            lhs = new (*mod) IntegerLiteral(aint(1), BuiltinType::Bool(*mod, tok.location), tok.location);
            NextToken();
            break;

        case Tk::False:
            lhs = new (*mod) IntegerLiteral(aint(0), BuiltinType::Bool(*mod, tok.location), tok.location);
            NextToken();
            break;

        case Tk::Template: {
            auto loc = tok.location;
            // Yeet 'template'
            NextToken();

            if (not Consume(Tk::LParen))
                return Error(loc, "Expected template parameter list (beginning with `(`) following `template`");

            std::vector<TemplateExpr::Param> parameters{};
            while (not At(Tk::RParen)) {
                // We have a (maybe comma-separated) list of names followed by a type.
                usz idx = parameters.size();
                if (not At(Tk::Ident)) return Error(loc, "Expected identifier in template parameter declaration");
                do {
                    parameters.emplace_back(tok.text, nullptr, tok.location);
                    NextToken();
                    ConsumeExpressionSeparator(ExpressionSeparator::Soft);
                } while (At(Tk::Ident));

                /// Parse the parameter type.
                if (not Consume(Tk::Colon)) return Error("Expected ':' in template parameter declaration");
                auto type = ParseType();
                if (not type) return type.diag();

                /// Fixup the type for all the parameters.
                for (; idx < parameters.size(); ++idx) parameters[idx].type = *type;

                /// Discard trailing comma.
                ConsumeExpressionSeparator(ExpressionSeparator::Soft);
            }

            /// Yeet ')'.
            if (not Consume(Tk::RParen)) return Error("Expected ')' closing template parameter list");

            lhs = ParseExpr(current_precedence, true);
            if (not lhs) return lhs.diag();

            lhs = new (*mod) TemplateExpr(*lhs, parameters, {loc, lhs->location()});
        } break;

        case Tk::Print: {
            auto loc = tok.location;
            // Yeet `print`
            NextToken();

            auto exprs = ParseExpressionList();
            if (not exprs) return exprs.diag();

            auto& e = *exprs;
            // FIXME: We currently don't have "expression list" implemented, so we use
            // a call to store everything until Sema.
            lhs = new (*mod) CallExpr(
                new (*mod) NameRefExpr("__glintprint", GlobalScope(), loc),
                e,
                {loc, tok.location}
            );

        } break;

        case Tk::Match: {
            auto loc = tok.location;
            // Yeet `match`
            NextToken();

            auto object = ParseExpr(current_precedence, true);
            if (not object) return object.diag();

            lhs = new (*mod) MatchExpr(*object, {loc, object->location()});

            if (not Consume(Tk::LBrace))
                return Error(loc, "Expected block expression for match body following match object expression.");

            while (not At(Tk::RBrace)) {
                if (not Consume(Tk::Dot))
                    return Error(tok.location, "Expected `.` followed by sum type member identifier (or `}}`)");

                if (not At(Tk::Ident)) return Error("Expected identifier after .");
                auto name = tok.text;
                // Yeet name
                Consume(Tk::Ident);

                // Optional `:`
                Consume(Tk::Colon);

                auto body = ParseExpr();
                if (not body) return body.diag();
                // Optionally consume expression separator.
                ConsumeExpressionSeparator();

                as<MatchExpr>(*lhs)->add_match(name, *body);
            }

            if (not Consume(Tk::RBrace))
                return Error(loc, "Expected end brace for match body block expression.");
        } break;

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

        case Tk::Has: {
            auto loc = tok.location;
            // Yeet `has`
            NextToken();
            lhs = ParseExpr();
            if (not lhs) return lhs.diag();
            lhs = new (*mod) UnaryExpr(Tk::Has, *lhs, false, {loc, lhs->location()});
        } break;

        case Tk::Typeof: {
            auto loc = tok.location;
            auto ty = ParseType();
            if (not ty) return ty.diag();
            lhs = new (*mod) TypeExpr(
                *ty,
                {loc, ty->location()}
            );
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
        case Tk::Union:
        case Tk::Sum:
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
        case Tk::RangedFor: lhs = ParseRangedForExpr(); break;
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
            // Eat '('
            NextToken();

            // Empty expression isn't an expression, becomes no ast node. If we are
            // parsing an expression, we need to keep going (or return an empty
            // expression).
            // "(" ")" => ()
            // TODO: Should we return the empty expression if we get an empty
            // parenthetical expression? We currently have no way to do that.
            if (Consume(Tk::RParen))
                return Error("Empty parenthetical expression probably has unintended consequences: try `,` or `;`.");

            auto exprs = ParseExpressionsUntil(Tk::RParen);
            if (not exprs) return exprs.diag();

            // FIXME: This is an issue; basically, we are doing semantic analysis
            // within syntactic analysis, and it's making things /messy/.
            // This is obviously semantics and has nothing to do with parsing
            // syntactically; we /should/ have an expression list node that is invalid
            // if it shows up in the final program but may be converted by semantic
            // analysis into valid code. i.e. expression list containing all integers
            // would multiply all of them together, but an expression list containing
            // a function followed by integers would be a call to that functions with
            // the integers as arguments.
            {
                auto& e = *exprs;
                if (e.size() == 1) {
                    lhs = e.at(0);
                } else if (e.size() > 1) {
                    // expression list becomes call
                    // (foo x y) -> foo(x,y)
                    lhs = new (*mod) CallExpr(
                        e.at(0),
                        {e.begin() + 1, e.end()},
                        {e.at(0)->location(), tok.location}
                    );
                } else {
                    // non-empty empty ()
                    // Basically, we only ever get here if we have a paren expression with
                    // hard separators or other empty paren expressions inside.
                    return Error("Empty parenthetical expression probably has unintended consequences.");
                }
            }

            if (not Consume(Tk::RParen)) return Error("Expected )");
        } break;

        /// Unary operators.
        case Tk::Ampersand:
        case Tk::At:
        case Tk::Exclam:
        case Tk::Minus:
        case Tk::Tilde:
        case TokenKind::MinusMinus:
        case TokenKind::PlusPlus: {
            auto loc = tok.location;
            auto op = tok.kind;
            NextToken();
            lhs = ParseExpr(PrefixOperatorPrecedence) >>= [&](Expr* operand) {
                return new (*mod) UnaryExpr(op, operand, false, {loc, tok.location});
            };
        } break;

        // Lambda expression.
        case Tk::Lambda: {
            auto loc = tok.location;
            // Yeet 'lambda'
            NextToken();

            auto ty = ParseType();
            if (not ty) return ty.diag();

            auto* func_ty = cast<FuncType>(*ty);
            if (not func_ty)
                return Error("Type of lambda must be a function type");

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
            NextToken(); /// Yeet "!{".

            /// Parse the elements.
            std::vector<CompoundLiteral::Member> elements{};
            std::string name{};
            while (not At(Tk::RBrace, Tk::Eof)) {
                name.clear();
                if (Consume(Tk::Dot)) {
                    if (not At(Tk::Ident)) {
                        return Error(
                            "Recognized named expression in compound literal, but the following token was not an identifier as expected. It was {}",
                            ToString(tok.kind)
                        );
                    }
                    name = tok.text;

                    NextToken(); // yeet identifier
                }
                auto element = ParseExpr(0, true);
                if (not element) return element.diag();
                elements.emplace_back(name, element.value());
                ConsumeExpressionSeparator(ExpressionSeparator::Soft);
            }

            /// Yeet "}".
            if (not Consume(Tk::RBrace))
                return Error("Expected }}");

            lhs = new (*mod) CompoundLiteral(std::move(elements), tok.location);
        } break;

        case TokenKind::Comma:
        case TokenKind::RParen:
        case TokenKind::RBrack:
        case TokenKind::RBrace:
        case TokenKind::Dot:
        case TokenKind::Plus:
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
        case TokenKind::StarStar:
        case TokenKind::PlusEq:
        case TokenKind::MinusEq:
        case TokenKind::StarEq:
        case TokenKind::SlashEq:
        case TokenKind::PercentEq:
        case TokenKind::AmpersandEq:
        case TokenKind::PipeEq:
        case TokenKind::CaretEq:
        case TokenKind::TildeEq:
        case TokenKind::LBrackEq:
        case TokenKind::RightArrow:
        case TokenKind::And:
        case TokenKind::Or:
        case TokenKind::Invalid:
        case TokenKind::Eof:
        case TokenKind::Supplant:
            return Error("Expected expression, got {}", ToString(tok.kind));

        case TokenKind::Star:
            return Error(
                "Expected expression, got {}. You likely meant to use '{}' to dereference.",
                ToString(tok.kind),
                ToString(Tk::At)
            );

        case TokenKind::MacroArg:
        case TokenKind::Semicolon:
            Note("Here");
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

    // If the LHS is a diag, return.
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
    // being parsed (i.e. a comma can be used to separate function arguments,
    // but not a function and it's arguments). Hope that makes sense.

    // Eat soft expression separator
    if (+ConsumeExpressionSeparator(ExpressionSeparator::Soft))
        return lhs;

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
    const auto ParseBinaryOps = [&]() -> Result<void> {
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
        return {};
    };

    if (auto r = ParseBinaryOps(); r.is_diag())
        return r.diag();

    // "()" after an expression is always a call with no arguments to the
    // preceding expression.
    if (At(Tk::LParen) and Is(LookAhead(1), Tk::RParen)) {
        lhs = new (*mod) CallExpr(
            lhs.value(),
            {},
            {lhs->location(), LookAhead(1)->location}
        );

        /// Yeet "(" and ")".
        NextToken();
        NextToken();
    }

    if (+ConsumeExpressionSeparator(ExpressionSeparator::Soft))
        return lhs;

    /// While we’re at the start of an expression, if we’re not parsing
    /// a single-expression, parse call arguments.
    // Exception: parsed expression (would-be callee) must be an identifier or
    // a lambda or a number or a template.
    else if (
        not single_expression
        and (is<NameRefExpr>(*lhs) or is<TypeExpr>(*lhs) or is<IntegerLiteral>(*lhs) or is<TemplateExpr>(*lhs) or (is<FuncDecl>(*lhs) and as<FuncDecl>(*lhs)->name().empty()))
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
            // Warning on multi-line call
            if (lhs->location().seekable(context) and tok.location.seekable(context)) {
                auto callee_location = lhs->location().seek_line_column(context);
                auto current_location = tok.location.seek_line_column(context);
                if (callee_location.line != current_location.line)
                    Warning("Multi-line Call; Did you forget an expression separator?");
            }
            lhs = new (*mod) CallExpr(
                lhs.value(),
                std::move(args),
                {lhs->location(), tok.location}
            );
        }
    }

    // Once again try to parse binary operators...
    if (auto r = ParseBinaryOps(); r.is_diag())
        return r.diag();

    // Eat any amount of commas following an expression
    while (+ConsumeExpressionSeparator(ExpressionSeparator::Soft));

    return lhs;
}

auto lcc::glint::Parser::ParseExprInNewScope() -> ExprResult {
    ScopeRAII sc{this};
    return ParseExpr();
}

auto lcc::glint::Parser::ParseForExpr() -> Result<ForExpr*> {
    auto loc = tok.location;
    LCC_ASSERT(Consume(Tk::For), "ParseForExpr called while not at 'for'");

    // Optionally consume expression separator between
    ConsumeExpressionSeparator();

    /// Parse init, cond, increment, and body.
    ScopeRAII sc{this};

    auto init = ParseExpr();
    if (not init) return init.diag();
    if (not +ConsumeExpressionSeparator(ExpressionSeparator::Hard))
        Error("Expected expression separator after 'for' initialisation expression");

    auto cond = ParseExpr();
    if (not cond) return cond.diag();
    if (not +ConsumeExpressionSeparator(ExpressionSeparator::Hard))
        Error("Expected expression separator after 'for' condition expression");

    auto increment = ParseExpr();
    if (not increment) return increment.diag();
    if (not +ConsumeExpressionSeparator(ExpressionSeparator::Hard))
        Error("Expected expression separator after 'for' increment expression");

    auto body = ParseExpr();
    if (not body) return body.diag();

    /// Check for errors and create the expression.
    return new (*mod) ForExpr(*init, *cond, *increment, *body, loc);
}

auto lcc::glint::Parser::ParseRangedForExpr() -> Result<ForExpr*> {
    auto loc = tok.location;
    LCC_ASSERT(Consume(Tk::RangedFor), "ParseRangedForExpr called while not at 'for'");

    /// Parse loop variable identifier, container expression, and body.
    ScopeRAII sc{this};
    auto scope = sc.scope;

    if (not At(Tk::Ident))
        return Error("Expected identifier (name of loop variable) following 'for'");

    auto loop_var = tok.text;
    // Eat loop variable identifier.
    NextToken();

    // Expect "in" contextual keyword.
    if (not At(Tk::Ident) or tok.text != "in")
        return Error("Expected contextual keyword 'in' following loop variable name in ranged 'for'");
    // Eat "in"
    NextToken();

    auto container = ParseExpr();
    if (not container) return container.diag();

    // NOTE: If the user forgets to separate the container and the body, the
    // container expression will end up being a call with the body as an
    // argument.

    // Eat optional comma separating container and body.
    // If we don't do this here, ParseExpr gets confused and thinks we are
    // parsing an empty expression.
    ConsumeExpressionSeparator(ExpressionSeparator::Soft);

    auto body = ParseExpr();
    if (not body) return body.diag();

    // TODO: We may want to create a RangedForExpr AST node and do this
    // transformation in Sema using sema templates...

    // Create init, condition, and increment expressions from loop variable
    // identifier and container expression.

    // Calculate end pointer (subscript of member access .data on container by
    // member access .size on container)
    auto member_access_data = new (*mod) MemberAccessExpr(*container, "data", loc);
    auto member_access_size = new (*mod) MemberAccessExpr(*container, "size", loc);
    auto end_pointer = new (*mod) BinaryExpr(Tk::LBrack, member_access_data, member_access_size, loc);
    auto end_name = mod->unique_name("rangedforend_");
    auto end = new (*mod) VarDecl(
        end_name,
        Type::Unknown,
        end_pointer,
        mod,
        Linkage::LocalVar,
        loc
    );
    auto end_decl
        = scope->declare(context, std::move(end_name), end);
    LCC_ASSERT(end_decl);

    // init: declaration of generated symbol assigned to member access ".data" on container
    auto iter_name = mod->unique_name("rangedforiter_");
    auto init = new (*mod) VarDecl(
        iter_name,
        Type::Unknown,
        new (*mod) MemberAccessExpr(*container, "data", loc),
        mod,
        Linkage::LocalVar,
        loc
    );
    auto init_decl
        = scope->declare(context, std::move(iter_name), init);
    LCC_ASSERT(init_decl);

    auto init_block = new (*mod) BlockExpr({end, init}, loc);

    // namerefexpr to init declaration != calculated end pointer
    auto cond = new (*mod) BinaryExpr(
        Tk::Ne,
        new (*mod) NameRefExpr(init_decl->name(), scope, loc),
        new (*mod) NameRefExpr(end_decl->name(), scope, loc),
        loc
    );

    // increment: assign init to subscript 1 of namerefexpr to init declaration.
    auto increment = new (*mod) BinaryExpr(
        Tk::LBrackEq,
        new (*mod) NameRefExpr(init_decl->name(), scope, loc),
        new (*mod) IntegerLiteral(1, loc),
        loc
    );

    // body: block expression containing loop_var declaration followed by
    // parsed body expression.
    auto deref_iter = new (*mod) UnaryExpr(
        Tk::At,
        new (*mod) NameRefExpr(init_decl->name(), scope, loc),
        false,
        loc
    );
    // TODO: type of loop variable should be reference to container element
    // type. Because we are doing this all syntactically, however, we don't
    // yet have that information.
    auto loop_var_vardecl
        = new (*mod) VarDecl(
            loop_var,
            Type::Unknown,
            deref_iter,
            mod,
            Linkage::LocalVar,
            loc
        );
    auto loop_var_decl
        = DeclScope(loop_var_vardecl->linkage() == Linkage::LocalVar)
              ->declare(context, std::move(loop_var), loop_var_vardecl);
    LCC_ASSERT(loop_var_decl);

    auto block_body = new (*mod) BlockExpr(
        {*loop_var_decl, *body},
        loc
    );

    return new (*mod) ForExpr(init_block, cond, increment, block_body, loc);
}

auto lcc::glint::Parser::ParseFuncAttrs() -> Result<FuncType::Attributes> {
    static const StringMap<FuncAttr> attrs_map{
        {"const", FuncAttr::Const},
        {"discardable", FuncAttr::Discardable},
        {"flatten", FuncAttr::Flatten},
        {"inline", FuncAttr::Inline},
        {"noinline", FuncAttr::NoInline},
        {"nomangle", FuncAttr::NoMangle},
        {"noreturn", FuncAttr::NoReturn},
        {"pure", FuncAttr::Pure},
        {"returns_twice", FuncAttr::ReturnsTwice}, // i.e. fork()
        {"used", FuncAttr::Used},
        {"__noopt__", FuncAttr::NoOpt}, // not part of Glint, but rather a compiler extension
    };

    /// Parse attributes while we’re at an identifier.
    FuncType::Attributes attrs;
    for (;;) {
        if (not At(Tk::Ident)) return attrs;
        auto it = attrs_map.find(tok.text);
        if (it == attrs_map.end()) return attrs;
        if (attrs[it->second]) Warning(tok.location, "Duplicate attribute ignored");
        attrs[it->second] = true;
        NextToken();
    }
}

auto lcc::glint::Parser::ParseFuncBody(bool is_external) -> Result<std::pair<Expr*, Scope*>> {
    /// If the declaration is external, but there still seems to be
    /// a function body, warn the the user that they might be trying
    /// to do something that doesn’t make sense.
    if (is_external) {
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
            return Warning(
                tok.location,
                "External functions cannot have a body. If this '{{' is not supposed to "
                "start a function body, consider adding a ';' after the function type"
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

    // Eat '=', if present.
    Consume(Tk::Eq);
    if (At(Tk::LBrace)) expr = ParseBlock(std::move(sc));
    else expr = ParseExpr();

    if (expr.is_diag()) return expr.diag();
    return std::pair{*expr, scope};
}

auto lcc::glint::Parser::ParseFuncSig(Type* return_type) -> Result<FuncType*> {
    LCC_ASSERT(Consume(Tk::LParen), "ParseFunctionSignature called while not at '('");

    // Parse parameters.
    std::vector<FuncType::Param> parameters;
    while (not At(Tk::RParen)) {
        /// If we’re at a colon, this parameter is unnamed.
        if (Consume(Tk::Colon)) {
            auto type = ParseType();
            if (not type) return type.diag();
            parameters.emplace_back("", *type, type->location());

        }

        // Otherwise, we have a (maybe comma-separated) list of names followed by
        // a type.
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

auto lcc::glint::Parser::ParseIdentExpr() -> Result<Expr*> {
    auto loc = tok.location;
    auto text = tok.text;
    LCC_ASSERT(Consume(Tk::Ident), "ParseIdentExpr called while not at identifier");

    if (tok.from_macro) {
        auto found = CurrScope()->find(text);
        if (not found.empty()) {
            auto decl = found.at(0);
            auto err = Error(
                loc,
                "Refusing to expand unhygienic expansion of macro due to emitted identifier ``{}'' matching that of a previously declared object.\n"
                "Recommended solution is to add ``defines {}'' to the macro definition, before the emits keyword.",
                text,
                text
            );
            err.attach(Note(decl->location(), "Original declaration here"));
            return err;
        }
    }

    /// If the next token is ':' or '::', then this is a declaration.
    if (At(Tk::Colon, Tk::ColonColon)) return ParseDeclRest(std::move(text), loc, false);

    /// Otherwise, it’s just a name.
    return new (*mod) NameRefExpr(std::move(text), CurrScope(), loc);
}

auto lcc::glint::Parser::ParseIfExpr() -> Result<IfExpr*> {
    auto loc = tok.location;
    /// Yeet "if".
    LCC_ASSERT(Consume(Tk::If), "ParseIf called while not at 'if'");

    /// Parse condition, then, and else.
    auto cond = ParseExpr();

    // NOTE: We "require" a soft expression separator here in the grammar, but
    // we don't actually consume it /here/. We consume it as the last part of
    // parsing the condition expression.

    // If at hard expression separator, check for the common error case of a
    // condition and then body missing a separator.
    if (At(TokenKind::Semicolon)) {
        if (cond and cond->kind() == Expr::Kind::Call) {
            auto call = as<CallExpr>(*cond);
            if (call->args().size() == 1) {
                auto e = Error(
                    call->location(),
                    "This if condition is a suspicious call, and there is no 'then' expression..."
                );
                e.attach(Note(
                    GetPastLocation(call->callee()),
                    "Maybe you forgot an expression separator right here"
                ));
                return e;
            }
        }
    }

    // Following condition, accept an optional expression separator
    ConsumeExpressionSeparator();

    auto then = ParseExprInNewScope();
    // Following then expression, accept an optional expression separator.
    // We have to use lookahead here because if we ate a separator
    // unconditionally but weren't subsequently at an 'else', the top level
    // would report a warning about a missing semi-colon when there isn't one.
    // Basically, ParseExpr mustn't eat a hard expression separator past the
    // end of the expression (like one that terminates it).
    //   foo;
    //      ^ don't eat this
    if (LookAhead(1)->kind == Tk::Else) {
        if (not +ConsumeExpressionSeparator()) {
            Note(LookAhead(1)->location, "Here");
            return Error(
                loc,
                "Expected expression separator (like a semicolon or comma)"
                " between then expression and 'else' of this if, but got {}",
                ToString(LookAhead(1)->kind)
            );
        }
    }

    auto else_ = ExprResult::Null();
    if (Consume(Tk::Else))
        else_ = ParseExpr();

    if (IsError(cond, then, else_))
        return GetDiag(cond, then, else_).diag();

    return new (*mod) IfExpr(cond.value(), then.value(), else_.value(), loc);
}

auto lcc::glint::Parser::ParsePreamble(File* f) -> Result<std::unique_ptr<Module>> {
    // Parse module name and create the module.

    // TODO: We probably want to implement a compiler directive that a module
    // can set to turn this on or off for itself. Oh yeah, that reminds me,
    // also a way to change the code that's inserted based on the LCC target.
    //
    //     *^ sum_type_bad_access_check [ true | false ];
    //
    // You know what I actually like more than that, I think? What if the act
    // of installing a handler is the act of enabling it. That way we also
    // don't have to do stupid by-hand code generation for the case, as well,
    // we just call the function they define. To me, it makes sense to be
    // per-module (per-library, in interop terms), because each module's
    // maintainer should have the ability to control the code they maintain.
    // So, a well-tested and thoroughly debugged library can omit the handler
    // in production while a new Glint programmer who wants to make sure they
    // know about any weirdness going on can install a classic "print message
    // and crash" handler. This is sort of like operator overloading but on a
    // module, I guess, lol.
    std::string module_name;
    auto module_kind = Module::IsAnExecutable;

    if (At(Tk::Ident) and tok.text == "module" and not tok.artificial) {
        module_kind = Module::IsAModule;
        NextToken(); /// Yeet "module".

        if (not At(Tk::Ident)) return Error("Expected module name");
        module_name = tok.text;
        NextToken(); /// Yeet module name.
    }

    auto m = std::make_unique<Module>(
        f,
        module_name,
        module_kind
    );
    mod = m.get();

    while (+ConsumeExpressionSeparator(ExpressionSeparator::Hard));

    // Parse imports.
    while (At(Tk::Ident) and tok.text == "import" and not tok.artificial) {
        Location loc = tok.location;
        NextToken(); /// Yeet "import".
        if (not At(Tk::Ident, Tk::String)) return Error("Expected module name after import");

        // Add the module to be loaded later.
        mod->add_import(tok.text, {loc, tok.location});
        NextToken(); // Yeet module name.

        while (+ConsumeExpressionSeparator(ExpressionSeparator::Hard));
    }

    return m;
}

auto lcc::glint::Parser::ParseStructType() -> Result<StructType*> {
    auto loc = tok.location;
    LCC_ASSERT(Consume(Tk::Struct), "ParseStructType called while not at 'struct'");
    if (not Consume(Tk::LBrace)) return Error("Expected '{{' after 'struct' in struct declaration");

    /// Parse the struct body.
    ScopeRAII sc{this};
    std::vector<StructType::Member> members;
    while (not At(Tk::RBrace)) {
        auto start = tok.location;

        // Supplanted Declaration
        if (Consume(Tk::Supplant)) {
            auto type = ParseType();
            if (not type) return type.diag();

            // Make up unique name with supplant marker prefix
            auto name = std::string("__sup") + std::to_string(members.size());
            members.emplace_back(std::move(name), *type, Location{start, type->location()});
            members.back().supplanted = true;
        } else {
            // Regular Declaration

            /// Name.
            auto name = tok.text;
            if (not Consume(Tk::Ident)) return Error("Expected member name in struct declaration");

            /// Type.
            if (not Consume(Tk::Colon)) return Error("Expected ':' in struct declaration");
            auto type = ParseType();
            if (not type) return type.diag();

            /// Add the member to the list.
            members.emplace_back(std::move(name), *type, Location{start, type->location()});
        }

        // Optionally eat expression separator
        ConsumeExpressionSeparator();
    }

    /// Yeet '}'.
    if (not Consume(Tk::RBrace)) return Error("Expected '}}' in struct declaration");

    /// Create the struct type.
    return new (*mod) StructType(sc.scope, std::move(members), Location{loc, tok.location});
}

auto lcc::glint::Parser::ParseUnionType() -> Result<UnionType*> {
    auto loc = tok.location;
    LCC_ASSERT(Consume(Tk::Union), "ParseUnionType called while not at '{}'", ToString(Tk::Union));
    if (not Consume(Tk::LBrace)) {
        return Error(
            "Expected '{}' after '{}' in union declaration",
            ToString(Tk::LBrace),
            ToString(Tk::Union)
        );
    }

    /// Parse the union body.
    ScopeRAII sc{this};
    std::vector<UnionType::Member> members;
    while (not At(Tk::RBrace)) {
        /// Name.
        auto name = tok.text;
        auto start = tok.location;
        if (not Consume(Tk::Ident))
            return Error("Expected member name in union declaration");

        /// Type.
        if (not Consume(Tk::Colon)) return Error("Expected ':' in union declaration");
        auto type = ParseType();
        if (not type) return type.diag();

        /// Add the member to the list.
        members.emplace_back(
            std::move(name),
            *type,
            Location{start, type->location()}
        );

        // Optionally eat soft or hard separator.
        ConsumeExpressionSeparator();
    }

    /// Yeet '}'.
    if (not Consume(Tk::RBrace))
        return Error("Expected '' in union declaration", ToString(Tk::RBrace));

    /// Create the type.
    return new (*mod) UnionType(sc.scope, std::move(members), Location{loc, tok.location});
}

auto lcc::glint::Parser::ParseSumType() -> Result<SumType*> {
    auto loc = tok.location;
    LCC_ASSERT(Consume(Tk::Sum), "ParseSumType called while not at 'sum'");
    if (not Consume(Tk::LBrace)) return Error("Expected '{{' after 'sum'");

    /// Parse the struct body.
    ScopeRAII sc{this};
    std::vector<SumType::Member> members;
    while (not At(Tk::RBrace)) {
        // Name
        auto name = tok.text;
        auto start = tok.location;
        if (not Consume(Tk::Ident)) return Error("Expected member name in sum type declaration");

        // Type
        if (not Consume(Tk::Colon)) return Error("Expected ':' in sum type declaration");
        auto type = ParseType();
        if (not type) return type.diag();

        // Default expression
        Expr* default_expression{};

        // TODO: I don't think this is how sum types will work anymore (it would
        // be cool if it was an option given all subtypes of a sum type have an
        // evaluatable default expression). That is, accessing a member of a sum
        // type doesn't need to /always/ return a default value if the access is
        // bad; after all, this /should/ never happen in production code, as all
        // the bugs were ironed out during development... Basically, I want for
        // people to be able to customise the behaviour of a bad access vs
        // requiring only good accesses.
        // TODO: The real todo from all that is:
        //     "make init expression an Expr* instead of EvalResult".

        // Eat '=', if present
        if (Consume(Tk::Eq)) {
            auto init = ParseExpr();
            if (not init) return init.diag();
            default_expression = *init;

            // Attempt to constant evaluate the expression
            EvalResult out{};
            if (init->evaluate(context, out, false))
                default_expression = new (*mod) ConstantExpr(default_expression, out);
        }

        // Add the member to the list.
        members.emplace_back(
            std::move(name),
            *type,
            default_expression,
            Location{start, type->location()}
        );

        // Optionally eat soft or hard separator
        ConsumeExpressionSeparator();
    }

    /// Yeet '}'.
    if (not Consume(Tk::RBrace)) return Error("Expected '}}' in sum type declaration");

    /// Create the struct type.
    return new (*mod) SumType(sc.scope, std::move(members), Location{loc, tok.location});
}

/// Parse a type where a type is expected.
auto lcc::glint::Parser::ParseType(isz current_precedence) -> Result<Type*> {
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

        case Tk::UInt:
            ty = BuiltinType::UInt(*mod, tok.location);
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

        case Tk::Typeof: {
            auto loc = tok.location;
            NextToken();
            auto expr = ParseExpr();
            if (not expr) return expr.diag();
            ty = new (*mod) TypeofType(*expr, loc);
        } break;

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
            if (auto type = ParseStructType())
                ty = *type;
            else return type.diag();
            break;

        /// Enumeration type.
        case Tk::Enum:
            if (auto type = ParseEnumType())
                ty = *type;
            else return type.diag();
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
                if (At(Tk::Ident) and tok.text == "view") {
                    NextToken();
                    ty = new (*mod) ArrayViewType(*type, location);
                } else {
                    auto size_expr = ParseExpr();
                    if (not size_expr) return size_expr.diag();
                    EvalResult out{};
                    if (size_expr->evaluate(context, out, false)) {
                        // FIXED ARRAY because size expression is compile-time-known.
                        size_expr = new (*mod) ConstantExpr(*size_expr, out);
                        ty = new (*mod) ArrayType(*type, *size_expr, type->location());
                    } else {
                        // DYNAMIC ARRAY because size expression is NOT compile-time-known.
                        ty = new (*mod) DynamicArrayType(
                            *type,
                            *size_expr,
                            location
                        );
                    }
                }

                if (not Consume(Tk::RBrack))
                    return Error("Expected ]");
            }
        } break;

        case Tk::Union: {
            if (auto type = ParseUnionType())
                ty = *type;
            else return type.diag();
        } break;

        case Tk::Sum: {
            if (auto type = ParseSumType())
                ty = *type;
            else return type.diag();
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

                location = {location, tok.location};
                if (tok.text == "pptr") {
                    ty = new (*mod) PointerType(
                        new (*mod) PointerType(ty, location)
                    );
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

auto lcc::glint::Parser::ParseWhileExpr() -> Result<WhileExpr*> {
    /// Yeet "while".
    auto loc = tok.location;
    LCC_ASSERT(Consume(Tk::While), "ParseWhile called while not at 'while'");

    /// Parse condition and body.
    auto cond = ParseExpr();
    auto body = ParseExprInNewScope();
    if (IsError(cond, body)) return GetDiag(cond, body).diag();
    return new (*mod) WhileExpr(cond.value(), body.value(), loc);
}

void lcc::glint::Parser::Synchronise() {
    while (not At(Tk::Semicolon, Tk::RBrace, Tk::Eof)) NextToken();
    // Skip to where the start of the next expression would be.
    while (At(Tk::Semicolon, Tk::RBrace)) NextToken();
}

/// Parse a function declaration.
///
/// \param name Function name. If empty, this is an anonymous function.
/// \param type The type of the function.
/// \param is_external Whether this is an external function.
/// \return The decl or an error.
auto lcc::glint::Parser::ParseFuncDecl(
    std::string name,
    FuncType* type,
    bool is_external
) -> Result<FuncDecl*> {
    /// Parse attributes and the function body.
    auto body = ParseFuncBody(is_external);
    if (not body) return body.diag();

    // External implies no mangling.
    if (is_external) type->set_attr(FuncAttr::NoMangle);

    /// Create the function.
    auto* func = new (*mod) FuncDecl(
        name,
        type,
        (*body).first,
        (*body).second,
        mod,
        is_external ? Linkage::Imported : Linkage::Internal,
        type->location()
    );

    /// If the function is anonymous, then this is a lambda.
    if (name.empty()) return func;

    /// Add it to the current scope and return it.
    return as<FuncDecl>(DeclScope()->declare(context, std::move(name), func));
}

void lcc::glint::Parser::ParseTopLevel() {
    curr_func = mod->top_level_function();

    /// Create the global and top-level scope. The top-level scope is a bit of
    /// a weird one because it only contains the local variables of the top-level
    /// functions. Everything else at the top-level goes in the global scope.
    auto* global = new (*mod) Scope(nullptr);
    auto* top_level = new (*mod) Scope(global);

    /// Set up the rest of the parser state.
    scope_stack.push_back(global);
    scope_stack.push_back(top_level);
    curr_func->scope(TopLevelScope());

    /// Parse the file.
    for (;;) {
        while (+ConsumeExpressionSeparator());

        /// Stop if we’re at end of file.
        if (At(Tk::Eof)) break;

        /// Parse a top-level expression.
        auto expr = ParseExpr();
        if (not +ConsumeExpressionSeparator(ExpressionSeparator::Hard)) {
            if (At(Tk::Eof)) {
                Warning("Expected hard expression separator but got end of file");
            } else if (expr) {
                Warning(GetRightmostLocation(*expr), "Expected hard expression separator")
                    .attach(Note("Before this"));
            }
        }
        if (expr)
            mod->add_top_level_expr(expr.value());
        else {
            // If we failed to parse an expression at the top level, there was an error.
            context->set_error();
            // Jump past the next semicolon or closing brace in case of an error.
            Synchronise();
        }
    }
}

auto lcc::glint::Parser::Parse(Context* context, std::string_view source) -> std::unique_ptr<Module> {
    Parser parser(context, source);

    /// Parse preamble. This also creates the module.
    auto maybe_m = parser.ParsePreamble(nullptr);
    if (not maybe_m) return {};
    auto m = std::move(*maybe_m);
    if (not m) return {};

    /// Parse Glint source.
    parser.ParseTopLevel();

    /// Return nullptr on error.
    if (context->has_error())
        return {};

    return m;
}

auto lcc::glint::Parser::Parse(Context* context, File& file) -> std::unique_ptr<Module> {
    Parser parser(context, &file);

    /// Parse preamble. This also creates the module.
    auto maybe_m = parser.ParsePreamble(&file);
    if (not maybe_m) return {};
    auto m = std::move(*maybe_m);
    if (not m) return {};

    /// Parse Glint source.
    parser.ParseTopLevel();

    /// Return nullptr on error.
    if (context->has_error())
        return std::unique_ptr<Module>{};

    return m;
}

auto lcc::glint::Parser::ParseFreestanding(
    Module& m,
    Context* context,
    File& file,
    Scope* starting_scope
) -> Result<std::vector<lcc::glint::Expr*>> {
    Parser parser(context, &file);
    // Initialize parser
    parser.mod = &m;
    parser.curr_func = m.top_level_function();
    // Construct scope stack
    for (auto s = starting_scope; s; s = s->parent())
        parser.scope_stack.emplace(parser.scope_stack.begin(), s);

    /// Parse the file.
    std::vector<Expr*> exprs{};
    for (;;) {
        while (+parser.ConsumeExpressionSeparator());

        /// Stop if we’re at end of file.
        if (parser.At(Tk::Eof)) break;

        /// Parse a top-level expression.
        auto expr = parser.ParseExpr();
        if (not +parser.ConsumeExpressionSeparator(ExpressionSeparator::Hard)) {
            if (parser.At(Tk::Eof)) {
                parser.Warning("Expected hard expression separator but got end of file");
            } else if (expr) {
                parser.Warning(GetRightmostLocation(*expr), "Expected hard expression separator")
                    .attach(parser.Note("Before this"));
            }
        }
        if (expr)
            exprs.emplace_back(*expr);
        else return expr.diag();
    }

    return exprs;
}

auto lcc::glint::Parser::JustGetTokens() -> std::vector<GlintToken> {
    std::vector<GlintToken> out{};
    while (not At(Tk::Eof)) {
        out.emplace_back(tok);
        NextToken();
    }
    return out;
}

// Doesn't do any parsing, just collects tokens.
auto lcc::glint::Parser::GetTokens(Context* context, File& file) -> std::vector<GlintToken> {
    Parser parser(context, &file);
    return parser.JustGetTokens();
}
