#include <filesystem>
#include <glint/ast.hh>
#include <glint/module_description.hh>
#include <glint/sema.hh>
#include <lcc/context.hh>
#include <lcc/utils.hh>
#include <lcc/utils/macros.hh>
#include <object/elf.h>
#include <object/elf.hh>

/// ===========================================================================
///  Helpers
/// ===========================================================================
bool lcc::glint::Sema::AnalyseAndDiscard(Expr** expr) {
    if (not Analyse(expr)) return false;
    Discard(expr);
    return true;
}

bool lcc::glint::Sema::Convert(Expr** expr, Type* type) {
    if ((*expr)->sema_errored()) return true;
    return ConvertImpl<true>(expr, type) >= 0;
}

/// For an explanation of the return value of this function, see
/// the comment on the declaration of TryConvert().
template <bool PerformConversion>
int lcc::glint::Sema::ConvertImpl(lcc::glint::Expr** expr_ptr, lcc::glint::Type* to) {
    enum : int {
        TypesContainErrors = -2,
        ConversionImpossible = -1,
        NoOp = 0,
    };

    /// Cannot convert if the types contain errors.
    auto from = (*expr_ptr)->type();
    if (from->sema_errored() or to->sema_errored()) return TypesContainErrors;

    /// This is so we don’t forget that we’ve applied lvalue-to-rvalue
    /// conversion and raised the score by one.
    int score = 0;
    auto Score = [&](int i) {
        LCC_ASSERT(i, "Score must be 1 or greater. Use the enum constants above for values <= 0");
        return i + int(score);
    };

    /// Any type can be converted to void.
    if (to->is_void()) return NoOp;

    /// Any type can be converted to itself.
    if (Type::Equal(from, to)) return NoOp;

    /// All conversions beside reference binding require lvalue-to-rvalue conversion.
    if (to->is_reference() and Type::Equal(from, to->elem())) {
        if ((*expr_ptr)->is_lvalue()) {
            if constexpr (PerformConversion) WrapWithCast(expr_ptr, to, CastKind::LValueToReference);
            return NoOp;
        }

        return ConversionImpossible;
    }

    /// Lvalue to rvalue conversion is required.
    score += (*expr_ptr)->is_lvalue();
    if constexpr (PerformConversion) LValueToRValue(expr_ptr, false);

    /// Get reference-to-reference conversions out of the way early.
    if (from->is_reference() and to->is_reference()) {
        /// A reference can be converted to the same reference.
        if (Type::Equal(from, to)) return NoOp;

        /// References to arrays can be converted to references to
        /// the first element.
        auto arr = cast<ArrayType>(from->elem());
        if (arr and Type::Equal(arr->element_type(), to->elem())) {
            if constexpr (PerformConversion) InsertImplicitCast(expr_ptr, to);
            return Score(1);
        }

        return ConversionImpossible;
    }

    /// Strip reference from `from` if need be.
    if (auto ref = cast<ReferenceType>(from)) {
        from = ref->element_type();
        score += 1;
        if constexpr (PerformConversion) LValueToRValue(expr_ptr);
    }

    /// Function types can be converted to their corresponding function types.
    if (from->is_function() and to->is_pointer() and Type::Equal(to->elem(), from)) {
        if constexpr (PerformConversion) InsertImplicitCast(expr_ptr, to);
        return NoOp;
    }

    /// Try deproceduring.
    if (Deproceduring(expr_ptr)) return Score(1);

    /// Now check if the types are equal. In many cases, lvalue-to-rvalue
    /// conversion is all we need.
    if (Type::Equal(from, to)) return NoOp;

    /// Pointer to pointer conversions.
    if (from->is_pointer() and to->is_pointer()) {
        /// Pointers to arrays are convertible to pointers to the first element.
        auto arr = cast<ArrayType>(from->elem());
        if (arr and Type::Equal(arr->element_type(), to->elem())) {
            if constexpr (PerformConversion) InsertImplicitCast(expr_ptr, to);
            return Score(1);
        }

        /// Any pointer is convertible to `@void`.
        if (Type::Equal(to, Type::VoidPtr)) {
            if constexpr (PerformConversion) InsertImplicitCast(expr_ptr, to);
            return Score(1);
        }
    }

    // Array to array conversions.
    if (from->is_array() and to->is_array()) {
        auto from_arr = as<ArrayType>(from);
        auto to_arr = as<ArrayType>(to);

        // If the array we are converting from is larger than the resulting array,
        // it wouldn't fit and that conversion is impossible.
        if (from_arr->dimension() > to_arr->dimension())
            return ConversionImpossible;

        // FIXME: We kind of need to check that the base types are convertible,
        // but, uhhh, we can't really do that right now without an expression of
        // that type due to how Convert works ... I wonder what idiot built it
        // that way.

        if constexpr (PerformConversion) InsertImplicitCast(expr_ptr, to);
        return Score(1);
    }

    /// Function types can be converted to their corresponding function types.
    if (from->is_function() and to->is_pointer() and Type::Equal(to->elem(), from)) {
        if constexpr (PerformConversion) InsertImplicitCast(expr_ptr, to);
        return NoOp;
    }

    /// Integer to boolean and vis versa implicit conversions.
    if ((from->is_integer() and to->is_bool()) or (from->is_bool() and to->is_integer())) {
        if constexpr (PerformConversion) InsertImplicitCast(expr_ptr, to);
        return Score(1);
    }

    /// Integer to integer
    ///
    /// For portability, we would ideally not make any assumptions about
    /// the size of `int`, but the issue with that is that it would make
    /// most code rather cumbersome to write as you’d have to, e.g., cast
    /// an `i16` to `int` manually. C FFI types suffer from similar problems,
    /// so we just use their width on the target.
    if (from->is_integer() and to->is_integer()) {
        // Integer types are always convertible to each other if the value is
        // known at compile time and in range for the type it is being converted
        // to.
        EvalResult res;
        if ((*expr_ptr)->evaluate(context, res, false)) {
            /// Note: We currently don’t support integer constants larger than 64
            /// bits internally, so if the type has a bit width larger than 64, it
            /// will always fit.
            auto val = res.as_int();

            /// Signed to Unsigned Conversion
            if (val.slt(0) and to->is_unsigned_int(context)) return ConversionImpossible;

            /// Unsigned to Unsigned Conversion
            auto bits = to->size(context);
            if (from->is_unsigned_int(context)
                and bits < 64
                and val > u64(utils::MaxBitValue(bits)))
                return ConversionImpossible;

            if constexpr (PerformConversion) {
                InsertImplicitCast(expr_ptr, to);
                *expr_ptr = new (mod) ConstantExpr(*expr_ptr, res);
            }
            return Score(1);
        }

        // Otherwise, if not known at compile-time, we will just go by what doesn'
        // t cause a memory error. If it fits, it ships.
        if (from->size(context) <= to->size(context)) {
            fmt::print("Integer type {} too large to convert to {}\n", *from, *to);
            if constexpr (PerformConversion) InsertImplicitCast(expr_ptr, to);
            return Score(1);
        }

        return ConversionImpossible;
    }

    /// Try deproceduring one last time.
    if (Deproceduring(expr_ptr)) return Score(1);

    return ConversionImpossible;
}

void lcc::glint::Sema::ConvertOrError(Expr** expr, Type* to) {
    if (not Convert(expr, to)) Error(
        (*expr)->location(),
        "Expression is not convertible to type {}",
        to
    );
}

bool lcc::glint::Sema::ConvertToCommonType(Expr** a, Expr** b) {
    // An integer literal should always be converted into the type of the
    // other side, favoring the left hand side when ambiguous.
    bool a_is_literal = is<IntegerLiteral>(*a);
    bool b_is_literal = is<IntegerLiteral>(*b);
    bool both_literals = a_is_literal and b_is_literal;
    if (not both_literals) {
        if (a_is_literal)
            return Convert(a, (*b)->type());
        if (b_is_literal)
            return Convert(b, (*a)->type());
    }
    return Convert(a, (*b)->type()) or Convert(b, (*a)->type());
}

auto lcc::glint::Sema::DeclTypeDecay(Type* type) -> Type* {
    return type->is_function() ? Ptr(type) : type;
}

bool lcc::glint::Sema::Deproceduring(Expr** expr_ptr) {
    /// This conversion only applies to functions and function pointers.
    auto expr = *expr_ptr;
    auto ty = expr->type();
    if (not ty->is_function()
        and (not ty->is_pointer() or not ty->elem()->is_function()))
        return false;

    /// Declarations are never deprocedured automatically.
    if (is<Decl>(expr)) return false;
    /// Block expressions are never deprocedured automatically.
    if (is<BlockExpr>(expr)) return false;

    /// Functions that take arguments are not affected.
    auto ftype = cast<FuncType>(ty->is_function() ? ty : ty->elem());
    if (not ftype->params().empty()) return false;

    /// Otherwise, insert a call.
    *expr_ptr = new (mod) CallExpr(expr, {}, expr->location());
    Analyse(expr_ptr);
    return true;
}

void lcc::glint::Sema::Discard(Expr** expr_ptr) {
    auto expr = *expr_ptr;

    /// If the expression returns void, or has an error, ignore it.
    if (not expr->ok() or expr->type()->is_void()) return;

    /// If the expression is a call to a function not marked
    /// as discardable, issue an error.
    if (auto call = cast<CallExpr>(expr)) {
        auto ftype = call->callee_type();
        if (not ftype->has_attr(FuncAttr::Discardable)) Error(
            call->location(),
            "Discarding return value of function not marked as 'discardable'"
        );
    }

    /// Otherwise, perform deproceduring. For now, we only apply
    /// deproceduring exactly once. If you need more, you can always
    /// use `()` to call the function.
    if (Deproceduring(expr_ptr)) return;

    /// Otherwise, issue a warning if this expression does not have
    /// side effects.
    if (not HasSideEffects(expr)) Diag::Warning(
        context,
        expr->location(),
        "Expression result unused"
    );
}

bool lcc::glint::Sema::EvaluateAsInt(Expr* expr, Type* int_type, aint& out) {
    EvalResult res;
    if (not expr->evaluate(context, res, true)) return false;

    /// Must be an int.
    if (not res.is_int()) {
        Error(expr->location(), "Expression is not an integer constant expression");
        return false;
    }

    /// Print a diagnostic if the thing doesn’t fit.
    bool ok = true;
    auto bits = int_type->size(context);
    aint val = res.as_int();
    auto TooLarge = [&]<typename Int>(auto cb) {
        out = std::invoke(cb, val, bits);
        if (std::invoke(cb, out, 64) != std::invoke(cb, val, 64)) {
            ok = false;
            Error(
                expr->location(),
                "Value {} of integer constant does not fit in an {}",
                Int(val),
                int_type
            );
        }
    };

    /// Check that the value fits in the integer type.
    bool is_signed = int_type->is_signed_int(context);
    LCC_ASSERT(bits <= 64, "Bit width of integer type in constant expression must be 64 or less");
    if (is_signed) utils::invoke_template<i64>(TooLarge, &aint::sext);
    else utils::invoke_template<u64>(TooLarge, &aint::zext);
    return ok;
}

bool lcc::glint::Sema::HasSideEffects(Expr* expr) {
    switch (expr->kind()) {
        /// These always have side effects.
        case Expr::Kind::While:
        case Expr::Kind::For:
        case Expr::Kind::Return:
        case Expr::Kind::TypeDecl:
        case Expr::Kind::TypeAliasDecl:
        case Expr::Kind::VarDecl:
        case Expr::Kind::FuncDecl:
        case Expr::Kind::EnumeratorDecl:
            return true;

        /// These never have side effects.
        case Expr::Kind::IntegerLiteral:
        case Expr::Kind::StringLiteral:
        case Expr::Kind::OverloadSet:
        case Expr::Kind::NameRef:
        case Expr::Kind::Module:
        case Expr::Kind::Type:
        case Expr::Kind::Sizeof:
        case Expr::Kind::Alignof:
            return false;

        /// For these, it depends.
        case Expr::Kind::Cast:
            return HasSideEffects(as<CastExpr>(expr)->operand());

        case Expr::Kind::Unary:
            return HasSideEffects(as<UnaryExpr>(expr)->operand());

        case Expr::Kind::MemberAccess:
            return HasSideEffects(as<MemberAccessExpr>(expr)->object());

        case Expr::Kind::CompoundLiteral:
            return rgs::any_of(as<CompoundLiteral>(expr)->values(), HasSideEffects);

        case Expr::Kind::Block:
            return rgs::any_of(as<BlockExpr>(expr)->children(), HasSideEffects);

        case Expr::Kind::EvaluatedConstant: {
            auto c = as<ConstantExpr>(expr);
            return c->expr() and HasSideEffects(c->expr());
        }

        case Expr::Kind::Binary: {
            auto b = as<BinaryExpr>(expr);
            if (HasSideEffects(b->lhs()) or HasSideEffects(b->rhs())) return true;
            return b->op() == TokenKind::ColonEq;
        }

        case Expr::Kind::If: {
            auto i = as<IfExpr>(expr);
            if (HasSideEffects(i->condition())) return true;
            if (HasSideEffects(i->then())) return true;
            return i->otherwise() and HasSideEffects(i->otherwise());
        }

        case Expr::Kind::Call: {
            auto c = as<CallExpr>(expr);

            if (HasSideEffects(c->callee())) return true;
            if (rgs::any_of(c->args(), HasSideEffects)) return true;

            // Function calls
            auto ty = c->callee()->type();
            while (is<PointerType, ReferenceType>(ty)) ty = ty->elem();
            if (ty->is_function()) {
                auto f = c->callee_type();
                return not f->has_attr(FuncAttr::Pure) and not f->has_attr(FuncAttr::Const);
            }

            return false;
        }

        case Expr::Kind::IntrinsicCall: {
            auto c = as<IntrinsicCallExpr>(expr);
            switch (c->intrinsic_kind()) {
                case IntrinsicKind::BuiltinDebugtrap:
                case IntrinsicKind::BuiltinMemCopy:
                case IntrinsicKind::BuiltinMemSet:
                case IntrinsicKind::BuiltinSyscall:
                    return true;

                case IntrinsicKind::BuiltinFilename:
                case IntrinsicKind::BuiltinLine:
                    return false;

                case IntrinsicKind::BuiltinInline:
                    if (c->sema_errored()) return true;
                    return HasSideEffects(c->args()[0]);
            }

            LCC_UNREACHABLE();
        }
    }

    LCC_UNREACHABLE();
}

bool lcc::glint::Sema::ImplicitDe_Reference(Expr** expr) {
    if (is<ReferenceType>((*expr)->type())) {
        /// Don’t strip reference here since we want an lvalue.
        LValueToRValue(expr, false);
        WrapWithCast(
            expr,
            as<TypeWithOneElement>((*expr)->type())->element_type(),
            CastKind::ReferenceToLValue
        );
    }

    return (*expr)->is_lvalue();
}

bool lcc::glint::Sema::ImplicitDereference(Expr** expr) {
    if (is<ReferenceType>((*expr)->type())) {
        /// Don’t strip reference here since we want an lvalue.
        LValueToRValue(expr, false);
        WrapWithCast(
            expr,
            as<TypeWithOneElement>((*expr)->type())->element_type(),
            CastKind::ReferenceToLValue
        );
    }

    while (is<PointerType>((*expr)->type())) {
        *expr = new (mod) UnaryExpr(
            TokenKind::At,
            *expr,
            false,
            (*expr)->location()
        );

        LCC_ASSERT(Analyse(expr));
    }

    return (*expr)->is_lvalue();
}

void lcc::glint::Sema::InsertImplicitCast(Expr** expr_ptr, Type* ty) {
    WrapWithCast(expr_ptr, ty, CastKind::ImplicitCast);
}

void lcc::glint::Sema::InsertPointerToIntegerCast(Expr** operand) {
    if ((*operand)->type()->is_pointer())
        InsertImplicitCast(operand, Type::Int);
}

void lcc::glint::Sema::LValueToRValue(Expr** expr, bool strip_ref) {
    if ((*expr)->sema_errored()) return;
    if ((*expr)->is_lvalue()) WrapWithCast(expr, (*expr)->type(), CastKind::LValueToRValueConv);
    if (strip_ref and is<ReferenceType>((*expr)->type())) {
        WrapWithCast(
            expr,
            as<TypeWithOneElement>((*expr)->type())->element_type(),
            CastKind::ReferenceToLValue
        );

        LValueToRValue(expr);
    }
}

auto lcc::glint::Sema::Ptr(Type* ty) -> PointerType* {
    Type* ptr = new (mod) PointerType(ty, ty->location());
    Analyse(&ptr);
    return as<PointerType>(ptr);
}

auto lcc::glint::Sema::Ref(Type* ty) -> ReferenceType* {
    Type* ref = new (mod) ReferenceType(ty, ty->location());
    Analyse(&ref);
    return as<ReferenceType>(ref);
}

int lcc::glint::Sema::TryConvert(Expr** expr, Type* type) {
    return ConvertImpl<false>(expr, type);
}

void lcc::glint::Sema::WrapWithCast(Expr** expr_ptr, Type* type, CastKind kind) {
    Expr* expr = new (mod) CastExpr(
        *expr_ptr,
        type,
        kind,
        (*expr_ptr)->location()
    );

    Analyse(&expr);
    *expr_ptr = expr;
}

/// ===========================================================================
///  Core
/// ===========================================================================
void lcc::glint::Sema::Analyse(Context* ctx, Module& m, bool use_colours) {
    if (ctx->has_error()) return;
    Sema s{ctx, m, use_colours};
    return s.AnalyseModule();
}

void lcc::glint::Sema::AnalyseModule() {
    /// Load imported modules.
    for (auto import : mod.imports()) {
        bool loaded{false};
        std::vector<std::string> paths_tried{};
        // Using module name, look in all include directories for
        // "<module name>.o". Parse the object file and get the `.intc_metadata`
        // section out of it, then deserialise that into the module.
        for (auto include_dir : context->include_directories()) {
            auto path_base0 = include_dir + std::filesystem::path::preferred_separator + import.name;
            auto path_base1 = include_dir + std::filesystem::path::preferred_separator + "lib" + import.name;
            auto paths = {
                path_base0 + ".o",
                path_base0 + ".obj",
                path_base0 + ".a",
                path_base1 + ".o",
                path_base1 + ".obj",
                path_base1 + ".a",
            };
            for (auto p : paths) {
                paths_tried.push_back(p);
                if (std::filesystem::exists(p)) {
                    fmt::print("Found IMPORT {} at {}\n", import.name, p);
                    // Open file, get contents
                    auto object_file = File::Read(p);
                    LCC_ASSERT(
                        not object_file.empty(),
                        "Found object file for module {} at {}, but the file is empty",
                        import.name,
                        p
                    );
                    // Determine file-type via magic bytes or extension
                    // TODO: Could possibly cheat and look for magic bytes/header in the file
                    // anywhere.
                    std::vector<u8> metadata_blob{};
                    if (object_file.size() >= sizeof(elf64_header)
                        and object_file.at(0) == 0x7f and object_file.at(1) == 'E'
                        and object_file.at(2) == 'L' and object_file.at(3) == 'F') {
                        auto section = elf::get_section_from_blob(
                            object_file,
                            ".intc_metadata"sv
                        );
                        metadata_blob = std::move(section.contents());
                    } else LCC_ASSERT(
                        false,
                        "Unrecognized file format of module {} at {}",
                        import.name,
                        p
                    );
                    // Very basic validation pass
                    LCC_ASSERT(
                        not metadata_blob.empty(),
                        "Didn't properly get metadata (it's empty) for module {} at {}",
                        import.name,
                        p
                    );
                    LCC_ASSERT(
                        metadata_blob.at(0) == ModuleDescription::default_version
                            and metadata_blob.at(1) == ModuleDescription::magic_byte0
                            and metadata_blob.at(2) == ModuleDescription::magic_byte1
                            and metadata_blob.at(3) == ModuleDescription::magic_byte2,
                        "Metadata for module {} at {} has invalid magic bytes",
                        import.name,
                        p
                    );
                    // Deserialise metadata blob into a module
                    // FIXME: (this module? or a new module?)
                    mod.deserialise(context, metadata_blob);
                    loaded = true;
                    break;
                }
            }
        }

        LCC_ASSERT(
            loaded,
            "Could not find imported module {} in any include directory\n"
            "Paths tried:\n"
            "{}",
            import.name,
            fmt::join(paths_tried, "\n")
        );
    }

    /// Analyse the signatures of all functions. This must be done
    /// before analysing bodies since, in order to perform overload
    /// resolution properly, we first need to apply decltype decay
    /// to all parameters (e.g. convert parameters of function type
    /// to function pointers etc.).
    for (auto& func : mod.functions()) AnalyseFunctionSignature(func);

    /// Analyse function bodies.
    for (auto& func : mod.functions()) AnalyseFunctionBody(func);
}

void lcc::glint::Sema::AnalyseFunctionBody(FuncDecl* decl) {
    tempset curr_func = decl;
    auto ty = as<FuncType>(decl->type());

    /// If the function has no body, then we’re done.
    if (not decl->body()) return;

    /// Create variable declarations for the parameters.
    for (auto& param : ty->params()) {
        if (param.name.empty()) continue;

        /// Check that we don’t already have a declaration with that
        /// name in the function scope.
        auto decls = decl->scope()->find(param.name);
        if (decls.first != decls.second) {
            Error(decls.first->second->location(), "Declaration conflicts with parameter name");
            Diag::Note(context, param.location, "Parameter declared here");
            continue;
        }

        /// Declare the parameter.
        Expr* d = new (mod) VarDecl(
            param.name,
            param.type,
            {},
            &mod,
            Linkage::LocalVar,
            param.location
        );

        LCC_ASSERT(decl->scope()->declare(context, auto(param.name), as<VarDecl>(d)).is_value());
        Analyse(&d);
        decl->param_decls().push_back(as<VarDecl>(d));
    }

    /// Analyse the body.
    Analyse(&decl->body(), ty->return_type());

    /// The last expression in a function must be a return expression or convertible
    /// to the return type of the function. If it is a return expression, then it has
    /// already been checked for that, so ignore that case.
    ///
    /// Note that the body may be a block, in which case we should check the last
    /// expression of the block rather than just the block itself.
    if (not ty->return_type()->is_void()) {
        Expr** last{};
        if (auto block = cast<BlockExpr>(decl->body())) {
            if (block->children().empty()) {
                // For anything except the top-level function, if there is an expected
                // return value, there has to be one, otherwise it's an error.
                if (decl->name() != "main") {
                    Error(decl->location(), "Function `{}` has non-void return type, and must return a value", decl->name());
                    return;
                }

                // For the top level function of executable programs, a return value is be
                // created if a valid one is not present.
                auto inserted_return_value = new (mod) IntegerLiteral(0, {});
                block->add(new (mod) ReturnExpr(inserted_return_value, {}));
            }

            last = block->last_expr();
        } else last = &decl->body();

        if (is<ReturnExpr>(*last)) return;

        if (not Convert(last, ty->return_type())) Error(
            (*last)->location(),
            "Type of last expression {} is not convertible to return type {}",
            (*last)->type(),
            ty->return_type()
        );

        LValueToRValue(last);

        // Insert a `ReturnExpr` which returns `last`.
        if (is<BlockExpr>(decl->body())) {
            *last = new (mod) ReturnExpr(*last, {});
        } else {
            decl->body() = new (mod) ReturnExpr(*last, {});
        }
    } else {
        if (auto block = cast<BlockExpr>(decl->body())) {
            if (not block->children().size() or not is<ReturnExpr>(*block->last_expr()))
                block->add(new (mod) ReturnExpr(nullptr, {}));
        } else {
            // TODO: If a function with void return type and a non-block body
            // (i.e. `foo : void() = bar 42;`) does not have a return expression, we
            // must replace the body with a block containing the non-block body
            // followed by an empty return expression.
        }

        Discard(&decl->body());
    }
}

void lcc::glint::Sema::AnalyseFunctionSignature(FuncDecl* decl) {
    /// Set a name for the decl if it’s empty.
    if (decl->name().empty()) decl->name(mod.unique_function_name());

    /// Typecheck the function type.
    Analyse(decl->type_ref());

    /// Used attribute is ignored on functions that aren’t internal. If
    /// the function is internal, then set the linkage to used so it isn’t
    /// deleted by the optimiser.
    auto ty = as<FuncType>(decl->type());
    if (ty->has_attr(FuncAttr::Used)) {
        if (decl->linkage() != Linkage::Internal)
            Diag::Warning(context, decl->location(), "'used' has no effect on this function");
        else decl->linkage(Linkage::Used);
    }
}

/// ===========================================================================
///  Analysing Expressions
/// ===========================================================================
/// Invariants:
///
///   - If an expression is marked as `Done` or `Errored`, it will
///     not be analysed again.
///
///   - If an expression is a `TypedExpr`, its type is analysed first.
///
///   - When this function returns, the expression pointed to by
///     `expr_pointer` will be marked as `Done`, unless it is already
///     marked as `Errored`. This may not end up being the same
///     expression as `expr` in the body of this function.
///
/// \param expr_ptr A pointer to the expression to analyse.
/// \param expected_type The type used for top-down inference. May be null.
/// \return (*expr_ptr)->ok().
bool lcc::glint::Sema::Analyse(Expr** expr_ptr, Type* expected_type) {
    auto expr = *expr_ptr;

    /// Don’t analyse the same expression twice.
    if (expr->sema() != SemaNode::State::NotAnalysed) return expr->ok();
    expr->set_sema_in_progress();

    /// Analyse the type if there is one.
    if (auto tc = cast<TypedExpr>(expr))
        Analyse(tc->type_ref());

    /// Analyse the expression itself.
    switch (expr->kind()) {
        /// The condition of a loop must be convertible to bool.
        case Expr::Kind::For: {
            auto f = as<ForExpr>(expr);
            AnalyseAndDiscard(&f->init());
            AnalyseAndDiscard(&f->increment());
            [[fallthrough]];
        }

        case Expr::Kind::While: {
            auto l = as<Loop>(expr);
            Analyse(&l->condition());
            if (not Convert(&l->condition(), Type::Bool)) Error(
                l->location(),
                "Invalid type for loop condition: {}",
                l->condition()->type()
            );
            LValueToRValue(&l->condition());
            AnalyseAndDiscard(&l->body());
        } break;

        /// For return expressions, make sure that the type of the
        /// argument, if any, matches that of the function containing
        /// the return expression.
        case Expr::Kind::Return: {
            /// Check the return value.
            auto r = as<ReturnExpr>(expr);
            auto ret_type = as<FuncType>(curr_func->type())->return_type();
            if (r->value()) Analyse(&r->value(), ret_type);

            /// Make sure that it matches the return type.
            if (ret_type->is_void()) {
                /// Note we allow return expressions to have an operand so long
                /// as that operand has type void; this can be the case for e.g.
                /// calls to functions returning void.
                if (r->value() and r->value()->ok() and not r->value()->type()->is_void())
                    Error(r->location(), "Function returning void must not return a value");
            } else {
                if (not r->value()) Error(
                    r->location(),
                    "Non-void function must return a value"
                );
                else if (not Convert(&r->value(), ret_type)) Error(
                    r->location(),
                    "Type of return expression is not convertible to return type {}",
                    ret_type
                );
                LValueToRValue(&r->value());
            }
        } break;

        /// The condition of an if statement must be convertible to bool, and
        /// its type is the common type of the two branches.
        case Expr::Kind::If: {
            auto i = as<IfExpr>(expr);
            Analyse(&i->condition());
            if (not Convert(&i->condition(), Type::Bool)) Error(
                i->condition()->location(),
                "Invalid type for if condition: {}",
                i->condition()->type()
            );
            LValueToRValue(&i->condition());

            /// Analyse the branches.
            Analyse(&i->then());
            if (i->otherwise()) Analyse(&i->otherwise());

            /// If the branches are both void, then the type of the if statement
            /// is void. Otherwise, it is the common type of the two branches.
            if (i->then()->ok() and (not i->otherwise() or i->otherwise()->ok())) {
                if (not i->otherwise()
                    or not ConvertToCommonType(&i->then(), &i->otherwise()))
                    i->type(Type::Void);
                else {
                    i->type(i->then()->type());

                    /// Ensure that either both branches are lvalues, or neither is.
                    if (i->then()->is_lvalue() and i->otherwise()->is_lvalue()) i->set_lvalue();
                    else {
                        LValueToRValue(&i->then());
                        LValueToRValue(&i->otherwise());
                    }
                }
            } else i->set_sema_errored();

            if (i->type()->is_void()) {
                Discard(&i->then());
                if (i->otherwise()) Discard(&i->otherwise());
            }
        } break;

        /// The type of a block is the type of its last expression. Type
        /// inference is only used for the last expression in the block.
        case Expr::Kind::Block: {
            auto block = as<BlockExpr>(expr);
            if (block->children().empty()) {
                block->type(Type::Void);
                break;
            }

            for (auto*& child : block->children()) {
                const bool last = &child == block->last_expr();
                if (not Analyse(&child, last ? expected_type : nullptr))
                    block->set_sema_errored();
                if (not last and child->ok())
                    Discard(&child);
            }

            if (not block->sema_errored()) {
                block->set_lvalue(block->children().back()->is_lvalue());
                block->type(block->children().back()->type());
            }
        } break;

        /// This mainly handles explicit casts, which allow more
        /// conversions than implicit casts.
        ///
        /// We don’t ever mark this as errored because there is no
        /// type that we *cannot* cast to, and the type this expr
        /// is supposed to have is known.
        case Expr::Kind::Cast:
            AnalyseCast(as<CastExpr>(expr));
            break;

        /// Intrinsics need to be analysed individually.
        case Expr::Kind::IntrinsicCall:
            AnalyseIntrinsicCall(expr_ptr, as<IntrinsicCallExpr>(expr));
            break;

        /// This is handled by the overload resolution code. We do *not*
        /// pass in an expected type because we do not perform overload
        /// resolution on return types.
        case Expr::Kind::Call:
            AnalyseCall(expr_ptr, as<CallExpr>(expr));
            break;

        /// Analyse local and global variable declarations.
        case Expr::Kind::VarDecl: {
            auto v = as<VarDecl>(expr);

            /// If this has an initialiser, analyse it.
            if (v->init()) {
                /// Obviously, we can only perform top-down type inference
                /// if we’re not already performing bottom-up inference. If
                /// the type is known, make sure that we use a type that is
                /// legal in a declaration for inference.
                const bool infer_type = v->type()->is_unknown();
                Analyse(
                    &v->init(),
                    infer_type ? nullptr : DeclTypeDecay(v->type())
                );

                /// If we’re using type inference, break if there was an
                /// error since we can’t validate the type of this if we
                /// don’t know it. Otherwise, set the type of this to the
                /// type of the initialiser.
                if (infer_type) {
                    if (v->init()->ok()) v->type(v->init()->type());
                    else {
                        v->set_sema_errored();
                        break;
                    }
                }
            }

            /// Check that the type makes sense. In particular, if it is
            /// a function type, convert it to a function pointer type.
            v->type(DeclTypeDecay(v->type()));

            /// Make sure the initialiser is convertible to that type. Note
            /// that, if this fails, we do not mark this node as errored as
            /// its type is well-formed; it’s just the initialiser that has
            /// a problem.
            if (v->init()) {
                if (not Convert(&v->init(), v->type())) Error(
                    v->init()->location(),
                    "Type of initialiser, {}, is not convertible to variable type {}",
                    v->init()->type(),
                    v->type()
                );

                LValueToRValue(&v->init());
            }

            v->set_lvalue();
        } break;

        /// These are handled by the code that also handles enums.
        case Expr::Kind::EnumeratorDecl: LCC_UNREACHABLE();

        case Expr::Kind::CompoundLiteral: {
            auto c = as<CompoundLiteral>(expr);

            /// Analyse all subexpressions.
            for (auto*& child : c->values()) {
                if (Analyse(&child)) LValueToRValue(&child);
                else c->set_sema_errored();
            }

            if (not c->type() and not expected_type) {
                Error(
                    c->location(),
                    "Cannot infer type of Untyped Compound Literal"
                );
            }
            // TODO: If both c->type() and expected_type, Convert to expected_type.
            LCC_ASSERT(Analyse(c->type_ref()));
        } break;

        /// LHS must be a (pointer to a) struct, and the identifier must
        /// exist in the struct.
        case Expr::Kind::MemberAccess: {
            auto m = as<MemberAccessExpr>(expr);
            /// If there is an error analysing the object, we don’t know
            /// its type and can thus not continue checking this.
            if (not Analyse(&m->object())) {
                m->set_sema_errored();
                break;
            }

            /// Accessing ‘members’ of modules.
            if (is<NameRefExpr>(m->object())
                and is<ModuleExpr>(as<NameRefExpr>(m->object())->target())) {
                // m->name() == name of member we are accessing
                // m->object() == NameRef to module we are accessing
                auto name_ref = as<NameRefExpr>(m->object());
                auto module_expr = as<ModuleExpr>(name_ref->target());
                auto* referenced_module = module_expr->mod();
                auto* scope = referenced_module->global_scope();
                // Replace member access with a name ref
                *expr_ptr = new (mod) NameRefExpr(m->name(), scope, m->location());
                AnalyseNameRef(as<NameRefExpr>(*expr_ptr));
                break;
            }

            /// ‘object’ is actually a type name.
            if (is<NameRefExpr>(m->object())
                and is<TypeDecl>(as<NameRefExpr>(m->object())->target())) {
                auto t = as<TypeDecl>(as<NameRefExpr>(m->object())->target());
                if (is<StructType>(t->type())) LCC_TODO();

                /// Handle accessing enumerators.
                if (auto e = cast<EnumType>(t->type())) {
                    auto it = rgs::find_if(
                        e->enumerators(),
                        [&](auto&& en) { return en->name() == m->name(); }
                    );
                    if (it == e->enumerators().end()) {
                        Error(m->location(), "Type {} has no enumerator named '{}'", e, m->name());
                        m->set_sema_errored();
                        break;
                    }

                    auto enumerator = *it;
                    if (enumerator->sema_errored()) {
                        m->set_sema_errored();
                        break;
                    }

                    if (not enumerator->ok()) {
                        Error(
                            m->location(),
                            "Enumerator {} cannot be used before it is defined",
                            enumerator->name()
                        );
                        m->set_sema_errored();
                        break;
                    }

                    m->type(enumerator->type());
                    m->set_sema_done();
                    *expr_ptr = new (mod) ConstantExpr(expr, enumerator->value());
                    break;
                }

                LCC_UNREACHABLE();
            }

            /// Type must be a struct type (or something that represents one, like a DynamicArrayType)
            auto stripped_object_type = m->object()->type()->strip_pointers_and_references();
            auto struct_type = cast<StructType>(stripped_object_type);
            if (not struct_type and is<DynamicArrayType>(stripped_object_type))
                struct_type = as<DynamicArrayType>(stripped_object_type)->struct_type(mod);

            if (not struct_type) {
                Error(
                    m->object()->location(),
                    "LHS of member access must be a (pointer or reference to) struct, but was {}",
                    m->object()->type()
                );

                m->set_sema_errored();
                break;
            }

            /// The struct type must contain the member.
            auto& members = struct_type->members();
            auto it = rgs::find_if(members, [&](auto& member) { return member.name == m->name(); });
            if (it == members.end()) {
                Error(m->location(), "Struct {} has no member named '{}'", struct_type, m->name());
                m->set_sema_errored();
                break;
            }

            /// Set the struct and member index.
            m->finalise(struct_type, usz(std::distance(members.begin(), it)));

            /// Dereference pointers until we have an lvalue to struct. The
            /// member access is an lvalue, iff the struct is an lvalue.
            m->set_lvalue(ImplicitDereference(&m->object()));
            m->type(it->type);
        } break;

        case Expr::Kind::Sizeof: {
            auto sizeof_expr = as<SizeofExpr>(expr);
            Analyse(sizeof_expr->expr_ref());

            aint value{};
            if (auto typed_expr = cast<TypedExpr>(sizeof_expr->expr()))
                value = typed_expr->type()->size(context);
            else Error(sizeof_expr->location(), "Unhandled expression in sizeof");

            *expr_ptr = new (mod) IntegerLiteral(value, expr->location());
        } break;

        case Expr::Kind::Alignof: {
            auto alignof_expr = as<AlignofExpr>(expr);
            Analyse(alignof_expr->expr_ref());

            aint value{};
            if (auto typed_expr = cast<TypedExpr>(alignof_expr->expr()))
                value = typed_expr->type()->align(context);
            else Error(alignof_expr->location(), "Unhandled expression in alignof");

            *expr_ptr = new (mod) IntegerLiteral(value, expr->location());
        } break;

        /// Unary prefix and postfix expressions.
        case Expr::Kind::Unary:
            AnalyseUnary(as<UnaryExpr>(expr));
            break;

        /// Binary expressions.
        case Expr::Kind::Binary:
            AnalyseBinary(as<BinaryExpr>(expr));
            break;

        /// Reference to a declared entity.
        case Expr::Kind::NameRef:
            AnalyseNameRef(as<NameRefExpr>(expr));
            break;

        /// Functions are analysed separately.
        case Expr::Kind::FuncDecl:
            LCC_ASSERT(expr->type()->is_function());
            break;

        /// The actual work here is analysing the type, so this is a no-op.
        case Expr::Kind::Type:
        case Expr::Kind::TypeDecl:
        case Expr::Kind::TypeAliasDecl:
            break;

        /// There isn’t really a way these could be malformed.
        case Expr::Kind::IntegerLiteral:
        case Expr::Kind::StringLiteral:
            break;

        /// These should only be created by sema and are thus no-ops.
        case Expr::Kind::Module:
        case Expr::Kind::EvaluatedConstant:
            break;

        /// Validate overload sets.
        case Expr::Kind::OverloadSet: {
            auto& os = as<OverloadSet>(expr)->overloads();

            /// An overload set must not contain two overloads with the
            /// same parameter types. All function signatures have already
            /// been analysed, so we just need to compare them.
            for (usz i = 0; i < os.size(); i++) {
                auto oi = os[i];
                auto oi_params = oi->param_types();
                for (usz j = i + 1; j < os.size(); j++) {
                    auto oj = os[j];
                    auto oj_params = oj->param_types();

                    /// Different number of parameters means these two can’t be the same.
                    if (oi_params.size() != oj_params.size()) continue;

                    /// Compare the parameters.
                    usz k = 0;
                    for (; k < oi_params.size(); k++)
                        if (not Type::Equal(oi_params[isz(k)], oj_params[isz(k)]))
                            break;

                    /// If all of them are equal, then we have a problem.
                    if (k != oi_params.size()) {
                        Error(
                            oi->location(),
                            "Overload set contains two overloads with the same parameter types"
                        );
                        Diag::Note(context, oj->location(), "Conflicting overload is here");
                        expr->set_sema_errored();
                    }
                }
            }
        } break;
    }

    /// Do *not* use `expr` here, as it may have been replaced by something else.
    if (not(*expr_ptr)->sema_done_or_errored()) (*expr_ptr)->set_sema_done();
    return (*expr_ptr)->ok();
}

void lcc::glint::Sema::AnalyseBinary(BinaryExpr* b) {
    /// Give up if there is an error in either operand.
    if (not Analyse(&b->lhs()) or not Analyse(&b->rhs())) {
        b->set_sema_errored();
        return;
    }

    switch (b->op()) {
        default: Diag::ICE("Invalid binary operator '{}'", ToString(b->op()));

        case TokenKind::And:
        case TokenKind::Or: {
            LValueToRValue(&b->lhs());
            LValueToRValue(&b->rhs());
            auto lhs = b->lhs()->type();
            auto rhs = b->rhs()->type();

            /// Both types must be integers or booleans.
            if (not lhs->is_integer(true) or not rhs->is_integer(true)) {
                Error(b->location(), "Cannot perform arithmetic on {} and {}", lhs, rhs);
                b->set_sema_errored();
                return;
            }

            /// Convert both operands to booleans.
            if (not Convert(&b->lhs(), Type::Bool)) {
                Error(
                    b->location(),
                    "Binary logical operator {} on {} and {}: cannot convert lhs, of type {}, to {}",
                    ToString(b->op()),
                    lhs,
                    rhs,
                    lhs,
                    Type::Bool
                );
                b->set_sema_errored();
                return;
            }
            if (not Convert(&b->rhs(), Type::Bool)) {
                Error(
                    b->location(),
                    "Binary logical operator {} on {} and {}: cannot convert rhs, of type {}, to {}",
                    ToString(b->op()),
                    lhs,
                    rhs,
                    lhs,
                    Type::Bool
                );
                b->set_sema_errored();
                return;
            }

            /// The result type is bool.
            b->type(Type::Bool);
        } break;

        /// Pointer or array subscript.
        case TokenKind::LBrack: {
            ImplicitDe_Reference(&b->lhs());
            auto ty = b->lhs()->type();
            if (not is<PointerType, ArrayType>(ty)) {
                Error(
                    b->location(),
                    "LHS of subscript must be a pointer or array, but was {}",
                    b->lhs()->type()
                );
                b->set_sema_errored();
                return;
            }

            /// Result type is the pointer type or a pointer to the array element.
            b->type(is<PointerType>(ty) ? ty : Ptr(as<ArrayType>(ty)->element_type()));

            /// The RHS must be an integer.
            LValueToRValue(&b->rhs());
            if (not Convert(&b->rhs(), Type::Int)) {
                Error(b->rhs()->location(), "RHS of subscript must be an integer");
                return;
            }

            /// If it is an integer, try to evaluate it for bounds checking.
            if (auto arr = cast<ArrayType>(ty); arr and arr->size() and arr->size()->ok() and arr->size()->kind() == Expr::Kind::EvaluatedConstant) {
                EvalResult res;
                if (b->rhs()->evaluate(context, res, false)) {
                    if (res.as_int().is_negative()
                        or res.as_int() >= as<ConstantExpr>(arr->size())->value().as_int().value())
                        Error(b->location(), "Array subscript out of bounds");

                    /// Since we already have the result, store it for later.
                    b->rhs() = new (mod) ConstantExpr(b->rhs(), res);
                }
            }
        } break;

        /// Pointer arithmetic is handled by the subscript operator,
        /// so these are all just regular arithmetic.
        case TokenKind::Star:
        case TokenKind::Slash:
        case TokenKind::Percent:
        case TokenKind::Plus:
        case TokenKind::Minus:
        case TokenKind::Shl:
        case TokenKind::Shr:
        case TokenKind::Ampersand:
        case TokenKind::Pipe:
        case TokenKind::Caret: {
            LValueToRValue(&b->lhs());
            LValueToRValue(&b->rhs());
            auto lhs = b->lhs()->type();
            auto rhs = b->rhs()->type();

            /// Both types must be integers.
            if (not lhs->is_integer() or not rhs->is_integer()) {
                Error(b->location(), "Cannot perform arithmetic on {} and {}", lhs, rhs);
                b->set_sema_errored();
                return;
            }

            /// Convert both operands to their common type.
            if (not ConvertToCommonType(&b->lhs(), &b->rhs())) {
                Error(b->location(), "Cannot perform arithmetic on {} and {}", lhs, rhs);
                b->set_sema_errored();
                return;
            }

            /// The result type is the common type.
            b->type(b->lhs()->type());
        } break;

        /// Comparisons are all handled the same.
        case TokenKind::Eq:
        case TokenKind::Ne:
        case TokenKind::Lt:
        case TokenKind::Gt:
        case TokenKind::Le:
        case TokenKind::Ge: {
            LValueToRValue(&b->lhs());
            LValueToRValue(&b->rhs());
            auto lhs = b->lhs()->type();
            auto rhs = b->rhs()->type();

            /// If both operands are integers, convert them to their common type.
            if (lhs->is_integer() and rhs->is_integer()) {
                if (not ConvertToCommonType(&b->lhs(), &b->rhs())) {
                    Error(b->location(), "Cannot compare {} and {}", lhs, rhs);
                    b->set_sema_errored();
                    return;
                }
            }

            /// Bool can only be compared with bool.
            else if (lhs->is_bool() and rhs->is_bool()) { /** No-op **/
            }

            /// If both operands are pointers, they must be the same type.
            else if (lhs->is_pointer() and rhs->is_pointer()) {
                if (not Type::Equal(lhs, rhs)) Error(
                    b->location(),
                    "Cannot compare unrelated pointer types {} and {}",
                    lhs,
                    rhs
                );
            }

            /// Other comparisons are not allowed.
            else { Error(b->location(), "Cannot compare {} and {}", lhs, rhs); }

            /// Comparisons return bool.
            b->type(Type::Bool);
        } break;

        /// Assignment.
        case TokenKind::ColonEq: {
            LValueToRValue(&b->rhs());
            ImplicitDe_Reference(&b->lhs());
            if (not b->lhs()->is_lvalue()) {
                Error(b->location(), "LHS of assignment must be an lvalue");
                b->set_sema_errored();
                return;
            }

            /// The type of the assignment is the same lvalue. Note that if
            /// the lhs is indeed an lvalue, we don’t ever mark this as errored
            /// because we know what its type is going to be, irrespective of
            /// whether the assignment is valid or not.
            b->type(b->lhs()->type());

            /// Assignment yields an lvalue.
            b->set_lvalue();

            /// The RHS must be assignable to the LHS.
            if (not Convert(&b->rhs(), b->lhs()->type())) {
                Error(
                    b->rhs()->location(),
                    "Type of expression {} is not convertible to variable type {}",
                    b->rhs()->type(),
                    b->lhs()->type()
                );
                return;
            }

        } break;
    }
}

void lcc::glint::Sema::AnalyseCall(Expr** expr_ptr, CallExpr* expr) {
    /// If the callee is a name ref, check for builtins first.
    if (auto name = cast<NameRefExpr>(expr->callee())) {
        static const StringMap<IntrinsicKind> builtin_names{
            {"__builtin_debugtrap", IntrinsicKind::BuiltinDebugtrap},
            {"__builtin_filename", IntrinsicKind::BuiltinFilename},
            {"__builtin_inline", IntrinsicKind::BuiltinInline},
            {"__builtin_line", IntrinsicKind::BuiltinLine},
            {"__builtin_memcpy", IntrinsicKind::BuiltinMemCopy},
            {"__builtin_memset", IntrinsicKind::BuiltinMemSet},
            {"__builtin_syscall", IntrinsicKind::BuiltinSyscall},
        };

        /// Check if this is the name of a builtin.
        auto n = name->name();
        if (auto kind = builtin_names.find(n); kind != builtin_names.end()) {
            /// We copy the arguments and leave the original expression unchanged
            /// since this node may be references in multiple places, all of which
            /// may need to be patched, and there is no good way of doing that
            /// without copying each use individually.
            auto intrinsic = new (mod) IntrinsicCallExpr(
                kind->second,
                expr->args()
            );

            /// Make sure to actually analyse this intrinsic, as it will otherwise
            /// just be marked as done without actually being analysed.
            *expr_ptr = intrinsic;
            Analyse(expr_ptr);
            return;
        }
    }

    /// Analyse the callee and the arguments.
    for (auto*& arg : expr->args()) Analyse(&arg);

    /// If analysing the callee fails, we can’t do anything else.
    if (not Analyse(&expr->callee())) {
        expr->set_sema_errored();
        return;
    }

    /// If the callee is an overload set, perform overload resolution.
    if (is<OverloadSet>(expr->callee())) {
        /// If any of the arguments errored, we can’t resolve this.
        if (rgs::any_of(expr->args(), &Expr::sema_errored)) {
            expr->set_sema_errored();
            return;
        }

        /// TODO: Overload resolution.
        // See `docs/function_overload_resolution.org`
        Diag::ICE("Sorry, overload resolution is currently not implemented. Annoy the developer if you don't want it.");
    }

    // If the callee is a type expression, this is a type instantiation.
    // TODO: This NameRefExpr check is probably a sign of something more
    // sinister going on, but I can't exactly pinpoint it right now.
    if (is<TypeExpr>(expr->callee()) or (is<NameRefExpr>(expr->callee()) and is<TypeDecl>(as<NameRefExpr>(expr->callee())->target()))) {
        const usz s = expr->args().size();
        for (usz i = 0; i < s; ++i)
            LValueToRValue(expr->args().data() + i);

        if (expr->args().size() == 1) {
            *expr_ptr = new (mod) CastExpr(
                expr->args().at(0),
                expr->callee()->type(),
                CastKind::HardCast,
                expr->location()
            );
        } else {
            *expr_ptr = new (mod) CompoundLiteral(
                expr->args(),
                expr->location(),
                expr->callee()->type()
            );
        }
        return;
    }

    /// If the callee is a function pointer, dereference it.
    if (auto ty = expr->callee()->type(); ty->is_pointer() and ty->elem()->is_function())
        InsertImplicitCast(&expr->callee(), ty->elem());

    // if the callee is an integer, multiply all the arguments.
    // `100 x;` -> 100 * x
    //   CallExpr(
    //       ConstantExpr 100
    //       NameRefExpr x
    //   )
    // becomes
    //   BinaryExpr(
    //       '*'
    //       ConstantExpr 100
    //       NameRefExpr x
    // )
    //
    // `100 x y` -> 100 * x * y
    //   CallExpr(
    //       ConstantExpr 100
    //       NameRefExpr x
    //       NameRefExpr y
    //   )
    // becomes
    //   BinaryExpr(
    //       '*'
    //       ConstantExpr 100
    //       BinaryExpr(
    //           NameRefExpr x
    //           NameRefExpr y
    //       )
    // )

    else if (auto ty = expr->callee()->type(); ty->is_integer()) {
        // NOTE: Call of integer with zero arguments by deproceduring should not
        // be possible, but this handles `100();`
        if (expr->args().empty() and not HasSideEffects(expr)) {
            Diag::Warning(
                context,
                expr->location(),
                "Expression result unused"
            );
            return;
        }

        auto* rhs = expr->args().back();
        // NOTE: Relies on unsigned underflow
        for (usz i = expr->args().size() - 2; i < expr->args().size(); --i) {
            auto* lhs = expr->args().at(i);
            rhs = new (mod) BinaryExpr(
                TokenKind::Star,
                lhs,
                rhs,
                {lhs->location(), rhs->location()}
            );
        }

        *expr_ptr = new (mod) BinaryExpr(
            TokenKind::Star,
            expr->callee(),
            rhs,
            expr->location()
        );

        Analyse(expr_ptr);

        return;
    }

    /// Otherwise, if the type is not already a function type, we can’t call this.
    else if (not ty->is_function()) {
        Error(expr->callee()->location(), "Cannot call non-function(-pointer) type {}", ty);
        expr->set_sema_errored();
        return;
    }

    /// The type of the call is the return type of the function.
    auto func_type = cast<FuncType>(expr->callee()->type());
    expr->type(func_type->return_type());

    /// Check that there are as many arguments as parameters.
    if (expr->args().size() != func_type->params().size()) {
        Error(
            expr->location(),
            "Incorrect number of arguments for function. Expected {} instead of {}",
            func_type->params().size(),
            expr->args().size()
        );
    }

    /// Check that the arguments are convertible to the parameter types. This
    /// is one of the few places where we allow reference binding, so perform
    /// lvalue-to-rvalue conversion only if the parameter type is not a reference
    /// type. This is all handled transparently by Convert().
    for (usz i = 0, end = std::min(expr->args().size(), func_type->params().size()); i < end; i++) {
        LValueToRValue(expr->args().data() + i);
        if (not Convert(expr->args().data() + i, func_type->params()[i].type)) Error(
            expr->args()[i]->location(),
            "Type of argument {} is not convertible to parameter type {}",
            expr->args()[i]->type(),
            func_type->params()[i].type
        );
    }
}

void lcc::glint::Sema::AnalyseCast(CastExpr* c) {
    /// Implicit casts and lvalue-to-rvalue conversions are
    /// only ever created by sema, so we know they’re fine.
    if (c->is_implicit_cast() or c->is_lvalue_to_rvalue()
        or c->is_lvalue_to_ref() or c->is_ref_to_lvalue()) {
        c->set_lvalue(c->is_ref_to_lvalue());
        return;
    }

    /// If analysis of the operand failed, we don’t know its
    /// type and thus have no way of checking whether the cast
    /// makes sense.
    if (not Analyse(&c->operand(), c->type())) return;

    /// If the types are implicitly convertible, then the cast
    /// is fine. If this fails, it will still perform lvalue to
    /// rvalue conversion on the operand, which is exactly what
    /// we want.
    if (Convert(&c->operand(), c->type())) return;

    /// All conversions that rely on references have already been
    /// taken care of by Convert(), so we don’t care about references
    /// anymore at this point.
    ///
    /// Thus, the type we’re casting to must not be a reference type.
    auto from = c->operand()->type();
    auto to = c->type();
    if (to->is_reference()) {
        Error(c->location(), "Invalid cast of rvalue to reference type");
        return;
    }

    /// Explicitly casting from enums/integers to integers and
    /// enums/integers to booleans and booleans to integers is allowed.
    if ((from->is_integer(true) or from->is_enum()) and to->is_integer(true)) return;

    /// Casting from pointers to integers and pointers to booleans is allowed.
    if (from->is_pointer() and to->is_integer(true)) return;

    /// Helper to allow only hard casts.
    auto HardCast = [&] {
        if (not c->is_hard_cast()) Error(
            c->location(),
            "Cast from {} to {} is unsafe. If this is intended, use 'as!' instead",
            from,
            to
        );
    };

    /// Hard casts from integers to enums are allowed.
    if (from->is_integer(true) and to->is_enum()) return HardCast();

    /// Hard casts between pointers and from pointers to integers are
    /// allowed. Note that, if the pointers are compatible, the call to
    /// Convert() above will have already taken care of this case, so we
    /// don’t need to check for that here.
    if (to->is_pointer() and (from->is_integer() or from->is_pointer())) return HardCast();

    /// Hard casts between types that have the same size are allowed.
    if (from->size(context) == to->size(context) and c->is_hard_cast()) return;

    /// Any other casts are currently not allowed.
    Error(c->location(), "Invalid cast from {} to {}", from, to);
}

void lcc::glint::Sema::AnalyseIntrinsicCall(Expr** expr_ptr, IntrinsicCallExpr* expr) {
    switch (expr->intrinsic_kind()) {
        case IntrinsicKind::BuiltinDebugtrap: {
            if (not expr->args().empty())
                Error(expr->location(), "__builtin_debugtrap() takes no arguments");
            expr->type(Type::Void);
        } break;

        case IntrinsicKind::BuiltinFilename: {
            if (not expr->args().empty())
                Error(expr->location(), "__builtin_filename() takes no arguments");

            /// Get the name of the file containing this call.
            std::string filename = "<unknown>";
            if (expr->location().seekable(context))
                filename = context->files()[expr->location().file_id]->path().filename().string();

            /// Create a string literal containing the filename.
            auto* str = new (mod) StringLiteral(mod, filename, expr->location());
            expr->type(str->type());
            expr->set_sema_done();
            *expr_ptr = new (mod) ConstantExpr(expr, str);
        } break;

        case IntrinsicKind::BuiltinInline: {
            /// This takes one argument, and it must be a call expression.
            if (expr->args().size() != 1)
                Error(expr->location(), "__builtin_inline() takes exactly one argument");

            /// Analyse the call.
            auto*& call = expr->args().front();
            if (not Analyse(&call)) expr->set_sema_errored();
            if (not is<CallExpr>(call)) Error(
                call->location(),
                "Argument to __builtin_inline() must be a (non-builtin) function call"
            );

            /// Return type is the type of the callee.
            if (call->ok()) expr->type(call->type());
        } break;

        case IntrinsicKind::BuiltinLine: {
            if (not expr->args().empty()) Error(
                expr->location(),
                "__builtin_line() takes no arguments"
            );
            expr->type(Type::Int);
            expr->set_sema_done();

            /// If possible, seek to the location, if not we just insert 0.
            i64 line = 0;
            if (expr->location().seekable(context)) line = i64(expr->location().seek_line_column(context).line);
            *expr_ptr = new (mod) ConstantExpr(expr, line);
        } break;

        case IntrinsicKind::BuiltinMemCopy: {
            /// This takes two pointer and a size argument.
            if (expr->args().size() != 3)
                Error(expr->location(), "__builtin_memcpy() takes exactly three arguments");

            /// Analyse the arguments.
            for (auto*& arg : expr->args()) Analyse(&arg);
            ConvertOrError(&expr->args()[0], Type::VoidPtr);
            ConvertOrError(&expr->args()[1], Type::VoidPtr);
            ConvertOrError(&expr->args()[2], Type::Int);
            LValueToRValue(&expr->args()[0]);
            LValueToRValue(&expr->args()[1]);
            LValueToRValue(&expr->args()[2]);

            /// Unlike C’s memcpy()/memmove(), this returns nothing.
            expr->type(Type::Void);
        } break;

        case IntrinsicKind::BuiltinMemSet: {
            /// This takes two pointer and a size argument.
            if (expr->args().size() != 3)
                Error(expr->location(), "__builtin_memset() takes exactly three arguments");

            /// Analyse the arguments.
            for (auto*& arg : expr->args()) Analyse(&arg);
            ConvertOrError(&expr->args()[0], Type::VoidPtr);
            ConvertOrError(&expr->args()[1], Type::Byte);
            ConvertOrError(&expr->args()[2], Type::Int);
            LValueToRValue(&expr->args()[0]);
            LValueToRValue(&expr->args()[1]);
            LValueToRValue(&expr->args()[2]);

            /// Unlike C’s memset(), this returns nothing.
            expr->type(Type::Void);
        } break;

        case IntrinsicKind::BuiltinSyscall: {
            /// This has 1-7 integer-sized arguments and returns an integer.
            if (expr->args().empty() or expr->args().size() > 7)
                Error(expr->location(), "__builtin_syscall() takes between 1 and 7 arguments");

            /// Arguments must be integers or pointers.
            for (auto*& arg : expr->args()) {
                Analyse(&arg);
                InsertPointerToIntegerCast(&arg);
                ConvertOrError(&arg, Type::Int);
                LValueToRValue(&arg);
            }

            /// Syscalls all return integer.
            expr->type(Type::Int);
        } break;
    }
}

void lcc::glint::Sema::AnalyseNameRef(NameRefExpr* expr) {
    /// Look up the thing in its scope, if there is no definition of the
    /// symbol in its scope, search its parent scopes until we find one.
    auto scope = expr->scope();
    decltype(scope->find(expr->name())) syms;
    while (scope) {
        syms = scope->find(expr->name());
        scope = scope->parent();
        if (syms.first != syms.second) break;
    }

    /// If we’re at the global scope and there still is no symbol, then
    /// this symbol is apparently not declared.
    if (syms.first == syms.second) {
        /// Search imported modules here.
        for (auto ref : mod.imports()) {
            if (expr->name() == ref.name) {
                // Set expr->target() and expr->type() to something reasonable.
                auto module_expr = new (mod) ModuleExpr(&mod, expr->location());
                expr->target(module_expr);
                expr->type(Type::Void);
                return;
            }
        }

        Error(expr->location(), "Unknown symbol '{}'", expr->name());

        /// If there is a declaration of this variable in the top-level
        /// scope, tell the user that they may have forgotten to make it
        /// static.
        auto top_level = mod.top_level_scope()->find(expr->name());
        if (top_level.first != top_level.second) {
            Diag::Note(
                context,
                top_level.first->second->location(),
                "A declaration exists at the top-level. Did you mean to make it 'static'?"
            );
        }

        expr->set_sema_errored();
        return;
    }

    /// Either there is exactly one node that is not a function, or, there
    /// may be one or more nodes with that name that are functions. In the
    /// former case, resolve the reference to that node.
    if (not is<FuncDecl>(syms.first->second)) {
        Expr* e = syms.first->second;
        Analyse(&e);
        LCC_ASSERT(syms.first->second == e);
        syms.first->second = as<Decl>(e);

        /// If sema is still in progress for the declaration, then we’re in
        /// its initialiser, which is illegal.
        if (e->sema() == SemaNode::State::InProgress) {
            Error(
                expr->location(),
                "Cannot use variable '{}' in its own initialiser",
                expr->name()
            );
            expr->set_sema_errored();
            return;
        }

        expr->target(syms.first->second);
        expr->type(syms.first->second->type());
        if (syms.first->second->is_lvalue()) expr->set_lvalue();
        return;
    }

    /// Helper to append iterator ranges to an overload set.
    std::vector<FuncDecl*> overloads;
    auto Append = [&overloads](auto&& range) {
        for (auto it = range.first; it != range.second; it++)
            overloads.push_back(as<FuncDecl>(it->second));
    };

    /// In the other case, collect all functions with that name as well as
    /// all functions with that name in parent scopes and create an overload
    /// set for them.
    Append(syms);
    for (; scope; scope = scope->parent()) Append(scope->find(expr->name()));

    /// If there is only one function, resolve it directly to that function.
    if (overloads.size() == 1) {
        expr->target(overloads[0]);
        expr->type(overloads[0]->type());
        return;
    }

    /// Create a new overload set and analyse it. This will make sure there are
    /// no redeclarations etc.
    Expr* os = new (mod) OverloadSet(overloads, expr->location());
    Analyse(&os);
    if (os->sema_errored()) expr->set_sema_errored();

    /// The type of an overload set is special because its actual type will depend
    /// on the context. Roughly, the `OverloadSet` type is convertible to (a pointer
    /// to) any of the function types in the set.
    expr->target(os);
    expr->type(Type::OverloadSet);
}

void lcc::glint::Sema::AnalyseUnary(UnaryExpr* u) {
    /// Give up if there is an error in the operand.
    if (not Analyse(&u->operand())) {
        u->set_sema_errored();
        return;
    }

    /// Postfix operators.
    if (u->is_postfix()) {
        /// We currently don’t have postfix operators.
        LCC_UNREACHABLE();
    }

    /// Prefix operators.
    switch (u->op()) {
        default: Diag::ICE("Invalid prefix operator '{}'", ToString(u->op()));

        /// Get the address of an lvalue or function.
        case TokenKind::Ampersand: {
            if (not u->operand()->is_lvalue()) {
                Error(u->location(), "Cannot take address of rvalue");
                u->set_sema_errored();
                break;
            }

            u->type(Ptr(u->operand()->type()));
        } break;

        /// Convert a pointer to an lvalue.
        case TokenKind::At: {
            /// The pointer itself must be an rvalue.
            LValueToRValue(&u->operand());
            auto ty = u->operand()->type();
            if (not is<PointerType>(ty)) {
                Error(u->location(), "Cannot dereference non-pointer type {}", ty);
                u->set_sema_errored();
                break;
            }

            u->type(as<PointerType>(ty)->element_type());
            u->set_lvalue();
        } break;

        /// Negate an integer.
        case TokenKind::Minus: {
            LValueToRValue(&u->operand());
            auto ty = u->operand()->type();
            if (not ty->is_integer()) {
                Error(
                    u->location(),
                    "Operand of operator '-' must be an integer type, but was {}",
                    ty
                );
                u->set_sema_errored();
                break;
            }

            u->type(ty);
        } break;

        /// Bitwise-not an integer.
        case TokenKind::Tilde: {
            LValueToRValue(&u->operand());
            auto ty = u->operand()->type();
            if (not ty->is_integer()) {
                Error(
                    u->location(),
                    "Operand of operator '~' must be an integer type, but was {}",
                    ty
                );
                u->set_sema_errored();
                break;
            }

            u->type(ty);
        } break;

        /// Negate a bool, integer, or pointer.
        case TokenKind::Exclam: {
            LValueToRValue(&u->operand());
            auto ty = u->operand()->type();
            if (not is<PointerType>(ty) and not ty->is_integer(true)) {
                Error(
                    u->location(),
                    "Operand of operator '!' must be a bool, integer, or pointer type, but was {}",
                    ty
                );

                /// No need to mark this as errored because the
                /// result type is always bool.
                break;
            }

            /// The result of '!' is always a bool.
            u->type(Type::Bool);
        } break;
    }
}

/// ===========================================================================
///  Analysing Types
/// ===========================================================================
bool lcc::glint::Sema::Analyse(Type** type_ptr) {
    auto type = *type_ptr;

    // Don’t analyse the same type twice.
    if (type->sema() != SemaNode::State::NotAnalysed) return type->ok();
    type->set_sema_in_progress();

    switch (type->kind()) {
        // These are marked as done in the constructor.
        case Type::Kind::Builtin: LCC_UNREACHABLE();

        // These are no-ops.
        case Type::Kind::FFIType: break;

        // Named types need to be resolved to a type.
        case Type::Kind::Named: {
            auto n = as<NamedType>(type);
            LCC_ASSERT(n->name().size(), "NamedType has empty name");
            LCC_ASSERT(n->scope(), "NamedType {} has NULL scope", n->name());

            /// This code is similar to name resolution for expressions,
            /// except that we don’t need to worry about overloads.
            Type* ty{};
            for (auto scope = n->scope(); scope; scope = scope->parent()) {
                auto syms = scope->find(n->name());
                if (syms.first == syms.second) continue;
                if (auto s = cast<TypeDecl>(syms.first->second)) {
                    Expr* e = s;
                    Analyse(&e);
                    ty = s->type();
                    break;
                }

                if (auto a = cast<TypeAliasDecl>(syms.first->second)) {
                    Expr* e = a;
                    Analyse(&e);
                    ty = a->type();
                    break;
                }

                Error(n->location(), "'{}' is not a type", n->name());
                Diag::Note(
                    context,
                    syms.first->second->location(),
                    "Because of declaration here",
                    n->name()
                );

                n->set_sema_errored();
                break;
            }

            if (not ty) {
                Error(n->location(), "'{}' does not name a type", n->name());
                n->set_sema_errored();
            } else {
                *type_ptr = ty;
            }
        } break;

        /// Pointers to any non-reference types are fine.
        case Type::Kind::Pointer: {
            auto p = as<PointerType>(type);
            LCC_ASSERT(p->element_type(), "PointerType has NULL element type");
            Analyse(&p->element_type());

            auto elem = p->element_type();
            if (is<ReferenceType>(elem)) {
                if (elem->ok()) Error(
                    p->location(),
                    "Cannot create pointer to reference type {}",
                    elem
                );
                p->set_sema_errored();
            }
        } break;

        /// References to references are collapsed to a single reference.
        case Type::Kind::Reference: {
            auto r = as<ReferenceType>(type);
            LCC_ASSERT(r->element_type(), "ReferenceType has NULL element type");
            Analyse(&r->element_type());

            /// Collapse refs.
            while (is<ReferenceType>(r->element_type()))
                r->element_type(r->element_type()->elem());
        } break;

        /// Apply decltype decay to the element type and prohibit
        /// arrays of references. Also check the size.
        case Type::Kind::Array: {
            auto a = as<ArrayType>(type);
            LCC_ASSERT(a->element_type(), "Array has NULL element type");
            Analyse(&a->element_type());
            a->element_type(DeclTypeDecay(a->element_type()));

            auto elem = a->element_type();
            if (is<ReferenceType>(elem)) {
                if (elem->ok()) Error(
                    a->location(),
                    "Cannot create array of reference type {}",
                    elem
                );
                a->set_sema_errored();
            }

            usz size = 0;
            LCC_ASSERT(a->size(), "Array has NULL size expression");
            Analyse(&a->size());
            if (a->size()->ok()) {
                EvalResult res;
                if (a->size()->evaluate(context, res, false)) {
                    if (res.as_int().slt(1)) {
                        Error(a->location(), "Array size must be greater than 0");
                        a->set_sema_errored();
                    }

                    size = res.as_int().value();
                    a->size() = new (mod) ConstantExpr(a->size(), EvalResult(size));
                } else {
                    // Should be an ICE
                    Error(a->location(), "Array with variable size should have been made a dynamic array by the parser");
                    a->set_sema_errored();
                }
            }
        } break;

        // Apply decltype decay to the element type, prohibit arrays of
        // references, and, if there is an initial size expression, analyse that.
        // Also set cached struct type for IRGen by calling struct_type().
        case Type::Kind::DynamicArray: {
            auto a = as<DynamicArrayType>(type);
            LCC_ASSERT(a->element_type(), "DynamicArray has NULL element type");
            Analyse(&a->element_type());
            a->element_type(DeclTypeDecay(a->element_type()));

            auto elem = a->element_type();
            if (is<ReferenceType>(elem)) {
                if (elem->ok()) Error(
                    a->location(),
                    "Cannot create dynamic array of reference type {}",
                    elem
                );
                a->set_sema_errored();
            }

            // Cache struct type for IRGen.
            (void) a->struct_type(mod);

            if (a->initial_size()) Analyse(&a->initial_size());
        } break;

        /// Analyse the parameters, the return type, and attributes.
        case Type::Kind::Function: {
            auto ty = as<FuncType>(type);
            LCC_ASSERT(ty->return_type(), "Function type has NULL return type");
            Analyse(&ty->return_type());

            for (auto& param : ty->params()) {
                LCC_ASSERT(param.type, "Function type has parameter with NULL type");
                param.type = DeclTypeDecay(param.type);
                Analyse(&param.type);
            }

            /// If the function returns void, it must not be marked discardable.
            if (ty->return_type()->ok() and ty->return_type()->is_void()) {
                if (ty->has_attr(FuncAttr::Discardable))
                    Error(type->location(), "Function returning void cannot be 'discardable'");
            }

            /// Noreturn functions always have side effects.
            if (ty->has_attr(FuncAttr::NoReturn)) {
                if (ty->has_attr(FuncAttr::Const)) Error(
                    type->location(),
                    "'noreturn' function cannot be 'const'"
                );

                if (ty->has_attr(FuncAttr::Pure)) Error(
                    type->location(),
                    "'noreturn' function cannot be 'pure'"
                );
            }

            /// Check for conflicting inline/noinline attributes.
            if (ty->has_attr(FuncAttr::Inline) and ty->has_attr(FuncAttr::NoInline))
                Error(type->location(), "Function cannot be both 'inline' and 'noinline'");
        } break;

        /// Bit width may not be 0.
        case Type::Kind::Integer: {
            if (as<IntegerType>(type)->bit_width() == 0) {
                Error(type->location(), "Bit width of integer type cannot be 0");
                type->set_sema_errored();
            }
        } break;

        /// Calculate size, alignment, and member offsets.
        case Type::Kind::Struct: {
            /// TODO: Packed structs should probably be a separate type altogether and
            /// for those, we’ll have to perform all these calculations below in bits
            /// instead. Cereals!
            auto s = as<StructType>(type);
            usz byte_size = 0;
            usz alignment = 1;

            /// Finalise all members.
            for (auto& member : s->members()) {
                /// Analyse the member type.
                Analyse(&member.type);
                member.type = DeclTypeDecay(member.type);
                if (member.type->sema_errored()) {
                    type->set_sema_errored();
                    continue;
                }

                /// Align the member to its alignment.
                auto msize = member.type->size(context) / 8;
                auto malign = member.type->align(context) / 8;
                member.byte_offset = utils::AlignTo(byte_size, malign);
                byte_size = member.byte_offset + msize;
                alignment = std::max(alignment, malign);
            }

            /// Align the struct to its alignment.
            /// Empty structs have a size of 0.
            s->alignment(alignment);
            s->byte_size(byte_size ? utils::AlignTo(byte_size, alignment) : 0);
        } break;

        /// Calculate enumerator values.
        case Type::Kind::Enum: {
            auto e = as<EnumType>(type);
            LCC_ASSERT(e->underlying_type(), "Enum type has NULL underlying type");

            /// If the underlying type is invalid, default to int so we don’t crash.
            if (not Analyse(&e->underlying_type())) {
                e->set_sema_errored();
                e->underlying_type() = Type::Int;
            }

            /// Check that all enumerators are unique.
            std::unordered_set<std::string> names;
            for (auto& val : e->enumerators()) {
                if (not names.insert(val->name()).second) {
                    Error(val->location(), "Duplicate enumerator '{}'", val->name());
                    e->set_sema_errored();
                }
            }

            /// Assign enumerator values to all enumerators.
            isz next_val = 0;
            for (auto& val : e->enumerators()) {
                val->type(e);

                /// Set the value if there is none.
                if (not val->init()) {
                    val->init() = new (mod) ConstantExpr(e, next_val, val->location());
                    val->set_sema_done();
                }

                /// Analyse the value and evaluate it as an integer; if
                /// either one fails, default the value to 0 so we don’t
                /// crash.
                else {
                    aint res;
                    bool converted;
                    if (not Analyse(&val->init())
                        or not(converted = Convert(&val->init(), e->underlying_type()))
                        or not EvaluateAsInt(val->init(), e->underlying_type(), res)) {
                        if (val->init()->ok() and not converted) Error(
                            val->init()->location(),
                            "Type {} of initialiser is not convertible to {}",
                            val->init()->type(),
                            e->underlying_type()
                        );

                        val->init() = new (mod) ConstantExpr(e, aint(), val->location());
                        val->set_sema_errored();
                    } else {
                        InsertImplicitCast(&val->init(), e);
                        val->init() = new (mod) ConstantExpr(val->init(), EvalResult{res});
                        val->set_sema_done();
                        next_val = isz(as<ConstantExpr>(val->init())->value().as_int().zext(64).value());
                    }
                }

                next_val++;
                if (val->ok()) {
                    auto d = e->scope()->declare(context, auto{val->name()}, val);
                    LCC_ASSERT(not d.is_diag());
                }
            }
        } break;
    }

    /// Do *not* use `type` here, as it may have been replaced by something else.
    if (not(*type_ptr)->sema_done_or_errored()) (*type_ptr)->set_sema_done();
    return (*type_ptr)->ok();
}
