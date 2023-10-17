#include <intercept/sema.hh>
#include <lcc/context.hh>
#include <lcc/utils/macros.hh>

namespace intc = lcc::intercept;
/// ===========================================================================
///  Helpers
/// ===========================================================================
bool intc::Sema::AnalyseAndDiscard(Expr** expr) {
    if (not Analyse(expr)) return false;
    Discard(expr);
    return true;
}

bool intc::Sema::Convert(Expr** expr, Type* type) {
    if ((*expr)->sema_errored()) return true;
    return ConvertImpl<true>(expr, type) >= 0;
}

/// For an explanation of the return value of this function, see
/// the comment on the declaration of TryConvert().
template <bool PerformConversion>
int intc::Sema::ConvertImpl(intc::Expr** expr_ptr, intc::Type* to) {
    enum : int {
        TypesContainErrors = -2,
        ConversionImpossible = -1,
        NoOp = 0,
    };

    /// If the types contain errors, return -2.
    auto from = (*expr_ptr)->type();
    if (from->sema_errored() or to->sema_errored()) return TypesContainErrors;

    /// This is so we don’t forget that we’ve applied lvalue-to-rvalue
    /// conversion and raised the score by one.
    bool requires_lvalue_to_rvalue_conversion = false;
    auto Score = [&](int i) {
        LCC_ASSERT(i, "Score must be 1 or greater. Use the enum constants above for values <= 0");
        return i + int(requires_lvalue_to_rvalue_conversion);
    };

    /// Any type can be converted to void.
    if (to->is_void()) return NoOp;

    /// Any type can be converted to itself.
    if (Type::Equal(from, to)) return NoOp;

    /// Function types can be converted to their corresponding function types.
    if (from->is_function() and to->is_pointer() and Type::Equal(to->elem(), from)) {
        if constexpr (PerformConversion) InsertImplicitCast(expr_ptr, to);
        return NoOp;
    }

    /// Try deproceduring.
    if (Deproceduring(expr_ptr)) return Score(1);

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

    /// Lvalues and are convertible to references.
    if (to->is_reference()) { return (*expr_ptr)->is_lvalue() ? NoOp : ConversionImpossible; }

    /// Any conversions after this require lvalue-to-rvalue conversion
    /// first if the expression is an lvalue.
    requires_lvalue_to_rvalue_conversion = (*expr_ptr)->is_lvalue();
    if constexpr (PerformConversion) from = LValueToRValue(expr_ptr);
    else from = from->strip_references();

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

    /// Function types can be converted to their corresponding function types.
    if (from->is_function() and to->is_pointer() and Type::Equal(to->elem(), from)) {
        if constexpr (PerformConversion) InsertImplicitCast(expr_ptr, to);
        return NoOp;
    }

    /// Integer to boolean implicit conversions.
    if (from->is_integer() and to->is_bool()) {
        return Score(1);
    }

    /// Integer to integer
    ///
    /// For portability, we would ideally not make any assumptions about
    /// the size of `int`, but the issue with that is that it would make
    /// most code rather cumbersome to write as you’d have to, e.g., cast
    /// an `i16` to `int` manually. Moreover, integer literals are of type
    /// `int`, so that would also cause problems. C FFI types suffer from
    /// similar problems, so we just use their width on the target.
    if (from->is_integer() and to->is_integer()) {
        /// Integer types (but not bool) are convertible to each other if
        /// the value is known at compile time and in range for the type
        /// it is being converted to.
        ///
        /// Note that there shouldn’t really be a way that the conversion
        /// would make sense if the value is known at compile time to not
        /// be in range for the target type.
        EvalResult res;
        if ((*expr_ptr)->evaluate(context, res, false)) {
            auto val = res.as_i64();
            if (val < 0 and to->is_unsigned_int(context)) return ConversionImpossible;

            /// Note: We currently don’t support integer constants larger than 64
            /// bits internally, so if the type has a bit width larger than 64, it
            /// will always fit.
            auto bits = to->size(context);
            if (bits < 64 and u64(val) > u64(utils::MaxBitValue(bits))) return ConversionImpossible;
            if constexpr (PerformConversion) {
                InsertImplicitCast(expr_ptr, to);
                *expr_ptr = new (mod) ConstantExpr(*expr_ptr, res);
            }
            return Score(1);
        }

        /// Furthermore, smaller sized integer types are convertible to larger sized
        /// integer types, so long as we’re not converting from a signed to an unsigned
        /// type.
        if (
            from->size(context) <= to->size(context) and
            (from->is_unsigned_int(context) or to->is_signed_int(context))
        ) {
            if constexpr (PerformConversion) InsertImplicitCast(expr_ptr, to);
            return Score(1);
        }

        return ConversionImpossible;
    }

    /// Try deproceduring one last time.
    if (Deproceduring(expr_ptr)) return Score(1);

    return ConversionImpossible;
}

void intc::Sema::ConvertOrError(Expr** expr, Type* to) {
    if (not Convert(expr, to)) Error(
        (*expr)->location(),
        "Expression is not convertible to type {}",
        to
    );
}

bool intc::Sema::ConvertToCommonType(Expr** a, Expr** b) {
    return Convert(a, (*b)->type()) or Convert(b, (*a)->type());
}

auto intc::Sema::DeclTypeDecay(Type* type) -> Type* {
    return type->is_function() ? Ptr(type) : type;
}

bool intc::Sema::Deproceduring(Expr** expr_ptr) {
    /// This conversion only applies to functions and function pointers.
    auto expr = *expr_ptr;
    auto ty = expr->type();
    if (
        not ty->is_function() and
        (not ty->is_pointer() or not ty->elem()->is_function())
    ) return false;

    /// Function declarations are never deprocedured automatically.
    if (is<FuncDecl>(expr)) return false;

    /// Functions that take arguments are not affected.
    auto ftype = cast<FuncType>(ty->is_function() ? ty : ty->elem());
    if (not ftype->params().empty()) return false;

    /// Otherwise, insert a call.
    *expr_ptr = new (mod) CallExpr(expr, {}, expr->location());
    Analyse(expr_ptr);
    return true;
}

void intc::Sema::Discard(Expr** expr_ptr) {
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

bool intc::Sema::HasSideEffects(Expr* expr) {
    switch (expr->kind()) {
        /// These always have side effects.
        case Expr::Kind::While:
        case Expr::Kind::For:
        case Expr::Kind::Return:
        case Expr::Kind::StructDecl:
        case Expr::Kind::TypeAliasDecl:
        case Expr::Kind::VarDecl:
        case Expr::Kind::FuncDecl:
            return true;

        /// These never have side effects.
        case Expr::Kind::IntegerLiteral:
        case Expr::Kind::StringLiteral:
        case Expr::Kind::OverloadSet:
        case Expr::Kind::NameRef:
            return false;

        /// For these, it depends.
        case Expr::Kind::EvaluatedConstant:
            return HasSideEffects(as<ConstantExpr>(expr)->expr());

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

        case Expr::Kind::Binary: {
            auto b = as<BinaryExpr>(expr);
            if (HasSideEffects(b->lhs()) or HasSideEffects(b->rhs())) return true;
            return b->op() == TokenKind::ColonEq;
        }

        case Expr::Kind::If: {
            auto i = as<IfExpr>(expr);
            if (HasSideEffects(i->condition())) return true;
            if (HasSideEffects(i->then())) return true;
            return i->else_() and HasSideEffects(i->else_());
        }

        case Expr::Kind::Call: {
            auto c = as<CallExpr>(expr);
            auto f = c->callee_type();
            if (HasSideEffects(c->callee())) return true;
            if (rgs::any_of(c->args(), HasSideEffects)) return true;
            return not f->has_attr(FuncAttr::Pure) and not f->has_attr(FuncAttr::Const);
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

void intc::Sema::InsertImplicitCast(Expr** expr_ptr, Type* ty) {
    WrapWithCast(expr_ptr, ty, CastKind::ImplicitCast);
}

void intc::Sema::InsertPointerToIntegerCast(Expr** operand) {
    if ((*operand)->type()->is_pointer())
        InsertImplicitCast(operand, Type::Int);
}

auto intc::Sema::LValueToRValue(Expr** expr) -> Type* {
    /// Functions are cast to function pointers.
    if ((*expr)->type()->is_function()) {
        auto ty = Ptr((*expr)->type());
        WrapWithCast(expr, ty, CastKind::LValueToRValueConv);
        return ty;
    }

    /// Otherwise, remove references and cast to that.
    auto ty = (*expr)->type()->strip_references();
    if (not Type::Equal(ty, (*expr)->type()))
        WrapWithCast(expr, ty, CastKind::LValueToRValueConv);
    return ty;
}

auto intc::Sema::Ptr(Type* ty) -> PointerType* {
    Type* ptr = new (mod) PointerType(ty, ty->location());
    Analyse(&ptr);
    return as<PointerType>(ptr);
}

auto intc::Sema::Ref(Type* ty) -> ReferenceType* {
    Type* ref = new (mod) ReferenceType(ty, ty->location());
    Analyse(&ref);
    return as<ReferenceType>(ref);
}

int intc::Sema::TryConvert(Expr** expr, Type* type) {
    return ConvertImpl<false>(expr, type);
}

void intc::Sema::WrapWithCast(Expr** expr_ptr, Type* type, CastKind kind) {
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
void intc::Sema::Analyse(Context* ctx, Module& m, bool use_colours) {
    if (ctx->has_error()) return;
    Sema s{ctx, m, use_colours};
    return s.AnalyseModule();
}

void intc::Sema::AnalyseModule() {
    /// TODO(Sirraide): Load imported modules.
    LCC_ASSERT(mod.imports().empty(), "Importing modules is not yet supported.");

    /// Analyse the signatures of all functions. This must be done
    /// before analysing bodies since, in order to perform overload
    /// resolution properly, we first need to apply decltype decay
    /// to all parameters (e.g. convert parameters of function type
    /// to function pointers etc.).
    for (auto& func : mod.functions()) AnalyseFunctionSignature(func);

    /// Analyse function bodies.
    for (auto& func : mod.functions()) AnalyseFunctionBody(func);
}

void intc::Sema::AnalyseFunctionBody(FuncDecl* decl) {
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
            if (block->children().empty() and not ty->return_type()->is_void()) {
                Error(decl->location(), "Non-void function must return a value");
                return;
            }

            last = &block->children().back();
        } else {
            last = &decl->body();
        }

        if (is<ReturnExpr>(*last)) return;

        if (not Convert(last, ty->return_type())) Error(
            (*last)->location(),
            "Type of last expression {} is not convertible to return type {}",
            (*last)->type(),
            ty->return_type()
        );

        // Insert a `ReturnExpr` which returns `last`.
        if (auto block = cast<BlockExpr>(decl->body()))
            block->add(new (mod) ReturnExpr(*last, {}));
        else decl->body() = new (mod) ReturnExpr(*last, {});
    } else {
        Discard(&decl->body());
    }
}

void intc::Sema::AnalyseFunctionSignature(FuncDecl* decl) {
    /// Set a name for the decl if it’s empty.
    if (decl->name().empty()) decl->name(mod.unique_function_name());

    /// Typecheck the function type.
    Analyse(decl->type_ref());

    /// Used attribute is ignored on functions that aren’t internal. If
    /// the function is internal, then set the linkage to used so it isn’t
    /// deleted by the optimiser.
    auto ty = as<FuncType>(decl->type());
    if (ty->has_attr(FuncAttr::Used)) {
        if (decl->linkage() != Linkage::Internal) {
            Diag::Warning(context, decl->location(), "'used' has no effect on this function");
        } else {
            decl->linkage(Linkage::Used);
        }
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
bool intc::Sema::Analyse(Expr** expr_ptr, Type* expected_type) {
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
                if (not r->value()) Error(r->location(), "Non-void function must return a value");
                else if (not Convert(&r->value(), ret_type)) Error(
                    r->location(),
                    "Type of return expression is not convertible to return type {}",
                    ret_type
                );
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

            /// Analyse the branches.
            Analyse(&i->then());
            if (i->else_()) Analyse(&i->else_());

            /// If the branches are both void, then the type of the if statement
            /// is void. Otherwise, it is the common type of the two branches.
            if (i->then()->ok() and (not i->else_() or i->else_()->ok())) {
                if (not i->else_() or not ConvertToCommonType(&i->then(), &i->else_())) i->type(Type::Void);
                else i->type(i->then()->type());
            } else {
                i->set_sema_errored();
            }

            if (i->type()->is_void()) {
                Discard(&i->then());
                if (i->else_()) Discard(&i->else_());
            }
        } break;

        /// The type of a block is the type of its last expression. Type
        /// inference is only used for the last expression in the block.
        case Expr::Kind::Block: {
            auto b = as<BlockExpr>(expr);
            if (b->children().empty()) {
                b->type(Type::Void);
                break;
            }

            for (auto*& child : b->children()) {
                const bool last = &child == &b->children().back();
                if (not Analyse(&child, last ? expected_type : nullptr)) b->set_sema_errored();
                if (not last and child->ok()) Discard(&child);
            }

            if (not b->sema_errored()) b->type(b->children().back()->type());
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
                Analyse(&v->init(), infer_type ? nullptr : DeclTypeDecay(v->type()));

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
            if (v->init() and not Convert(&v->init(), v->type())) Error(
                v->init()->location(),
                "Type of initialiser is not convertible to variable type {}",
                v->type()
            );
        } break;

        /// Currently, each expression of a compound literal must be
        /// convertible to the same type.
        case Expr::Kind::CompoundLiteral: {
            auto c = as<CompoundLiteral>(expr);

            /// Analyse all subexpressions.
            for (auto*& child : c->values()) {
                if (Analyse(&child)) LValueToRValue(&child);
                else c->set_sema_errored();
            }

            /// If there wasn’t an error, then make sure all subexpressions
            /// have the same type.
            if (not c->sema_errored()) {
                auto ty = c->values().front()->type();
                for (auto*& child : c->values() | vws::drop(1)) {
                    if (not Convert(&child, ty)) Error(
                        child->location(),
                        "Type of compound literal element is not convertible to type {}",
                        ty
                    );
                }
            }

            /// Type is an array type for now.
            c->type(new (mod) ArrayType(
                c->values().front()->type(),
                new (mod) IntegerLiteral(c->values().size(), {})
            ));

            LCC_ASSERT(Analyse(c->type_ref()));
        } break;

        /// LHS must be a (pointer to a) struct, and the identifier must
        /// exist in the struct.
        case Expr::Kind::MemberAccess: {
            auto m = as<MemberAccessExpr>(expr);

            /// If there is an error analysing the object, we don’t know
            /// its type an can thus not continue checking this.
            if (not Analyse(&m->object())) {
                m->set_sema_errored();
                break;
            }

            /// TODO(Sirraide): Accessing ‘members’ of modules.

            /// Type must be a struct type.
            auto struct_type = cast<StructType>(m->object()->type()->strip_pointers_and_references());
            if (struct_type) {
                Error(
                    m->location(),
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

            /// If the object is a pointer or reference, we need to dereference it
            /// until only an lvalue is left.
            auto obj = m->object();
            for (;;) {
                /// If the object is a reference, then its object type must be
                /// a pointer type or the struct. If it’s the struct, we’re done.
                if (auto ref = cast<ReferenceType>(obj->type())) {
                    if (ref->element_type()->is_struct()) break;

                    /// Otherwise, load the pointer.
                    LValueToRValue(&obj);
                }

                /// If the object is a pointer, then we need to dereference it.
                if (is<PointerType>(obj->type())) {
                    obj = new (mod) UnaryExpr(TokenKind::At, obj, false, obj->location());
                    LCC_ASSERT(Analyse(&obj));
                }
            }

            /// A member access is an lvalue iff the object is an lvalue.
            m->object(obj);
            m->type(m->is_lvalue() ? Ref(it->type) : it->type);
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
        case Expr::Kind::StructDecl:
        case Expr::Kind::TypeAliasDecl:
            break;

        /// There isn’t really a way these could be malformed.
        case Expr::Kind::IntegerLiteral:
        case Expr::Kind::StringLiteral:
            break;

        /// These should only be created by sema and are thus no-ops.
        case Expr::Kind::EvaluatedConstant: break;

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
                        Error(oi->location(), "Overload set contains two overloads with the same parameter types");
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

void intc::Sema::AnalyseBinary(BinaryExpr* b) {
    /// Give up if there is an error in either operand.
    if (not Analyse(&b->lhs()) or not Analyse(&b->rhs())) {
        b->set_sema_errored();
        return;
    }

    switch (b->op()) {
        default: Diag::ICE("Invalid binary operator '{}'", ToString(b->op()));

        /// Pointer or array subscript.
        case TokenKind::LBrack: {
            auto ty = b->lhs()->type()->strip_references();
            if (not is<PointerType, ArrayType>(ty)) {
                Error(b->location(), "LHS of subscript must be a pointer or array, but was {}", b->lhs()->type());
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
            if (auto arr = as<ArrayType>(ty); arr and arr->size()->ok()) {
                EvalResult res;
                if (b->rhs()->evaluate(context, res, false)) {
                    if (res.as_i64() < 0 or res.as_i64() >= as<ConstantExpr>(arr->size())->value().as_i64())
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
            if (not b->lhs()->is_assignable_lvalue()) {
                Error(b->location(), "LHS of assignment must be an (assignable) lvalue");
                b->set_sema_errored();
                return;
            }

            /// The type of the assignment is the same lvalue. Note that if
            /// the lhs is indeed an lvalue, we don’t ever mark this as errored
            /// because we know what its type is going to be, irrespective of
            /// whether the assignment if valid or not.
            b->type(Ref(b->lhs()->type()));

            /// The RHS must be assignable to the LHS.
            auto var_type = b->lhs()->type()->strip_references();
            if (not Convert(&b->rhs(), var_type)) {
                Error(
                    b->location(),
                    "Type of expression {} is not convertible to variable type {}",
                    b->rhs()->type(),
                    var_type
                );
                return;
            }
        } break;
    }
}

void intc::Sema::AnalyseCall(Expr** expr_ptr, CallExpr* expr) {
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

        /// TODO(Sirraide): Overload resolution.
        Diag::ICE("Sorry, overload resolution is currently not implemented");
    }

    /// If the callee is a function pointer, dereference it.
    if (auto ty = expr->callee()->type(); ty->is_pointer() and ty->elem()->is_function())
        InsertImplicitCast(&expr->callee(), ty->elem());

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

    /// Check that the arguments are convertible to the parameter types.
    for (usz i = 0, end = std::min(expr->args().size(), func_type->params().size()); i < end; i++) {
        if (not Convert(expr->args().data() + i, func_type->params()[i].type)) Error(
            expr->args()[i]->location(),
            "Type of argument {} is not convertible to parameter type {}",
            expr->args()[i]->type(),
            func_type->params()[i].type
        );
    }
}

void intc::Sema::AnalyseCast(CastExpr* c) {
    /// Implicit casts and lvalue-to-rvalue conversions are
    /// only ever created by sema, so we know they’re fine.
    if (c->is_implicit_cast() or c->is_lvalue_to_rvalue()) return;

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

    /// Explicitly casting from integers to integers and
    /// integers to booleans and booleans to integers is allowed.
    if (from->is_integer(true) and to->is_integer(true)) return;

    /// Casting from pointers to integers and pointers to booleans is allowed.
    if (from->is_pointer() and to->is_integer(true)) return;

    /// Hard casts between pointers and from pointers to integers are
    /// allowed. Note that, if the pointers are compatible, the call to
    /// Convert() above will have already taken care of this case, so we
    /// don’t need to check for that here.
    if (to->is_pointer() and (from->is_integer() or from->is_pointer())) {
        if (c->is_hard_cast()) return;
        Error(
            c->location(),
            "Cast from {} to {} is unsafe. If this is intended, use 'as!' instead",
            from,
            to
        );
        return;
    }

    /// Hard casts between types that have the same size are allowed.
    if (from->size(context) == to->size(context) and c->is_hard_cast()) return;

    /// Any other casts are currently not allowed.
    Error(c->location(), "Invalid cast from {} to {}", from, to);
}

void intc::Sema::AnalyseIntrinsicCall(Expr** expr_ptr, IntrinsicCallExpr* expr) {
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
            if (not expr->args().empty()) Error(expr->location(), "__builtin_line() takes no arguments");
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
            }

            /// Syscalls all return integer.
            expr->type(Type::Int);
        } break;
    }
}

void intc::Sema::AnalyseNameRef(NameRefExpr* expr) {
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
    /// TODO(Sirraide): Search imported modules here.
    if (syms.first == syms.second) {
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
        expr->type(Ref(syms.first->second->type()));
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

void intc::Sema::AnalyseUnary(UnaryExpr* u) {
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

            u->type(Ptr(as<ReferenceType>(u->operand()->type())->element_type()));
        } break;

        /// Convert a pointer to an lvalue.
        case TokenKind::At: {
            /// The pointer itself must be an rvalue.
            auto ty = LValueToRValue(&u->operand());
            if (not is<PointerType>(ty)) {
                Error(u->location(), "Cannot dereference non-pointer type {}", ty);
                u->set_sema_errored();
                break;
            }

            u->type(Ref(as<PointerType>(ty)->element_type()));
        } break;

        /// Negate an integer.
        case TokenKind::Minus: {
            auto ty = LValueToRValue(&u->operand());
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
            auto ty = LValueToRValue(&u->operand());
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
            auto ty = LValueToRValue(&u->operand());
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
bool intc::Sema::Analyse(Type** type_ptr) {
    auto type = *type_ptr;

    /// Don’t analyse the same type twice.
    if (type->sema_done_or_errored()) return type->ok();
    type->set_sema_in_progress();

    switch (type->kind()) {
        /// These are marked as done in the constructor.
        case Type::Kind::Builtin: LCC_UNREACHABLE();

        /// These are no-ops.
        case Type::Kind::FFIType: break;

        /// Named types need to be resolved to a type.
        case Type::Kind::Named: {
            auto n = as<NamedType>(type);

            /// This code is similar to name resolution for expressions,
            /// except that we don’t need to worry about overloads.
            Type* ty{};
            for (auto scope = n->scope(); scope; scope = scope->parent()) {
                auto syms = scope->find(n->name());
                if (syms.first == syms.second) continue;
                if (auto s = cast<StructDecl>(syms.first->second)) {
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
                    "Because of declaration of '{}' here",
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
            Analyse(&p->element_type());

            auto elem = p->element_type();
            if (is<ReferenceType>(elem)) {
                if (elem->ok()) Error(p->location(), "Cannot create pointer to reference type {}", elem);
                p->set_sema_errored();
            }
        } break;

        /// References to references are collapsed to a single reference.
        case Type::Kind::Reference: {
            auto r = as<ReferenceType>(type);
            Analyse(&r->element_type());

            /// Collapse refs.
            while (is<ReferenceType>(r->element_type()))
                r->element_type(r->element_type()->elem());
        } break;

        /// Apply decltype decay to the element type and prohibit
        /// arrays of references. Also check the size.
        case Type::Kind::Array: {
            auto a = as<ArrayType>(type);
            Analyse(&a->element_type());
            a->element_type(DeclTypeDecay(a->element_type()));

            auto elem = a->element_type();
            if (is<ReferenceType>(elem)) {
                if (elem->ok()) Error(a->location(), "Cannot create array of reference type {}", elem);
                a->set_sema_errored();
            }

            /// Array size must be known at compile time.
            Analyse(&a->size());
            EvalResult res;
            usz size = 1;
            if (a->size()->ok() and a->size()->evaluate(context, res, false)) {
                if (res.as_i64() < 1) {
                    Error(a->location(), "Array size must be greater than 0");
                    a->set_sema_errored();
                }

                size = usz(res.as_i64());
            } else {
                if (a->size()->ok()) Error(a->location(), "Array size must be known at compile time");
                a->set_sema_errored();
            }

            /// Always create a constant expression for the array size as
            /// other parts of the program assume that array sizes are
            /// constant.
            a->size() = new (mod) ConstantExpr(a->size(), EvalResult{i64(size)});
        } break;

        /// Analyse the parameters, the return type, and attributes.
        case Type::Kind::Function: {
            auto ty = as<FuncType>(type);
            Analyse(&ty->return_type());
            for (auto& param : ty->params()) {
                param.type = DeclTypeDecay(param.type);
                Analyse(&param.type);
            }

            /// If the function returns void, it must not be discardable.
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
            /// TODO(Sirraide): Packed structs should probably be a separate
            ///     type altogether and for those, we’ll have to perform all
            ///     these calculations below in bits instead.
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
            /// TODO(Sirraide): Should empty structs have a size of 1 or 0?
            ///     Currently, it’s 0.
            s->alignment(alignment);
            s->byte_size(byte_size ? utils::AlignTo(byte_size, alignment) : 0);
        } break;
    }

    /// Do *not* use `type` here, as it may have been replaced by something else.
    if (not(*type_ptr)->sema_done_or_errored()) (*type_ptr)->set_sema_done();
    return (*type_ptr)->ok();
}
