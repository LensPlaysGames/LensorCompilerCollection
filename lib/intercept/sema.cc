#include <intercept/sema.hh>
#include <lcc/context.hh>
#include <lcc/utils/macros.hh>

namespace intc = lcc::intercept;

void intc::Sema::Analyse(Context* ctx, Module& m, bool use_colours) {
    if (ctx->has_error()) return;
    Sema s{ctx, m, use_colours};
    return s.AnalyseModule();
}

void intc::Sema::AnalyseModule() {
    /// TODO(Sirraide): Load imported modules.
    LCC_ASSERT(mod.imports().empty(), "Importing modules is not yet supported.");

    /// Analyse the top-level function.
    AnalyseFunction(mod.top_level_func());

    /// Analyse all other functions.
    for (auto& func : mod.functions()) {
        if (func == mod.top_level_func()) continue;
        AnalyseFunction(func);
    }
}

void intc::Sema::AnalyseFunction(FuncDecl* decl) {
    tempset curr_func = decl;

    /// Set a name for the decl if it’s empty.
    if (decl->name().empty()) decl->name(mod.unique_function_name());

    /// Typecheck the function type.
    Analyse(decl->type_ref());

    /// If the function returns void, it must not be discardable.
    auto ty = as<FuncType>(decl->type());
    if (ty->return_type()->is_void()) {
        if (ty->has_attr(FuncAttr::Discardable))
            Error(decl->location(), "Function returning void cannot be 'discardable'");
    }

    /// Noreturn functions always have side effects.
    if (ty->has_attr(FuncAttr::NoReturn)) {
        if (ty->has_attr(FuncAttr::Const)) Error(decl->location(), "'noreturn' function cannot be 'const'");
        if (ty->has_attr(FuncAttr::Pure)) Error(decl->location(), "'noreturn' function cannot be 'pure'");
    }

    /// Check for conflicting inline/noinline attributes.
    if (ty->has_attr(FuncAttr::Inline) and ty->has_attr(FuncAttr::NoInline))
        Error(decl->location(), "Function cannot be both 'inline' and 'noinline'");

    /// Used attribute is ignored on functions that aren’t internal. If
    /// the function is internal, then set the linkage to used so it isn’t
    /// deleted by the optimiser.
    if (ty->has_attr(FuncAttr::Used)) {
        if (decl->linkage() != Linkage::Internal) {
            Diag::Warning(context, decl->location(), "'used' has no effect on this function");
        } else {
            decl->linkage(Linkage::Used);
        }
    }

    /// Typecheck the function body. If the function has no body, then we’re done.
    if (decl->body()) Analyse(&decl->body(), ty->return_type());
    else return;

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
        Convert(last, ty->return_type());
    }
}

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
    if (expr->sema_done_or_errored()) return expr->ok();
    expr->set_sema_in_progress();

    /// Analyse the type if there is one.
    if (auto tc = cast<TypedExpr>(expr)) Analyse(tc->type_ref());

    /// Analyse the expression itself.
    switch (expr->kind()) {
        /// The condition of a loop must be convertible to bool.
        case Expr::Kind::For: {
            auto f = as<ForExpr>(expr);
            Analyse(&f->init());
            Analyse(&f->increment());
            [[fallthrough]];
        }

        case Expr::Kind::While: {
            auto w = as<Loop>(expr);
            Analyse(&w->condition());
            Convert(&w->condition(), Type::Bool);
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
                else Convert(&r->value(), ret_type);
            }
        } break;

        /// The condition of an if statement must be convertible to bool, and
        /// its type is the common type of the two branches.
        case Expr::Kind::If: {
            auto i = as<IfExpr>(expr);
            Analyse(&i->condition());
            Convert(&i->condition(), Type::Bool);

            /// Analyse the branches.
            Analyse(&i->then());
            if (i->else_()) Analyse(&i->else_());

            /// If the branches are both void, then the type of the if statement
            /// is void. Otherwise, it is the common type of the two branches.
            if (i->then()->ok() and (not i->else_() or i->else_()->ok())) {
                if (not i->else_() or not ConvertToCommonType(&i->then(), &i->else_(), false)) i->type(Type::Void);
                else i->type(i->then()->type());
            } else {
                i->set_sema_errored();
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

            for (auto*& child : b->children())
                if (not Analyse(&child, &child == &b->children().back() ? expected_type : nullptr))
                    b->set_sema_errored();

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
            if (v->init()) Convert(&v->init(), v->type());
        } break;

        /// Currently, each expression of a compound literal must be
        /// convertible to the same type.
        case Expr::Kind::CompoundLiteral: {
            auto c = as<CompoundLiteral>(expr);

            /// Analyse all subexpressions.
            for (auto*& child : c->values())
                if (not Analyse(&child))
                    c->set_sema_errored();

            /// If there wasn’t an error, convert to the common type.
            if (not c->sema_errored()) ConvertToCommonType(c->values(), true);
        } break;

        /// Functions are analysed separately.
        case Expr::Kind::FuncDecl:
            LCC_ASSERT(expr->type()->is_function());
            break;

        /// LHS must be a struct, and the identifier must exist in the struct.
        case Expr::Kind::MemberAccess: {

        } break;

        /// The actual work here is analysing the type, so this is a no-op.
        case Expr::Kind::StructDecl: break;
        case Expr::Kind::TypeAliasDecl: break;

        /// There isn’t really a way these could be malformed.
        case Expr::Kind::IntegerLiteral: break;
        case Expr::Kind::StringLiteral: break;

        /// These should only be created by sema and are thus no-ops.
        case Expr::Kind::EvaluatedConstant: break;

        case Expr::Kind::Unary: break;
        case Expr::Kind::Binary: break;
        case Expr::Kind::NameRef: break;
    }

    /// Do *not* use `expr` here, as it may have been replaced by something else.
    if (not(*expr_ptr)->sema_done_or_errored()) (*expr_ptr)->set_sema_done();
    return (*expr_ptr)->ok();
}

void lcc::intercept::Sema::AnalyseCast(CastExpr* c) {
    /// Implicit casts and lvalue-to-rvalue conversions are
    /// only ever created by sema, so we know they’re fine.
    if (c->is_implicit_cast() or c->is_lvalue_to_rvalue()) return;

    /// If analysis of the operand failed, we don’t know its
    /// type and thus have no way of checking whether the cast
    /// makes sense.
    if (not Analyse(&c->operand(), c->type())) return;

    /// If the types are implicitly convertible, then the cast
    /// is fine.
    if (TryConvert(&c->operand(), c->type())) return;

    /// Remove references from the operand.
    LvalueToRvalue(&c->operand());

    /// All conversions that rely on references have already been
    /// taken care of by TryConvert(), so we don’t care about
    /// references anymore at this point.
    ///
    /// Thus, the type we’re casting to must not be a reference type.
    auto from = c->operand()->type();
    auto to = c->type();
    if (to->is_reference()) {
        Error(c->location(), "Invalid cast of rvalue to reference type");
        return;
    }

    /// Casting from pointers to integers is allowed.
    if (from->is_pointer() and to->is_any_integer(true)) return;

    /// Hard casts between pointers and from pointers to integers are allowed.
    if (from->is_pointer() and (to->is_pointer() or to->is_any_integer(true))) {
        if (c->is_hard_cast()) return;
        Error(
            c->location(),
            "Cast from {} to {} is unsafe. If the cast was really intended, try using 'as!' instead",
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

void lcc::intercept::Sema::AnalyseIntrinsicCall(Expr** expr_ptr, IntrinsicCallExpr* expr) {
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
            ReplaceWithNewNode(expr_ptr, new (mod) ConstantExpr(expr, str));
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
            expr->type(Type::Integer);
            expr->set_sema_done();

            /// If possible, seek to the location, if not we just insert 0.
            i64 line = 0;
            if (expr->location().seekable(context)) line = i64(expr->location().seek_line_column(context).line);
            ReplaceWithNewNode(expr_ptr, new (mod) ConstantExpr(expr, line));
        } break;

        case IntrinsicKind::BuiltinMemCopy: {
            /// This takes two pointer and a size argument.
            if (expr->args().size() != 3)
                Error(expr->location(), "__builtin_memcpy() takes exactly three arguments");

            /// Analyse the arguments.
            for (auto*& arg : expr->args()) Analyse(&arg);
            Convert(&expr->args()[0], Type::VoidPtr);
            Convert(&expr->args()[1], Type::VoidPtr);
            Convert(&expr->args()[2], Type::Integer);

            /// Unlike C’s memcpy()/memmove(), this returns nothing.
            expr->type(Type::Void);
        } break;

        case IntrinsicKind::BuiltinMemSet: {
            /// This takes two pointer and a size argument.
            if (expr->args().size() != 3)
                Error(expr->location(), "__builtin_memset() takes exactly three arguments");

            /// Analyse the arguments.
            for (auto*& arg : expr->args()) Analyse(&arg);
            Convert(&expr->args()[0], Type::VoidPtr);
            Convert(&expr->args()[1], Type::Byte);
            Convert(&expr->args()[2], Type::Integer);

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
                Convert(&arg, Type::Integer);
            }

            /// Syscalls all return integer.
            expr->type(Type::Integer);
        } break;
    }
}
