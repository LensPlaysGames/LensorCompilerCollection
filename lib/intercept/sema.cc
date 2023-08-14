#include <intercept/sema.hh>
#include <lcc/context.hh>

namespace intc = lcc::intercept;

void intc::Sema::Analyse(Context* ctx, Module& m) {
    if (ctx->has_error()) return;
    Sema s{ctx, m};
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
    /// Set a name for the decl if it’s empty.
    if (decl->name().empty()) decl->name(mod.unique_function_name());

    /// Typecheck the function type.
    Analyse(decl->type_ref());

    /// If the function returns void, it must not be discardable.
    auto ty = as<FuncType>(decl->type());
    if (ty->return_type()->is_void()) {
        if (ty->has_attr(FuncAttr::Discardable))
            Error(decl->location(), "Function returning void cannot be discardable");
    }

    /// TODO: Check other attributes.

    /// Typecheck the function body. If the function has no body, then we’re done.
    if (decl->body()) Analyse(&decl->body());
    else return;

    /// If the function returns a value, all paths must return a
    /// value. Note that if there is a return expression in the
    /// middle of a block, everything after it will already have
    /// been deleted by the code that handles blocks.
    std::vector<Expr*> paths;
    paths.push_back(decl->body());
    while (not paths.empty()) {
        /// TODO.
    }


}

void intc::Sema::Analyse(Expr** expr_ptr) {
    auto expr = *expr_ptr;

    /// Don’t analyse the same expression twice.
    if (expr->type_checked()) return;
    expr->set_type_checked();

    /// Typecheck the type if there is one.
    if (auto tc = cast<TypedExpr>(expr)) Analyse(tc->type_ref());
}