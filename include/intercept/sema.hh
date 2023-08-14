#ifndef INTERCEPT_SEMA_HH
#define INTERCEPT_SEMA_HH

#include <lcc/utils.hh>
#include <intercept/ast.hh>

namespace lcc::intercept {
class Sema {
    Context* context;
    Module& mod;

    Sema(Context* ctx, Module& mod) : context(ctx), mod(mod) {}
public:

    /// Perform semantic analysis on the given module.
    ///
    /// To check for errors, check the has_error() flag
    /// of the context.
    ///
    /// \param ctx The context that owns the module.
    /// \param m The module to analyse.
    static void Analyse(Context* ctx, Module& m);

private:
    void AnalyseModule();
    void AnalyseFunction(FuncDecl* decl);
    void Analyse(Expr** expr);
    void Analyse(Type** type);

    template <typename ...Args>
    Diag Error(Location loc, fmt::format_string<Args...> fmt, Args&& ... args) {
        return Diag::Error(context, loc, fmt, std::forward<Args>(args)...);
    }
};
}

#endif // INTERCEPT_SEMA_HH
