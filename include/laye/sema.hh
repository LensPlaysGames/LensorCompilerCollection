#ifndef LAYE_SEMA_HH
#define LAYE_SEMA_HH

#include <laye/ast.hh>
#include <lcc/utils.hh>

namespace lcc::laye {
class Sema {
    LayeContext* _laye_context;
    Module* _module;

    /// Whether to use colours in diagnostics.
    bool use_colours;

    Sema(LayeContext* context, Module* module, bool use_colours)
        : _laye_context(context), _module(module), use_colours(use_colours) {}

public:
    static void Analyse(LayeContext* context, Module* module, bool use_colours = true);

    auto context() const { return _laye_context->context(); }
    auto laye_context() const { return _laye_context; }
    auto module() const { return _module; }

private:
    void Analyse(Module* module);
    void AnalysePrototype(FunctionDecl* func);
    void Analyse(Statement*& statement);
    bool Analyse(Expr*& expr, Type* expected_type = nullptr);
    bool AnalyseAndDiscard(Expr*& expr) {
        if (not Analyse(expr)) return false;
        Discard(expr);
        return true;
    }
    bool AnalyseType(Type*& type);

    template <bool PerformConversion>
    int ConvertImpl(Expr*& expr, Type* to);

    [[nodiscard]] bool Convert(Expr*& expr, Type* to);
    void ConvertOrError(Expr*& expr, Type* to);
    [[nodiscard]] bool ConvertToCommonType(Expr*& a, Expr*& b);
    [[nodiscard]] int TryConvert(Expr*& expr, Type* to);

    void Discard(Expr*& expr);
    bool HasSideEffects(Expr* expr);

    void InsertImplicitCast(Expr*& expr_ptr, Type* ty);
    void InsertPointerToIntegerCast(Expr*& operand);
    void WrapWithCast(Expr*& expr, Type* type, CastKind kind);

    auto Ptr(Type* type) -> PointerType*;

    /// Issue a note.
    template <typename... Args>
    Diag Note(Location where, fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag::Note(context(), where, fmt, std::forward<Args>(args)...);
    }

    /// Issue a warning.
    template <typename... Args>
    Diag Warning(Location where, fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag::Warning(context(), where, fmt, std::forward<Args>(args)...);
    }

    /// Issue an error.
    template <typename... Args>
    Diag Error(Location where, fmt::format_string<Args...> fmt, Args&&... args) {
        return Diag::Error(context(), where, fmt, std::forward<Args>(args)...);
    }
};
} // namespace lcc::laye

#endif // LAYE_SEMA_HH