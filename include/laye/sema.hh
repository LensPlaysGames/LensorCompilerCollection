#ifndef LAYE_SEMA_HH
#define LAYE_SEMA_HH

#include <laye/ast.hh>
#include <lcc/utils.hh>

namespace lcc::laye {
class Sema {
    LayeContext* _laye_context;

    /// Whether to use colours in diagnostics.
    bool use_colours;

    FunctionDecl* curr_func;

    Sema(LayeContext* context, bool use_colours)
        : _laye_context(context), use_colours(use_colours) {}

public:
    static void Analyse(LayeContext* context, Module* module, bool use_colours = true);

    auto context() const { return _laye_context->context(); }
    auto laye_context() const { return _laye_context; }

private:
    void Analyse(Module* module, Statement*& statement);
    bool Analyse(Module* module, Expr*& expr, Type* expected_type = nullptr);
    bool AnalyseAndDiscard(Module* module, Expr*& expr) {
        if (not Analyse(module, expr)) return false;
        Discard(module, expr);
        return true;
    }
    bool AnalyseType(Module* module, Type*& type);

    template <bool PerformConversion>
    int ConvertImpl(Module* module, Expr*& expr, Type* to);

    [[nodiscard]] bool Convert(Module* module, Expr*& expr, Type* to);
    void ConvertOrError(Module* module, Expr*& expr, Type* to);
    [[nodiscard]] bool ConvertToCommonType(Module* module, Expr*& a, Expr*& b);
    [[nodiscard]] int TryConvert(Module* module, Expr*& expr, Type* to);

    void Discard(Module* module, Expr*& expr);
    bool HasSideEffects(Expr* expr);

    void InsertImplicitCast(Module* module, Expr*& expr_ptr, Type* ty);
    void InsertPointerToIntegerCast(Module* module, Expr*& operand);
    void WrapWithCast(Module* module, Expr*& expr, Type* type, CastKind kind);

    /// Convert lvalues to rvalues and leave rvalues unchanged.
    ///
    /// This may insert a cast expression.
    /// \return The type of the rvalue.
    auto LValueToRValue(Module* module, Expr*& expr) -> Type*;

    /// Create a (type-checked) pointer to a type.
    auto Ptr(Module* module, Type* type, TypeAccess access) -> PointerType*;

    /// Create a (type-checked) reference to a type.
    auto Ref(Module* module, Type* type, TypeAccess access) -> ReferenceType*;

    auto NameToMangledString(std::string_view s) -> std::string;
    auto TypeToMangledString(Type* type) -> std::string;
    void MangleName(NamedDecl* decl);

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
