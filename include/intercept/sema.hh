#ifndef INTERCEPT_SEMA_HH
#define INTERCEPT_SEMA_HH

#include <intercept/ast.hh>
#include <lcc/utils.hh>

namespace lcc::intercept {
class Sema {
    Context* context;
    Module& mod;

    /// The function we’re currently analysing.
    FuncDecl* curr_func;

    /// Whether to use colours in diagnostics.
    bool use_colours;

    Sema(Context* ctx, Module& mod, bool use_colours)
        : context(ctx),
          mod(mod),
          curr_func(mod.top_level_func()),
          use_colours(use_colours) {}
public:
    /// Perform semantic analysis on the given module.
    ///
    /// To check for errors, check the has_error() flag
    /// of the context.
    ///
    /// \param ctx The context that owns the module.
    /// \param m The module to analyse.
    /// \param use_colours Whether to use colours in diagnostics.
    static void Analyse(Context* ctx, Module& m, bool use_colours = false);

private:
    /// \see Error()
    template <typename Ty>
    struct format_type {
        using type = Ty;
    };

    template <typename Ty>
    requires requires (Ty ty) { cast<Type>(ty); }
    struct format_type<Ty> {
        using type = std::string;
    };

    template <typename Ty>
    using format_type_t = typename format_type<Ty>::type;

    bool Analyse(Type** type);
    bool Analyse(Expr** expr, Type* expected_type = nullptr);
    void AnalyseCall(Expr** expr_ptr, CallExpr* expr);
    void AnalyseCast(CastExpr* expr);
    void AnalyseFunction(FuncDecl* decl);
    void AnalyseIntrinsicCall(Expr** expr_ptr, IntrinsicCallExpr* expr);
    void AnalyseModule();
    void AnalyseUnary(UnaryExpr* expr);

    /// Attempt to convert an expression to a given type.
    ///
    /// This may replace the expression with a cast and will issue
    /// and error if the conversion fails for whatever reason.
    ///
    /// Note that the expression to be converted must be marked as
    /// either done or errored by sema. If marked as errored, this
    /// always returns true and does nothing so.
    ///
    /// \param expr A pointer to the expression to convert.
    /// \param type The type to convert to.
    /// \see TryConvert().
    void Convert(Expr** expr, Type* type);

    /// Like Convert(), but converts expressions to their *common type* instead.
    ///
    /// \param exprs The first expressions to convert.
    /// \param conversion_required Whether the conversion is required, i.e. whether
    ///     to error if the conversion fails.
    /// \return Whether the conversion succeeded, i.e. whether there is a common type.
    /// \see Convert()
    bool ConvertToCommonType(std::span<Expr*> exprs, bool conversion_required);

    /// Like ConvertToCommonType(), but for two types.
    ///
    /// \param a The first type.
    /// \param b The second type.
    /// \param conversion_required Whether the conversion is required.
    /// \return Whether the conversion succeeded.
    /// \see ConvertToCommonType()
    bool ConvertToCommonType(Expr** a, Expr** b, bool conversion_required) {
        Expr* exprs[] = {*a, *b};
        auto ok = ConvertToCommonType(exprs, conversion_required);
        *a = exprs[0];
        *b = exprs[1];
        return ok;
    }

    /// Convert a type to a type that is legal in a declaration.
    auto DeclTypeDecay(const Type* type) -> Type*;

    /// Wrapper that stringifies any types that are passed in and passes
    /// everything to \c Diag::Error.
    template <typename... Args>
    Diag Error(Location loc, fmt::format_string<format_type_t<Args>...> fmt, Args&&... args) {
        return Diag::Error(context, loc, fmt, Format<Args>(std::forward<Args>(args))...);
    }

    /// Format a type.
    template <typename Ty>
    requires requires (Ty ty) { cast<Type>(ty); }
    auto Format(Ty ty) -> std::string { return ty->string(use_colours); }

    /// Formatting anything else just passes it through unchanged.
    template <typename Ty>
    requires (not requires (Ty ty) { cast<Type>(ty); })
    auto Format(Ty&& t) -> decltype(std::forward<Ty>(t)) {
        return std::forward<Ty>(t);
    }

    /// If the type of an expression is a pointer type—not a reference
    /// type—convert the expression to \c integer instead by inserting
    /// a cast expression.
    ///
    /// Otherwise, this is a no-op.
    void InsertPointerToIntegerCast(Expr** operand);

    /// Convert lvalues to rvalues and leave rvalues unchanged.
    ///
    /// This inserts an implicit cast expression.
    /// \return The type of the rvalue.
    auto LvalueToRvalue(Expr** expr) -> Type*;

    /// Create a (type-checked) pointer to a type.
    auto Ptr(Type* type) -> PointerType*;

    /// Replace a node with a new node.
    ///
    /// The expression \c expr_ptr points to will be replaced with
    /// \c replacement, and the location of \c replacement will be
    /// set to the location of the original expression.
    void ReplaceWithNewNode(Expr** expr_ptr, Expr* replacement);

    /// Create a (type-checked) reference to a type.
    auto Ref(Type* type) -> ReferenceType*;

    /// Attempt to convert an expression to a given type.
    ///
    /// This is similar to \c Convert(), except that it does not perform
    /// any conversion and that it doesn’t issue a diagnostic on failure.
    ///
    /// Furthermore, this returns a score that may be used for overload
    /// resolution. The score indicates how ‘bad’ the conversion is, i.e. how
    /// badly the overload containing it matches the type of the expression.
    ///
    /// Note that, unlike \c Convert(), this function returns does not succeed
    /// if the expression is marked as errored.
    ///
    /// \param expr A pointer to the expression to convert.
    /// \param type The type to convert to.
    /// \return -2 if the expression is marked as errored.
    /// \return -1 if the conversion fails or is impossible.
    /// \return 0 if the conversion is (logically) a no-op.
    /// \return A number greater than 0 that indicates how ‘bad’ the conversion is.
    /// \see Convert().
    int TryConvert(Expr** expr, Type* type);
};
} // namespace lcc::intercept

#endif // INTERCEPT_SEMA_HH
