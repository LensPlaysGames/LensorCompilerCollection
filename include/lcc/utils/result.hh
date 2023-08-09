#ifndef LCC_RESULT_HH
#define LCC_RESULT_HH

#include <lcc/diags.hh>
#include <lcc/utils.hh>
#include <variant>

namespace lcc {

/// Result type that can hold either a value or a diagnostic.
///
/// If the result contains a diagnostic, the diagnostic is
/// issued in the destructor, as it usually would be.
template <typename Type, bool allow_construction_from_nullptr = not std::is_pointer_v<Type>>
requires (not std::is_reference_v<Type>)
class [[nodiscard]] Result {
    using ValueType = std::conditional_t<std::is_void_v<Type>, std::monostate, Type>;
    std::variant<ValueType, Diag> data;

    template <typename T>
    struct make_result { using type = Result<T>; };

    template <typename T>
    struct make_result<Result<T>> { using type = Result<T>; };

    template <typename T>
    using make_result_t = typename make_result<T>::type;

    /// Construct a null result.
    struct NullConstructTag {};
    Result(NullConstructTag)
    requires std::is_pointer_v<ValueType>
        : data(nullptr) {}

public:
    Result()
    requires (std::is_default_constructible_v<ValueType> and allow_construction_from_nullptr)
        : data(ValueType{}) {}

    /// Prohibit construction from nullptr unless explicitly allowed.
    Result(std::nullptr_t)
    requires (not allow_construction_from_nullptr)
    = delete;

    /// Create a result that holds a value.
    Result(ValueType value)
    requires (not std::is_void_v<Type>)
        : data(std::move(value)) {}

    /// Create a result that holds a diagnostic.
    Result(Diag diag) : data(std::move(diag)) {}

    /// Create a result from another result.
    template <typename T>
    requires (not std::is_void_v<Type> and std::convertible_to<T, ValueType>)
    Result(Result<T>&& other) {
        if (other.is_diag()) data = std::move(other.diag());
        else data = std::move(other.value());
    }

    /// Get the diagnostic.
    ///
    /// This returns a && to simplify the `return res.diag()` pattern.
    [[nodiscard]] auto diag() -> Diag&& { return std::move(std::get<Diag>(data)); }

    /// Check if the result holds a diagnostic.
    [[nodiscard]] bool is_diag() { return std::holds_alternative<Diag>(data); }

    /// Check if the result holds a value.
    [[nodiscard]] bool is_value() { return std::holds_alternative<ValueType>(data); }

    /// Get the value.
    [[nodiscard]] auto value() -> ValueType&
    requires (not std::is_void_v<Type>)
    { return std::get<ValueType>(data); }

    /// Check if this has a value.
    explicit operator bool() { return is_value(); }

    /// Access the underlying value.
    [[nodiscard]] auto operator*() -> ValueType& { return value(); }

    /// Access the underlying value.
    [[nodiscard]] auto operator->() -> ValueType
    requires std::is_pointer_v<ValueType>
    { return value(); }

    /// \brief Monad bind operator for results.
    ///
    /// If you know Haskell, then you already know what this does. If
    /// you know JavaScript, then this is like the \c .then() method
    /// on \c Promise. If you don’t know either, what this does is,
    /// if this holds a diagnostic, it just returns that diagnostic;
    /// otherwise, it passes the value to \c cb and returns the result
    /// of that call.
    ///
    /// You can use this to avoid writing code like this
    /// \code{.cpp}
    ///     auto expr = ParseExpr();
    ///     if (not expr) return expr.diag();
    ///
    ///     auto foo = Bar(expr.value());
    ///     if (not foo) return foo.diag();
    ///
    ///     return Baz(foo.value());
    /// \endcode
    ///
    /// and turn it into this instead
    /// \code{.cpp}
    ///     return ParseExpr() >>= Bar >>= Baz;
    /// \endcode
    ///
    /// Note that if e.g. \c ParseExpr in this example is a member function, then
    /// this won’t work because you need to bind it to the \c this pointer first,
    /// but you can check \c <intercept/parser.cc> for an example of how to get
    /// around that with a macro. Just search for \c >>= \c bind in that file.
    ///
    /// \param cb A callable that takes in a \c ValueType& and returns a \c Result.
    /// \return A diagnostic if this holds a diagnostic, and the result of invoking
    ///     \c cb with the value otherwise.
    template <typename Callable>
    [[nodiscard]] auto operator>>=(Callable&& cb) -> make_result_t<std::invoke_result_t<Callable, ValueType&>> {
        if (is_diag()) return diag();
        return std::invoke(std::forward<Callable>(cb), value());
    }

    /// Create an empty pointer result.
    ///
    /// This is an unsafe operation as there aren’t ever supposed
    /// to be empty results. Be careful when using this.
    static Result Null()
    requires std::is_pointer_v<ValueType>
    { return Result(NullConstructTag{}); }
};

/// Return true if any of the provided results are errors.
template <typename... ValueTypes>
bool IsError(Result<ValueTypes>&... results) {
    return (results.is_diag() or ...);
}

} // namespace lcc

#endif // LCC_RESULT_HH
