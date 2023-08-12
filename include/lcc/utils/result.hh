#ifndef LCC_RESULT_HH
#define LCC_RESULT_HH

#include <lcc/diags.hh>
#include <lcc/utils.hh>
#include <lcc/utils/rtti.hh>
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
    struct make_result {
        using type = Result<T>;
        static constexpr bool value = false;
    };

    template <typename T>
    struct make_result<Result<T>> {
        using type = Result<T>;
        static constexpr bool value = true;
    };

    template <typename T>
    using make_result_t = typename make_result<T>::type;

    template <typename T>
    static constexpr bool is_result_v = make_result<T>::value;

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

    /// Checked cast for results.
    ///
    /// Note: \c is() and \c cast() seem to have confusing semantics in
    /// conjunction with results, so we only provide this one for now:
    /// it’s unclear what \c is() should return if the result holds a
    /// diagnostic, and \c cast() is just a bad idea because it would
    /// just fill our results with null pointers, which is something
    /// we’re actively trying to prevent.
    template <typename To, typename From>
    requires is_result_v<From>
    friend auto as(From&& result) -> Result<To*> {
        if (result.is_diag()) return std::forward<decltype(result)>(result).diag();
        else return Result<To*>{lcc::as<To>(std::forward<decltype(result)>(result).value())};
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
    /// but you can use the `LCC_BIND` macro for that. See also the definition of
    /// that macro for more information.
    ///
    /// \param cb A callable that takes in a \c ValueType& and returns a \c Result.
    /// \return A diagnostic if this holds a diagnostic, and the result of invoking
    ///     \c cb with the value otherwise.
    template <typename Callable>
    [[nodiscard]] auto operator>>=(Callable&& cb) -> make_result_t<std::invoke_result_t<Callable, ValueType&>> {
        using ResultType = make_result_t<std::invoke_result_t<Callable, ValueType&>>;
        if (is_diag()) return diag();
        return ResultType{std::invoke(std::forward<Callable>(cb), value())};
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

/// Macro for binding a member function to the \c this pointer.
///
/// See the documentation for \c Result::operator>>= for more information.
///
/// It is recommended to put `#define bind LCC_BIND` at the top of any
/// implementation files to reduce visual clutter. However, do NOT put that
/// in header files because `bind` is too common of a word and could easily
/// break something.
///
/// Cute trick for monad binding.
#define LCC_BIND *this->*&
template <typename ClassType>
requires std::is_class_v<ClassType>
auto operator->*(ClassType& p, auto member_function) {
    return [p = std::addressof(p), member_function](auto&&... args) {
        return std::invoke(member_function, p, std::forward<decltype(args)>(args)...);
    };
}

#endif // LCC_RESULT_HH
