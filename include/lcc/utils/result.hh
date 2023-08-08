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
    [[nodiscard]] auto operator->() -> ValueType
    requires std::is_pointer_v<ValueType>
    { return value(); }

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
