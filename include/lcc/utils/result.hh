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

public:
    Result()
    requires std::is_default_constructible_v<ValueType>
        : data(ValueType{}) {}

    /// Prohibit construction from nullptr unless explicitly allowed.
    Result(std::nullptr_t) requires (not allow_construction_from_nullptr) = delete;

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
    [[nodiscard]] auto diag() -> Diag& { return std::get<Diag>(data); }

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
};

} // namespace lcc

#endif // LCC_RESULT_HH
