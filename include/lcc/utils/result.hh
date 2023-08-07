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
template <typename ValueType>
requires (not std::is_reference_v<ValueType>)
class Result {
    std::variant<ValueType, Diag> data;

public:
    /// Prohibit construction from nullptr.
    Result(std::nullptr_t) = delete;

    /// Create a result that holds a value.
    Result(ValueType value) : data(std::move(value)) {}

    /// Create a result that holds a diagnostic.
    Result(Diag diag) : data(std::move(diag)) {}

    /// Create a result and upcast the value.
    template <std::derived_from<std::remove_pointer_t<ValueType>> T>
    requires std::is_pointer_v<ValueType>
    Result(Result<T*>&& other) {
        if (other.is_diag()) data = std::move(other.diag());
        else data = other.value();
    }

    /// Get the diagnostic.
    [[nodiscard]] auto diag() -> Diag& { return std::get<Diag>(data); }

    /// Check if the result holds a diagnostic.
    [[nodiscard]] bool is_diag() { return std::holds_alternative<Diag>(data); }

    /// Check if the result holds a value.
    [[nodiscard]] bool is_value() { return std::holds_alternative<ValueType>(data); }

    /// Get the value.
    [[nodiscard]] auto value() -> ValueType& { return std::get<ValueType>(data); }

    /// Same as `is_diag()`
    explicit operator bool() { return is_diag(); }
};

} // namespace lcc

#endif // LCC_RESULT_HH
