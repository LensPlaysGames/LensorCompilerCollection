#ifndef GLINT_EVAL_HH
#define GLINT_EVAL_HH

#include <lcc/utils.hh>
#include <lcc/utils/aint.hh>

#include <concepts>
#include <cstddef>
#include <type_traits>
#include <variant>

namespace lcc::glint {
class Scope;
class Expr;
class Decl;
class FuncDecl;
class Type;
class ObjectDecl;
class Parser;
class StringLiteral;

class EvalResult {
    std::variant< // clang-format off
        aint,
        std::nullptr_t,
        StringLiteral*,
        std::monostate
    > data; // clang-format on
public:
    EvalResult() : data(std::monostate()) {}
    EvalResult(std::nullptr_t) : data(nullptr) {}
    EvalResult(StringLiteral* data) : data(data) {}
    EvalResult(aint data) : data(data) {}
    EvalResult(std::same_as<bool> auto data) : EvalResult(aint(1)) {}

    /// Requires rather annoying explicit disabmiguation due to subsumption rules
    EvalResult(std::integral auto data)
    requires (not std::is_same_v<std::remove_cvref_t<decltype(data)>, bool>)
        : EvalResult(aint(data)) {}

    bool is_int() const { return std::holds_alternative<aint>(data); }
    bool is_null() const { return std::holds_alternative<std::nullptr_t>(data); }
    bool is_string() const { return std::holds_alternative<StringLiteral*>(data); }

    aint as_int() const { return std::get<aint>(data); }
    StringLiteral* as_string() const { return std::get<StringLiteral*>(data); }
};

} // namespace lcc::glint

#endif // GLINT_EVAL_HH
