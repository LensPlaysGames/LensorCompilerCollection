#ifndef LCC_EVAL_HH
#define LCC_EVAL_HH

#include <lcc/utils.hh>

namespace lcc::intercept {
class Scope;
class Expr;
class Decl;
class FuncDecl;
class Type;
class ObjectDecl;
class Parser;
class StringLiteral;

class EvalResult {
    std::variant<i64, bool, std::nullptr_t, StringLiteral*, std::monostate> data;
public:
    EvalResult() : data(std::monostate()) {}
    EvalResult(i64 data) : data(data) {}
    EvalResult(bool data) : data(data) {}
    EvalResult(std::nullptr_t) : data(nullptr) {}
    EvalResult(StringLiteral* data) : data(data) {}

    bool is_i64() const { return std::holds_alternative<i64>(data); }
    bool is_bool() const { return std::holds_alternative<bool>(data); }
    bool is_null() const { return std::holds_alternative<std::nullptr_t>(data); }
    bool is_string() const { return std::holds_alternative<StringLiteral*>(data); }

    i64 as_i64() const { return std::get<i64>(data); }
    bool as_bool() const { return std::get<bool>(data); }
    StringLiteral* as_string() const { return std::get<StringLiteral*>(data); }
};

}

#endif // LCC_EVAL_HH
