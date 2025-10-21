#include <glint/ast.hh>

#include <lcc/utils.hh>

#include <string_view>
#include <unordered_map>

namespace lcc::glint {

class ASTEvaluator {
public:
    struct FunctionContext {
        FunctionContext* caller{nullptr};
        FuncType* current{nullptr};
    };

    struct EvalScope {
        EvalScope* parent{nullptr};
        // The type of the value of the bound symbol.
        std::unordered_map<std::string_view, Type*> types{};
        // The value of the bound symbol
        std::unordered_map<std::string_view, Expr*> values{};

        void declare(std::string_view name, Type* type, Expr* initial_value) {
            LCC_ASSERT(type);
            LCC_ASSERT(initial_value);
            types.emplace(name, type);
            values.emplace(name, initial_value);
        }

        Expr* get_value(std::string_view name) {
            if (not values.contains(name))
                return nullptr;
            return values.at(name);
        }

        Expr* get_value_recursive(std::string_view name) {
            auto v = get_value(name);
            // If we find the value in the current scope, return it.
            if (v)
                return v;
            // If we didn't find the value in the current scope, and we have a parent
            // scope, then look in that.
            if (parent)
                return parent->get_value_recursive(name);
            // Otherwise, we are at the global scope and have not found a value bound
            // to name...
            return nullptr;
        }
    };

    struct EvalContext {
        FunctionContext& context;
        EvalScope& current_scope;

        EvalContext(FunctionContext& f, EvalScope& s)
            : context{f}, current_scope{s} {}
    };

private:
    // The module that the AST was parsed into.
    Module& mod;
    // Root level node that we are evaluating
    Expr* root{};
    Expr* result{};
    EvalContext context;

    Expr* eval_expr(Expr* expr);

public:
    ASTEvaluator(Module& m, Expr* expr)
        : mod(m),
          root(expr),
          context(
              *(new FunctionContext()),
              *(new EvalScope())
          ) {}

    Expr* eval();
};

auto evaluate(Module& mod, Expr* expr) -> Expr*;

} // namespace lcc::glint
