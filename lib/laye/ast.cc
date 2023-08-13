#include <laye/ast.hh>
#include <laye/parser.hh>
#include <lcc/utils/rtti.hh>

void* lcc::laye::Scope::operator new(size_t sz, Parser& parser) {
    LCC_ASSERT(not parser.IsInSpeculativeParse(), "Should never be allocating syntax scopes while in speculative parse mode.");
    auto ptr = ::operator new(sz);
    parser.module->scopes.push_back(static_cast<Scope*>(ptr));
    return ptr;
}

void* lcc::laye::Statement::operator new(size_t sz, Parser& parser) {
    LCC_ASSERT(not parser.IsInSpeculativeParse(), "Should never be allocating syntax nodes while in speculative parse mode.");
    auto ptr = ::operator new(sz);
    parser.module->statements.push_back(static_cast<Statement*>(ptr));
    return ptr;
}

void* lcc::laye::Expr::operator new(size_t sz, Parser& parser) {
    LCC_ASSERT(not parser.IsInSpeculativeParse(), "Should never be allocating syntax nodes while in speculative parse mode.");
    auto ptr = ::operator new(sz);
    parser.module->exprs.push_back(static_cast<Expr*>(ptr));
    return ptr;
}

auto lcc::laye::Scope::declare(
    Parser* parser,
    std::string&& name,
    Statement* expr
) -> Result<Statement*> {
    /// Try to insert the symbol into the map.
    auto [it, inserted] = _symbols.insert_or_assign(name, expr);
    if (inserted) return expr;

    /// If the symbol already exists, and it is a function
    /// declaration or overload set, and the new symbol is
    /// also a function declaration, merge the two into one
    /// overload set.
    if (is<FunctionDecl>(expr) and is<FunctionDecl, OverloadSet>(it->second)) {
        if (not is<OverloadSet>(it->second)) {
            auto func = as<FunctionDecl>(it->second);
            auto os = new (*parser) OverloadSet(func->location());
            os->add(func);
            it->second = os;
        }

        auto os = as<OverloadSet>(it->second);
        os->add(as<FunctionDecl>(expr));
        return expr;
    }

    /// Any other case is an error.
    return Diag::Error(parser->context, expr->location(), "Redeclaration of '{}'", name);
}
