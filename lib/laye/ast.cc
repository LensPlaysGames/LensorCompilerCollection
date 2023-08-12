#include <laye/ast.hh>
#include <laye/parser.hh>

void* lcc::laye::Scope::operator new(size_t sz, Parser& parser) {
    LCC_ASSERT(not parser.IsInSpeculativeParse(),
        "Should never be allocating syntax scopes while in speculative parse mode.");
    auto ptr = ::operator new(sz);
    parser.module->scopes.push_back(static_cast<Scope*>(ptr));
    return ptr;
}

void* lcc::laye::Statement::operator new(size_t sz, Parser& parser) {
    LCC_ASSERT(not parser.IsInSpeculativeParse(),
        "Should never be allocating syntax nodes while in speculative parse mode.");
    auto ptr = ::operator new(sz);
    parser.module->statements.push_back(static_cast<Statement*>(ptr));
    return ptr;
}

void* lcc::laye::Expr::operator new(size_t sz, Parser& parser) {
    LCC_ASSERT(not parser.IsInSpeculativeParse(),
        "Should never be allocating syntax nodes while in speculative parse mode.");
    auto ptr = ::operator new(sz);
    parser.module->exprs.push_back(static_cast<Expr*>(ptr));
    return ptr;
}
