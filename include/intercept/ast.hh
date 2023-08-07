#ifndef INTERCEPT_AST_HH
#define INTERCEPT_AST_HH

#include <lcc/core.hh>
#include <lcc/utils.hh>

namespace lcc::intercept {
class Type {
public:
    enum struct TypeKind {
        Primitive,
        Named,
        Pointer,
        Reference,
        Array,
        Function,
        Struct,
        Integer,
    };

private:
    const TypeKind _kind;

protected:
    Type(TypeKind kind) : _kind(kind) {}

public:
    virtual ~Type() = default;

    void* operator new(size_t) = delete;

    TypeKind kind() const { return _kind; }
};

/// @brief Base class for expression syntax nodes.
class Expr {
public:
    enum struct ExprKind {
        Function,
        Decl,

        Literal,

        If,
        While,
        For,

        Block,
        Return,

        Call,
        IntrinsicCall,
        Cast,
        Unary,
        Binary,

        FunctionReference,
        ModuleReference,
        VariableReference,
        MemberAccess,
    };

private:
    const ExprKind _kind;

protected:
    Expr(ExprKind kind) : _kind(kind) {}

public:
    virtual ~Expr() = default;

    void* operator new(size_t) = delete;

    ExprKind kind() const { return _kind; }
};

class Decl : public Expr {
    Linkage _linkage;
    std::string _name;
    Expr* _init;

public:
    Decl(Linkage linkage, std::string name, Expr* init)
        : Expr(ExprKind::Decl), _linkage(linkage), _name(std::move(name)), _init(init) {}

    Linkage linkage() const { return _linkage; }
    std::string name() const { return _name; }
    Expr* init() const { return _init; }

    static bool classof(Expr* expr) { return expr->kind() == ExprKind::Decl; }
};

class Function : public Expr {
    Linkage _linkage;
    std::string _name;
    std::vector<Decl*> _params;
    Expr* _body;

public:
    Function(Linkage linkage, std::string name, std::vector<Decl*> params, Expr* body)
        : Expr(ExprKind::Function), _linkage(linkage), _name(std::move(name)),
          _params(std::move(params)), _body(body) {}

    Linkage linkage() const { return _linkage; }
    std::string name() const { return _name; }
    std::vector<Decl*> params() const { return _params; }
    Expr* body() const { return _body; }

    static bool classof(Expr* expr) { return expr->kind() == ExprKind::Function; }
};

class Literal : public Expr {
};
} // namespace lcc::intercept

#endif // INTERCEPT_AST_HH
