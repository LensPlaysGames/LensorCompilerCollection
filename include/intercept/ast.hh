#ifndef INTERCEPT_AST_HH
#define INTERCEPT_AST_HH

#include <lcc/core.hh>
#include <lcc/utils.hh>
#include <span>

#define INTERCEPT_INTRINSICS(X) \
    X(Inline)                   \
    X(Line)                     \
    X(FileNam)

namespace lcc::intercept {
enum struct IntrinsicKind {
#define X(I) I,
    LCC_INTRINSICS(X)
        INTERCEPT_INTRINSICS(X)
#undef X
};

class Symbol;
class Expr;
class Type;

class Scope {
    Scope* _parent;
    std::vector<Symbol*> _symbols;
    std::vector<Scope*> _children;

public:
    Scope(Scope* parent = NULL) : _parent(parent) {}

    Scope* parent() const { return _parent; }
};

class Symbol {
public:
    enum struct SymbolKind {
        Variable,
        Function,
        Type,
    };

private:
    const SymbolKind _kind;

    std::string _name;
    Scope* _scope;

protected:
    Symbol(SymbolKind kind, std::string name, Scope* scope) : _kind(kind), _name(std::move(name)), _scope(scope) {}

public:
    SymbolKind kind() const { return _kind; }

    const std::string& name() const { return _name; }
    Scope* scope() const { return _scope; }
};

class VariableSymbol : public Symbol {
    Expr* _value;

public:
    VariableSymbol(std::string name, Scope* scope, Expr* value) : Symbol(SymbolKind::Variable, name, scope), _value(value) {}

    Expr* value() const { return _value; }

    static bool classof(Symbol* symbol) { return symbol->kind() == SymbolKind::Variable; }
};

class FunctionSymbol : public Symbol {
    Expr* _value;

public:
    FunctionSymbol(std::string name, Scope* scope, Expr* value) : Symbol(SymbolKind::Variable, name, scope), _value(value) {}

    Expr* value() const { return _value; }

    static bool classof(Symbol* symbol) { return symbol->kind() == SymbolKind::Function; }
};

class TypeSymbol : public Symbol {
    Type* _type;

public:
    TypeSymbol(std::string name, Scope* scope, Type* type) : Symbol(SymbolKind::Variable, name, scope), _type(type) {}

    Type* type() const { return _type; }

    static bool classof(Symbol* symbol) { return symbol->kind() == SymbolKind::Type; }
};

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

    static Type* UnknownType;
    static Type* IntegerLiteralType;
    static Type* FloatLiteralType;
    static Type* BoolType;
};

/// @brief Base class for expression syntax nodes.
class Expr {
public:
    enum struct ExprKind {
        Function,
        Struct,
        Decl,

        IntegerLiteral,
        FloatLiteral,
        StringLiteral,
        CompoundLiteral,

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

        Reference,
        MemberAccess,
    };

private:
    const ExprKind _kind;

protected:
    Expr(ExprKind kind) : _kind(kind) {}

public:
    virtual ~Expr() = default;

    void* operator new(size_t) = delete;

    Type* type() const;

    ExprKind kind() const { return _kind; }
};

class TypedExpr : public Expr {
    Type* _type;

protected:
    TypedExpr(ExprKind kind, Type* type = Type::UnknownType)
        : Expr(kind), _type(type) {}

public:
    void type(Type* type) { _type = type; }
};

class Decl : public TypedExpr {
    Linkage _linkage;
    std::string _name;
    Expr* _init;

public:
    Decl(Linkage linkage, Type* type, std::string name, Expr* init)
        : TypedExpr(ExprKind::Decl, type), _linkage(linkage), _name(std::move(name)), _init(init) {}

    Linkage linkage() const { return _linkage; }
    std::string name() const { return _name; }
    Expr* init() const { return _init; }

    static bool classof(Expr* expr) { return expr->kind() == ExprKind::Decl; }
};

class FuncDecl : public TypedExpr {
    Linkage _linkage;
    std::string _name;
    std::vector<Decl*> _params;
    Expr* _body;

public:
    FuncDecl(Linkage linkage, Type* type, std::string name, std::vector<Decl*> params, Expr* body)
        : TypedExpr(ExprKind::Function, type), _linkage(linkage), _name(std::move(name)), _params(std::move(params)), _body(body) {}

    Linkage linkage() const { return _linkage; }
    const std::string& name() const { return _name; }
    std::span<Decl* const> params() const { return _params; }
    Expr* body() const { return _body; }

    static bool classof(Expr* expr) { return expr->kind() == ExprKind::Function; }
};

class StructDecl : public TypedExpr {
    Symbol* _symbol;

public:
    StructDecl(Symbol* symbol) : TypedExpr(ExprKind::Struct), _symbol(symbol) {}

    Symbol* symbol() const { return _symbol; }

    static bool classof(Expr* expr) { return expr->kind() == ExprKind::Struct; }
};

class IntegerLiteral : public TypedExpr {
    u64 _value;

public:
    IntegerLiteral(u64 value, Type* type = Type::IntegerLiteralType)
        : TypedExpr(ExprKind::IntegerLiteral, type), _value(value) {}

    u64 value() const { return _value; }

    static bool classof(Expr* expr) { return expr->kind() == ExprKind::IntegerLiteral; }
};

class FloatLiteral : public TypedExpr {
    double _value;

public:
    FloatLiteral(double value)
        : TypedExpr(ExprKind::FloatLiteral, Type::FloatLiteralType), _value(value) {}

    double value() const { return _value; }

    static bool classof(Expr* expr) { return expr->kind() == ExprKind::FloatLiteral; }
};

class StringLiteral : public TypedExpr {
    usz _index;

public:
    StringLiteral(usz index, Type* type)
        : TypedExpr(ExprKind::StringLiteral, type), _index(index) {}

    usz string_index() const { return _index; }

    static bool classof(Expr* expr) { return expr->kind() == ExprKind::StringLiteral; }
};

class CompoundLiteral : public TypedExpr {
    std::vector<Expr*> _values;

public:
    CompoundLiteral(std::vector<Expr*> values)
        : TypedExpr(ExprKind::CompoundLiteral), _values(std::move(values)) {}

    std::span<Expr* const> values() const { return _values; }

    static bool classof(Expr* expr) { return expr->kind() == ExprKind::CompoundLiteral; }
};

class IfExpr : public TypedExpr {
    Expr* _condition;
    Expr* _then;
    Expr* _else;

public:
    IfExpr(Expr* condition, Expr* then, Expr* else_)
        : TypedExpr(ExprKind::If), _condition(condition), _then(then), _else(else_) {}

    Expr* condition() const { return _condition; }
    Expr* then() const { return _then; }
    Expr* else_() const { return _else; }

    static bool classof(Expr* expr) { return expr->kind() == ExprKind::If; }
};

class WhileExpr : public Expr {
    Expr* _condition;
    Expr* _body;

public:
    WhileExpr(Expr* condition, Expr* body)
        : Expr(ExprKind::While), _condition(condition), _body(body) {}

    Expr* condition() const { return _condition; }
    Expr* body() const { return _body; }

    static bool classof(Expr* expr) { return expr->kind() == ExprKind::While; }
};

class ForExpr : public Expr {
    Expr* _init;
    Expr* _condition;
    Expr* _iterator;
    Expr* _body;

public:
    ForExpr(Expr* init, Expr* condition, Expr* iterator, Expr* body)
        : Expr(ExprKind::For), _init(init), _condition(condition), _iterator(iterator), _body(body) {}

    Expr* init() const { return _init; }
    Expr* condition() const { return _condition; }
    Expr* iterator() const { return _iterator; }
    Expr* body() const { return _body; }

    static bool classof(Expr* expr) { return expr->kind() == ExprKind::For; }
};

class BlockExpr : public TypedExpr {
    std::vector<Expr*> _children;

public:
    BlockExpr(std::vector<Expr*> children)
        : TypedExpr(ExprKind::Block), _children(std::move(children)) {}

    std::span<Expr* const> children() const { return _children; }

    static bool classof(Expr* expr) { return expr->kind() == ExprKind::Block; }
};

class ReturnExpr : public Expr {
    Expr* _value;

public:
    ReturnExpr(Expr* value)
        : Expr(ExprKind::Return), _value(value) {}

    Expr* value() const { return _value; }

    static bool classof(Expr* expr) { return expr->kind() == ExprKind::Return; }
};

class CallExpr : public TypedExpr {
    Expr* _callee;
    std::vector<Expr*> _args;

public:
    CallExpr(Expr* callee, std::vector<Expr*> args)
        : TypedExpr(ExprKind::Call), _callee(callee), _args(std::move(args)) {}

    Expr* callee() const { return _callee; }
    std::span<Expr* const> args() { return _args; }

    static bool classof(Expr* expr) { return expr->kind() == ExprKind::Call; }
};

class IntrinsicCallExpr : public TypedExpr {
    IntrinsicKind _kind;
    std::vector<Expr*> _args;

public:
    IntrinsicCallExpr(IntrinsicKind kind, std::vector<Expr*> args)
        : TypedExpr(ExprKind::IntrinsicCall), _kind(kind), _args(std::move(args)) {}

    IntrinsicKind intrinsic_kind() const { return _kind; }
    std::span<Expr* const> args() { return _args; }

    static bool classof(Expr* expr) { return expr->kind() == ExprKind::IntrinsicCall; }
};

class CastExpr : public TypedExpr {
public:
    CastExpr()
        : TypedExpr(ExprKind::Cast) {}

    static bool classof(Expr* expr) { return expr->kind() == ExprKind::Cast; }
};

class UnaryExpr : public TypedExpr {
public:
    UnaryExpr()
        : TypedExpr(ExprKind::Unary) {}

    static bool classof(Expr* expr) { return expr->kind() == ExprKind::Unary; }
};

class BinaryExpr : public TypedExpr {
public:
    BinaryExpr()
        : TypedExpr(ExprKind::Binary) {}

    static bool classof(Expr* expr) { return expr->kind() == ExprKind::Binary; }
};

class RefExpr : public TypedExpr {
    std::string _name;
    Expr* _target;

public:
    RefExpr(std::string name)
        : TypedExpr(ExprKind::Reference), _name(std::move(name)) {}

    const std::string& name() const { return _name; }
    Expr* target() const { return _target; }
    void target(Expr* target) { _target = target; }

    static bool classof(Expr* expr) { return expr->kind() == ExprKind::Reference; }
};

class StructMember {
    Type* _type;
    std::string _name;
    usz _byte_offset;

public:
    StructMember(Type* type, std::string name, usz byteOffset) : _type(type), _name(std::move(name)), _byte_offset(byteOffset) {}

    Type* type() const { return _type; }
    const std::string& name() const { return _name; }
    usz byte_offset() const { return _byte_offset; }
};

class MemberAccessExpr : public TypedExpr {
    Expr* _target;
    std::string _name;
    StructMember* _member;

public:
    MemberAccessExpr(Expr* target, std::string name)
        : TypedExpr(ExprKind::MemberAccess), _target(target), _name(std::move(name)) {}

    Expr* target() const { return _target; }
    const std::string& name() const { return _name; }
    StructMember* member() const { return _member; }
    void member(StructMember* member) { _member = member; }

    static bool classof(Expr* expr) { return expr->kind() == ExprKind::MemberAccess; }
};
} // namespace lcc::intercept

#endif // INTERCEPT_AST_HH
