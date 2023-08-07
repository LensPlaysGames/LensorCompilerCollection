#ifndef INTERCEPT_AST_HH
#define INTERCEPT_AST_HH

#include <lcc/core.hh>
#include <lcc/utils.hh>
#include <span>

#define INTERCEPT_INTRINSICS(X) \
    X(Inline)                   \
    X(Line)                     \
    X(FileNam)

#define INTERCEPT_FUNC_ATTR(X)  \
    X(Discardable, discardable) \
    X(Used, used)

namespace lcc::intercept {
enum struct IntrinsicKind {
#define X(I) I,
    LCC_INTRINSICS(X)
        INTERCEPT_INTRINSICS(X)
#undef X
};

enum struct FuncAttr {
#define X(I, J) I,
    LCC_FUNC_ATTR(X)
        INTERCEPT_FUNC_ATTR(X)
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

class FuncTypeParam {
    std::string _name;
    Type* _type;

public:
    FuncTypeParam(std::string name, Type* type)
        : _name(std::move(name)), _type(type) {}

    const std::string& name() const { return _name; }
    Type* type() const { return _type; }
};

class PrimitiveType : public Type {
    usz _size;
    usz _alignment;
    std::string_view _name;
    bool _is_signed;

public:
    PrimitiveType(usz size, usz alignment, std::string_view name, bool isSigned)
        : Type(TypeKind::Primitive), _size(size), _alignment(alignment), _name(name), _is_signed(isSigned) { }

    usz size() const { return _size; }
    usz alignment() const { return _alignment; }
    std::string_view name() const { return _name; }
    bool is_signed() const { return _is_signed; }

    static bool classof(Type* type) { return type->kind() == TypeKind::Primitive; }
};

class NamedType : public Type {
public:
    NamedType()
        : Type(TypeKind::Primitive) { }

    static bool classof(Type* type) { return type->kind() == TypeKind::Named; }
};

class TypeWithOneElement : public Type {
    Type* _element_type;

protected:
    TypeWithOneElement(TypeKind kind, Type* elementType)
        : Type(kind), _element_type(elementType) { }

public:
    Type* element_type() const { return _element_type; }
};

class PointerType : public TypeWithOneElement {
public:
    PointerType(Type* elementType)
        : TypeWithOneElement(TypeKind::Primitive, elementType) { }

    static bool classof(Type* type) { return type->kind() == TypeKind::Pointer; }
};

class ReferenceType : public TypeWithOneElement {
public:
    ReferenceType(Type* elementType)
        : TypeWithOneElement(TypeKind::Reference, elementType) { }

    static bool classof(Type* type) { return type->kind() == TypeKind::Reference; }
};

class ArrayType : public TypeWithOneElement {
    usz _size;

public:
    ArrayType(Type* elementType, usz size)
        : TypeWithOneElement(TypeKind::Array, elementType), _size(size) { }

    usz size() const { return _size; }

    static bool classof(Type* type) { return type->kind() == TypeKind::Array; }
};

class FuncType : public Type {
    Type* _return_type;
    std::vector<FuncTypeParam*> _params;

#define X(I, J) bool _attr_##J : 1 = false;
    LCC_FUNC_ATTR(X)
    INTERCEPT_FUNC_ATTR(X)
#undef X

public:
    FuncType(Type* returnType, std::vector<FuncTypeParam*> params)
        : Type(TypeKind::Function), _return_type(returnType), _params(std::move(params)) {}

    Type* return_type() const { return _return_type; }
    std::span<FuncTypeParam* const> params() const { return _params; }

#define X(I, J)                               \
    bool is_##J() const { return _attr_##J; } \
    void set_##J(bool value) { _attr_##J = value; }
    LCC_FUNC_ATTR(X)
    INTERCEPT_FUNC_ATTR(X)
#undef X

    static bool classof(Type* type) { return type->kind() == TypeKind::Function; }
};

class StructDecl;

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

class StructType : public Type {
    StructDecl* _structDecl;
    std::vector<StructMember*> _members;

    usz _byte_size;
    usz _alignment;

public:
    StructType(StructDecl* structDecl, std::vector<StructMember*> members)
        : Type(TypeKind::Struct), _structDecl(structDecl), _members(std::move(members)) { }

    StructDecl* struct_decl() const { return _structDecl; }
    std::span<StructMember* const> members() { return _members; }

    usz byte_size() const { return _byte_size; }
    void byte_size(usz byteSize) { _byte_size = byteSize; }

    usz alignment() const { return _alignment; }
    void alignment(usz alignment) { _alignment = alignment; }

    static bool classof(Type* type) { return type->kind() == TypeKind::Struct; }
};

class IntegerType : public Type {
    bool _is_signed;
    usz _bit_width;

public:
    IntegerType(bool isSigned, usz bitWidth)
        : Type(TypeKind::Integer), _is_signed(isSigned), _bit_width(bitWidth) { }

    bool is_signed() const { return _is_signed; }
    usz bit_width() const { return _bit_width; }

    static bool classof(Type* type) { return type->kind() == TypeKind::Integer; }
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
