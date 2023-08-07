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

// clang-format off
// the macros in here get formatted weird with it on
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
// clang-format on

class Symbol;
class Expr;
class Type;

class Scope {
    Scope* _parent;
    std::vector<Symbol*> _symbols;
    std::vector<Scope*> _children;

public:
    Scope(Scope* parent = NULL)
        : _parent(parent) {}

    auto parent() const { return _parent; }
};

class Symbol {
public:
    enum struct Kind {
        Variable,
        Function,
        Type,
    };

private:
    const Kind _kind;

    std::string _name;
    Scope* _scope;

protected:
    Symbol(Kind kind, std::string name, Scope* scope)
        : _kind(kind), _name(std::move(name)), _scope(scope) {}

public:
    Kind kind() const { return _kind; }

    auto name() const -> const std::string& { return _name; }
    auto scope() const { return _scope; }
};

class VariableSymbol : public Symbol {
    Expr* _value;

public:
    VariableSymbol(std::string name, Scope* scope, Expr* value)
        : Symbol(Kind::Variable, name, scope), _value(value) {}

    auto value() const { return _value; }

    static bool classof(Symbol* symbol) { return symbol->kind() == Kind::Variable; }
};

class FunctionSymbol : public Symbol {
    Expr* _value;

public:
    FunctionSymbol(std::string name, Scope* scope, Expr* value)
        : Symbol(Kind::Variable, name, scope), _value(value) {}

    auto value() const { return _value; }

    static bool classof(Symbol* symbol) { return symbol->kind() == Kind::Function; }
};

class TypeSymbol : public Symbol {
    Type* _type;

public:
    TypeSymbol(std::string name, Scope* scope, Type* type)
        : Symbol(Kind::Variable, name, scope), _type(type) {}

    auto type() const { return _type; }

    static bool classof(Symbol* symbol) { return symbol->kind() == Kind::Type; }
};

class Type {
public:
    enum struct Kind {
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
    const Kind _kind;

protected:
    Type(Kind kind)
        : _kind(kind) {}

public:
    virtual ~Type() = default;

    void* operator new(size_t) = delete;

    auto kind() const { return _kind; }

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
    auto type() const { return _type; }
};

class PrimitiveType : public Type {
    usz _size;
    usz _alignment;
    std::string_view _name;
    bool _is_signed;

public:
    PrimitiveType(usz size, usz alignment, std::string_view name, bool isSigned)
        : Type(Kind::Primitive), _size(size), _alignment(alignment), _name(name), _is_signed(isSigned) {}

    usz  size() const { return _size; }
    usz  alignment() const { return _alignment; }
    auto name() -> std::string_view const { return _name; }
    bool is_signed() const { return _is_signed; }

    static bool classof(Type* type) { return type->kind() == Kind::Primitive; }
};

class NamedType : public Type {
public:
    NamedType()
        : Type(Kind::Primitive) {}

    static bool classof(Type* type) { return type->kind() == Kind::Named; }
};

class TypeWithOneElement : public Type {
    Type* _element_type;

protected:
    TypeWithOneElement(Kind kind, Type* elementType)
        : Type(kind), _element_type(elementType) {}

public:
    auto element_type() const { return _element_type; }
};

class PointerType : public TypeWithOneElement {
public:
    PointerType(Type* elementType)
        : TypeWithOneElement(Kind::Primitive, elementType) {}

    static bool classof(Type* type) { return type->kind() == Kind::Pointer; }
};

class ReferenceType : public TypeWithOneElement {
public:
    ReferenceType(Type* elementType)
        : TypeWithOneElement(Kind::Reference, elementType) {}

    static bool classof(Type* type) { return type->kind() == Kind::Reference; }
};

class ArrayType : public TypeWithOneElement {
    usz _size;

public:
    ArrayType(Type* elementType, usz size)
        : TypeWithOneElement(Kind::Array, elementType), _size(size) {}

    usz size() const { return _size; }

    static bool classof(Type* type) { return type->kind() == Kind::Array; }
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
        : Type(Kind::Function), _return_type(returnType), _params(std::move(params)) {}

    auto return_type() const { return _return_type; }
    auto params() -> std::span<FuncTypeParam* const> const { return _params; }

#define X(I, J)                               \
    bool is_##J() const { return _attr_##J; } \
    void set_##J(bool value) { _attr_##J = value; }
    LCC_FUNC_ATTR(X)
    INTERCEPT_FUNC_ATTR(X)
#undef X

    static bool classof(Type* type) { return type->kind() == Kind::Function; }
};

class StructDecl;

class StructMember {
    Type* _type;
    std::string _name;
    usz _byte_offset;

public:
    StructMember(Type* type, std::string name, usz byteOffset) : _type(type), _name(std::move(name)), _byte_offset(byteOffset) {}

    auto type() const { return _type; }
    auto name() const -> const std::string& { return _name; }
    usz  byte_offset() const { return _byte_offset; }
};

class StructType : public Type {
    StructDecl* _structDecl;
    std::vector<StructMember*> _members;

    usz _byte_size;
    usz _alignment;

public:
    StructType(StructDecl* structDecl, std::vector<StructMember*> members)
        : Type(Kind::Struct), _structDecl(structDecl), _members(std::move(members)) {}

    StructDecl* struct_decl() const { return _structDecl; }
    std::span<StructMember* const> members() { return _members; }

    usz  byte_size() const { return _byte_size; }
    void byte_size(usz byteSize) { _byte_size = byteSize; }

    usz  alignment() const { return _alignment; }
    void alignment(usz alignment) { _alignment = alignment; }

    static bool classof(Type* type) { return type->kind() == Kind::Struct; }
};

class IntegerType : public Type {
    bool _is_signed;
    usz _bit_width;

public:
    IntegerType(bool isSigned, usz bitWidth)
        : Type(Kind::Integer), _is_signed(isSigned), _bit_width(bitWidth) {}

    bool is_signed() const { return _is_signed; }
    usz  bit_width() const { return _bit_width; }

    static bool classof(Type* type) { return type->kind() == Kind::Integer; }
};

/// @brief Base class for expression syntax nodes.
class Expr {
public:
    enum struct Kind {
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
    const Kind _kind;

protected:
    Expr(Kind kind)
        : _kind(kind) {}

public:
    virtual ~Expr() = default;

    void* operator new(size_t) = delete;

    auto type() const -> Type*;

    Kind kind() const { return _kind; }
};

class TypedExpr : public Expr {
    Type* _type;

protected:
    TypedExpr(Kind kind, Type* type = Type::UnknownType)
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
        : TypedExpr(Kind::Decl, type), _linkage(linkage), _name(std::move(name)), _init(init) {}

    auto linkage() const { return _linkage; }
    auto name() const -> const std::string& { return _name; }
    auto init() const { return _init; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Decl; }
};

class FuncDecl : public TypedExpr {
    Linkage _linkage;
    std::string _name;
    std::vector<Decl*> _params;
    Expr* _body;

public:
    FuncDecl(Linkage linkage, Type* type, std::string name, std::vector<Decl*> params, Expr* body)
        : TypedExpr(Kind::Function, type), _linkage(linkage), _name(std::move(name)), _params(std::move(params)), _body(body) {}

    auto linkage() const { return _linkage; }
    auto name() const -> const std::string& { return _name; }
    auto params() const -> std::span<Decl* const> { return _params; }
    auto body() const { return _body; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Function; }
};

class StructDecl : public TypedExpr {
    Symbol* _symbol;

public:
    StructDecl(Symbol* symbol) : TypedExpr(Kind::Struct), _symbol(symbol) {}

    auto symbol() const { return _symbol; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Struct; }
};

class IntegerLiteral : public TypedExpr {
    u64 _value;

public:
    IntegerLiteral(u64 value, Type* type = Type::IntegerLiteralType)
        : TypedExpr(Kind::IntegerLiteral, type), _value(value) {}

    u64 value() const { return _value; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::IntegerLiteral; }
};

class FloatLiteral : public TypedExpr {
    double _value;

public:
    FloatLiteral(double value)
        : TypedExpr(Kind::FloatLiteral, Type::FloatLiteralType), _value(value) {}

    auto value() const { return _value; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::FloatLiteral; }
};

class StringLiteral : public TypedExpr {
    usz _index;

public:
    StringLiteral(usz index, Type* type)
        : TypedExpr(Kind::StringLiteral, type), _index(index) {}

    usz string_index() const { return _index; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::StringLiteral; }
};

class CompoundLiteral : public TypedExpr {
    std::vector<Expr*> _values;

public:
    CompoundLiteral(std::vector<Expr*> values)
        : TypedExpr(Kind::CompoundLiteral), _values(std::move(values)) {}

    auto values() const -> std::span<Expr* const> { return _values; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::CompoundLiteral; }
};

class IfExpr : public TypedExpr {
    Expr* _condition;
    Expr* _then;
    Expr* _else;

public:
    IfExpr(Expr* condition, Expr* then, Expr* else_)
        : TypedExpr(Kind::If), _condition(condition), _then(then), _else(else_) {}

    auto condition() const { return _condition; }
    auto then() const { return _then; }
    auto else_() const { return _else; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::If; }
};

class WhileExpr : public Expr {
    Expr* _condition;
    Expr* _body;

public:
    WhileExpr(Expr* condition, Expr* body)
        : Expr(Kind::While), _condition(condition), _body(body) {}

    auto condition() const { return _condition; }
    auto body() const { return _body; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::While; }
};

class ForExpr : public Expr {
    Expr* _init;
    Expr* _condition;
    Expr* _iterator;
    Expr* _body;

public:
    ForExpr(Expr* init, Expr* condition, Expr* iterator, Expr* body)
        : Expr(Kind::For), _init(init), _condition(condition), _iterator(iterator), _body(body) {}

    auto init() const { return _init; }
    auto condition() const { return _condition; }
    auto iterator() const { return _iterator; }
    auto body() const { return _body; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::For; }
};

class BlockExpr : public TypedExpr {
    std::vector<Expr*> _children;

public:
    BlockExpr(std::vector<Expr*> children)
        : TypedExpr(Kind::Block), _children(std::move(children)) {}

    auto children() const -> std::span<Expr* const> { return _children; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Block; }
};

class ReturnExpr : public Expr {
    Expr* _value;

public:
    ReturnExpr(Expr* value)
        : Expr(Kind::Return), _value(value) {}

    auto value() const { return _value; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Return; }
};

class CallExpr : public TypedExpr {
    Expr* _callee;
    std::vector<Expr*> _args;

public:
    CallExpr(Expr* callee, std::vector<Expr*> args)
        : TypedExpr(Kind::Call), _callee(callee), _args(std::move(args)) {}

    auto callee() const { return _callee; }
    auto args() const -> std::span<Expr* const> { return _args; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Call; }
};

class IntrinsicCallExpr : public TypedExpr {
    IntrinsicKind _kind;
    std::vector<Expr*> _args;

public:
    IntrinsicCallExpr(IntrinsicKind kind, std::vector<Expr*> args)
        : TypedExpr(Kind::IntrinsicCall), _kind(kind), _args(std::move(args)) {}

    auto intrinsic_kind() const { return _kind; }
    auto args() const -> std::span<Expr* const> { return _args; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::IntrinsicCall; }
};

class CastExpr : public TypedExpr {
public:
    CastExpr()
        : TypedExpr(Kind::Cast) {}

    static bool classof(Expr* expr) { return expr->kind() == Kind::Cast; }
};

class UnaryExpr : public TypedExpr {
public:
    UnaryExpr()
        : TypedExpr(Kind::Unary) {}

    static bool classof(Expr* expr) { return expr->kind() == Kind::Unary; }
};

class BinaryExpr : public TypedExpr {
public:
    BinaryExpr()
        : TypedExpr(Kind::Binary) {}

    static bool classof(Expr* expr) { return expr->kind() == Kind::Binary; }
};

class RefExpr : public TypedExpr {
    std::string _name;
    Expr* _target;

public:
    RefExpr(std::string name)
        : TypedExpr(Kind::Reference), _name(std::move(name)) {}

    auto name() const -> const std::string& { return _name; }
    auto target() const { return _target; }
    void target(Expr* target) { _target = target; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Reference; }
};

class MemberAccessExpr : public TypedExpr {
    Expr* _target;
    std::string _name;
    StructMember* _member;

public:
    MemberAccessExpr(Expr* target, std::string name)
        : TypedExpr(Kind::MemberAccess), _target(target), _name(std::move(name)) {}

    auto target() const { return _target; }
    auto name() const -> const std::string& { return _name; }
    auto member() const { return _member; }
    void member(StructMember* member) { _member = member; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::MemberAccess; }
};
} // namespace lcc::intercept

#endif // INTERCEPT_AST_HH
