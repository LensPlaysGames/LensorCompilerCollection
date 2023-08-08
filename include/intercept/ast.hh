#ifndef INTERCEPT_AST_HH
#define INTERCEPT_AST_HH

#include <lcc/core.hh>
#include <lcc/diags.hh>
#include <lcc/syntax/token.hh>
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

enum struct TokenKind {
    Invalid,
    Eof,

    LParen,
    RParen,
    LBrack,
    RBrack,
    LBrace,
    RBrace,

    Comma,
    Colon,
    Semicolon,
    Dot,

    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Ampersand,
    Pipe,
    Caret,
    Tilde,
    Exclam,
    At,
    Hash,

    Shl,
    Shr,

    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,

    ColonEq,
    ColonColon,
    ColonGt,

    Ident,
    Number,
    String,

    If,
    Else,
    While,
    Extern,
    As,
    Type,
    Void,
    Byte,
    IntegerKw,
    ArbitraryInt,
    For,
    Return,
    Export,

    Gensym,
    MacroArg,

    Expression,
};

/// Convert a token kind to a string representation.
auto ToString(TokenKind kind) -> std::string_view;

class Scope;
class Symbol;
class Expr;
class FuncDecl;
class Type;

class Module {
    std::string name{};

    FuncDecl* top_level_function{};
    bool is_module;
    File* file;

    std::vector<Expr*> top_level_nodes;
    std::vector<std::pair<std::string, Module*>> imports;
    std::vector<Expr*> exports;
    std::vector<FuncDecl*> functions;

public:
    Module(File* file)
        : file(file) {}

    ~Module();

    /// Add an import.
    void add_import(std::string module_name) {
        imports.emplace_back(std::move(module_name), nullptr);
    }

    /// Add a top-level expression.
    void add_top_level_expr(Expr* node) {
        top_level_nodes.push_back(node);
    }

    /// Mark this as a module.
    void set_logical_module(std::string module_name) {
        is_module = true;
        name = std::move(module_name);
    }

    std::vector<Expr*> nodes;
    std::vector<Type*> types;
    std::vector<Scope*> scopes;
    std::vector<std::string> strings;
};

struct InterceptToken : public syntax::Token<TokenKind> {
    Expr* expression{};

    /// Whether the expression bound by this token should
    /// only be evaluated once.
    bool eval_once : 1 = true;
};

struct Macro {
    std::string name;
    std::vector<InterceptToken*> parameters;
    std::vector<InterceptToken*> expansion;
    Location location;
    usz gensym_count;
};

class Attribute {
    int kind;
    isz integer_value;
};

class Scope {
    Scope* _parent;
    std::vector<Symbol*> _symbols;

public:
    Scope(Scope* parent) : _parent(parent) {}

    /// Disallow creating scopes without a module reference.
    void* operator new(size_t) = delete;
    void* operator new(size_t sz, Module& mod) {
        auto ptr = ::operator new (sz);
        mod.scopes.push_back(static_cast<Scope*>(ptr));
        return ptr;
    }

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

    Location _location;

protected:
    Type(Kind kind, Location location)
        : _kind(kind), _location(location) {}

public:
    virtual ~Type() = default;

    void* operator new(size_t) = delete;

    auto kind() const { return _kind; }
    auto location() const { return _location; }

    static Type* UnknownType;
    static Type* IntegerLiteralType;
    static Type* FloatLiteralType;
    static Type* BoolType;
};

struct FuncTypeParam {
    Location location;
    std::string _name;
    Type* type;

    FuncTypeParam(Location location, std::string name, Type* type)
        : location(location), _name(std::move(name)), type(type) {}
};

class PrimitiveType : public Type {
    usz _size;
    usz _alignment;
    std::string_view _name;
    bool _is_signed;

public:
    PrimitiveType(Location location, usz size, usz alignment, std::string_view name, bool isSigned)
        : Type(Kind::Primitive, location), _size(size), _alignment(alignment), _name(name), _is_signed(isSigned) {}

    usz size() const { return _size; }
    usz alignment() const { return _alignment; }
    auto name() -> std::string_view const { return _name; }
    bool is_signed() const { return _is_signed; }

    static bool classof(Type* type) { return type->kind() == Kind::Primitive; }
};

class NamedType : public Type {
    std::string _name;

public:
    NamedType(Location location, std::string name)
        : Type(Kind::Primitive, location), _name(std::move(name)) {}

    auto name() const -> const std::string& { return _name; }

    static bool classof(Type* type) { return type->kind() == Kind::Named; }
};

class TypeWithOneElement : public Type {
    Type* _element_type;

protected:
    TypeWithOneElement(Kind kind, Location location, Type* elementType)
        : Type(kind, location), _element_type(elementType) {}

public:
    auto element_type() const { return _element_type; }
};

class PointerType : public TypeWithOneElement {
public:
    PointerType(Location location, Type* elementType)
        : TypeWithOneElement(Kind::Primitive, location, elementType) {}

    static bool classof(Type* type) { return type->kind() == Kind::Pointer; }
};

class ReferenceType : public TypeWithOneElement {
public:
    ReferenceType(Location location, Type* elementType)
        : TypeWithOneElement(Kind::Reference, location, elementType) {}

    static bool classof(Type* type) { return type->kind() == Kind::Reference; }
};

class ArrayType : public TypeWithOneElement {
    usz _size;

public:
    ArrayType(Location location, Type* elementType, usz size)
        : TypeWithOneElement(Kind::Array, location, elementType), _size(size) {}

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
    FuncType(Location location, Type* returnType, std::vector<FuncTypeParam*> params)
        : Type(Kind::Function, location), _return_type(returnType), _params(std::move(params)) {}

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

struct StructMember {
    Location location;
    Type* type;
    std::string name;
    usz byte_offset;

    StructMember(Location location, Type* type, std::string name, usz byteOffset)
        : location(location), type(type), name(std::move(name)), byte_offset(byteOffset) {}
};

class StructType : public Type {
    StructDecl* _structDecl;
    std::vector<StructMember*> _members;

    usz _byte_size;
    usz _alignment;

public:
    StructType(Location location, StructDecl* structDecl, std::vector<StructMember*> members)
        : Type(Kind::Struct, location), _structDecl(structDecl), _members(std::move(members)) {}

    StructDecl* struct_decl() const { return _structDecl; }
    std::span<StructMember* const> members() { return _members; }

    usz byte_size() const { return _byte_size; }
    void byte_size(usz byteSize) { _byte_size = byteSize; }

    usz alignment() const { return _alignment; }
    void alignment(usz alignment) { _alignment = alignment; }

    static bool classof(Type* type) { return type->kind() == Kind::Struct; }
};

class IntegerType : public Type {
    bool _is_signed;
    usz _bit_width;

public:
    IntegerType(Location location, bool isSigned, usz bitWidth)
        : Type(Kind::Integer, location), _is_signed(isSigned), _bit_width(bitWidth) {}

    bool is_signed() const { return _is_signed; }
    usz bit_width() const { return _bit_width; }

    static bool classof(Type* type) { return type->kind() == Kind::Integer; }
};

/// @brief Base class for expression syntax nodes.
class Expr {
public:
    enum struct Kind {
        Function,
        Struct,
        VarDecl,

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

        NamedRef,
        MemberAccess,
    };

private:
    const Kind _kind;

    Location _location;

protected:
    Expr(Kind kind, Location location)
        : _kind(kind), _location(location) {}

public:
    virtual ~Expr() = default;

    void* operator new(size_t) = delete;
    void* operator new(size_t sz, Module& mod) {
        auto ptr = ::operator new(sz);
        mod.nodes.push_back(static_cast<Expr*>(ptr));
        return ptr;
    }

    auto type() const -> Type*;

    Kind kind() const { return _kind; }
    auto location() const { return _location; }

    /// Deep-copy an expression.
    static Expr* Clone(Module& mod, Expr* expr);
};

class TypedExpr : public Expr {
    Type* _type;

protected:
    TypedExpr(Kind kind, Location location, Type* type = Type::UnknownType)
        : Expr(kind, location), _type(type) {}

public:
    void type(Type* type) { _type = type; }
};

class VarDecl : public TypedExpr {
    Linkage _linkage;
    std::string _name;
    Expr* _init;

public:
    VarDecl(Location location, Linkage linkage, Type* type, std::string name, Expr* init)
        : TypedExpr(Kind::VarDecl, location, type), _linkage(linkage), _name(std::move(name)), _init(init) {}

    auto linkage() const { return _linkage; }
    auto name() const -> const std::string& { return _name; }
    auto init() const { return _init; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::VarDecl; }
};

class FuncDecl : public TypedExpr {
    Linkage _linkage;
    std::string _name;
    std::vector<VarDecl*> _params;
    Expr* _body;

public:
    FuncDecl(Location location, Linkage linkage, Type* type, std::string name, std::vector<VarDecl*> params, Expr* body)
        : TypedExpr(Kind::Function, location, type), _linkage(linkage), _name(std::move(name)), _params(std::move(params)), _body(body) {}

    auto linkage() const { return _linkage; }
    auto name() const -> const std::string& { return _name; }
    auto params() const -> std::span<VarDecl* const> { return _params; }
    auto body() const { return _body; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Function; }
};

class StructDecl : public TypedExpr {
    Symbol* _symbol;

public:
    StructDecl(Location location, Symbol* symbol)
        : TypedExpr(Kind::Struct, location), _symbol(symbol) {}

    auto symbol() const { return _symbol; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Struct; }
};

class IntegerLiteral : public TypedExpr {
    u64 _value;

public:
    IntegerLiteral(Location location, u64 value, Type* type = Type::IntegerLiteralType)
        : TypedExpr(Kind::IntegerLiteral, location, type), _value(value) {}

    u64 value() const { return _value; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::IntegerLiteral; }
};

class FloatLiteral : public TypedExpr {
    double _value;

public:
    FloatLiteral(Location location, double value)
        : TypedExpr(Kind::FloatLiteral, location, Type::FloatLiteralType), _value(value) {}

    auto value() const { return _value; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::FloatLiteral; }
};

class StringLiteral : public TypedExpr {
    usz _index;

public:
    StringLiteral(Location location, usz index, Type* type)
        : TypedExpr(Kind::StringLiteral, location, type), _index(index) {}

    usz string_index() const { return _index; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::StringLiteral; }
};

class CompoundLiteral : public TypedExpr {
    std::vector<Expr*> _values;

public:
    CompoundLiteral(Location location, std::vector<Expr*> values)
        : TypedExpr(Kind::CompoundLiteral, location), _values(std::move(values)) {}

    auto values() const -> std::span<Expr* const> { return _values; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::CompoundLiteral; }
};

class IfExpr : public TypedExpr {
    Expr* _condition;
    Expr* _then;
    Expr* _else;

public:
    IfExpr(Expr* condition, Expr* then, Expr* else_, Location location)
        : TypedExpr(Kind::If, location), _condition(condition), _then(then), _else(else_) {}

    auto condition() const { return _condition; }
    auto then() const { return _then; }
    auto else_() const { return _else; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::If; }
};

class WhileExpr : public Expr {
    Expr* _condition;
    Expr* _body;

public:
    WhileExpr(Expr* condition, Expr* body, Location location)
        : Expr(Kind::While, location), _condition(condition), _body(body) {}

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
    ForExpr(Expr* init, Expr* condition, Expr* iterator, Expr* body, Location location)
        : Expr(Kind::For, location), _init(init), _condition(condition), _iterator(iterator), _body(body) {}

    auto init() const { return _init; }
    auto condition() const { return _condition; }
    auto iterator() const { return _iterator; }
    auto body() const { return _body; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::For; }
};

class BlockExpr : public TypedExpr {
    std::vector<Expr*> _children;

public:
    BlockExpr(std::vector<Expr*> children, Location location)
        : TypedExpr(Kind::Block, location), _children(std::move(children)) {}

    auto children() const -> std::span<Expr* const> { return _children; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Block; }
};

class ReturnExpr : public Expr {
    Expr* _value;

public:
    ReturnExpr(Expr* value, Location location)
        : Expr(Kind::Return, location), _value(value) {}

    auto value() const { return _value; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Return; }
};

class CallExpr : public TypedExpr {
    Expr* _callee;
    std::vector<Expr*> _args;

public:
    CallExpr(Expr* callee, std::vector<Expr*> args, Location location)
        : TypedExpr(Kind::Call, location), _callee(callee), _args(std::move(args)) {}

    auto callee() const { return _callee; }
    auto args() const -> std::span<Expr* const> { return _args; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Call; }
};

class IntrinsicCallExpr : public TypedExpr {
    IntrinsicKind _kind;
    std::vector<Expr*> _args;

public:
    IntrinsicCallExpr(Location location, IntrinsicKind kind, std::vector<Expr*> args)
        : TypedExpr(Kind::IntrinsicCall, location), _kind(kind), _args(std::move(args)) {}

    auto intrinsic_kind() const { return _kind; }
    auto args() const -> std::span<Expr* const> { return _args; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::IntrinsicCall; }
};

class CastExpr : public TypedExpr {
public:
    CastExpr(Location location)
        : TypedExpr(Kind::Cast, location) {}

    static bool classof(Expr* expr) { return expr->kind() == Kind::Cast; }
};

class UnaryExpr : public TypedExpr {
public:
    UnaryExpr(Location location)
        : TypedExpr(Kind::Unary, location) {}

    static bool classof(Expr* expr) { return expr->kind() == Kind::Unary; }
};

class BinaryExpr : public TypedExpr {
public:
    BinaryExpr(Location location)
        : TypedExpr(Kind::Binary, location) {}

    static bool classof(Expr* expr) { return expr->kind() == Kind::Binary; }
};

class NamedRefExpr : public TypedExpr {
    std::string _name;
    Expr* _target;

public:
    NamedRefExpr(Location location, std::string name)
        : TypedExpr(Kind::NamedRef, location), _name(std::move(name)) {}

    auto name() const -> const std::string& { return _name; }
    auto target() const { return _target; }
    void target(Expr* target) { _target = target; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::NamedRef; }
};

class MemberAccessExpr : public TypedExpr {
    Expr* _target;
    std::string _name;
    StructMember* _member;

public:
    MemberAccessExpr(Location location, Expr* target, std::string name)
        : TypedExpr(Kind::MemberAccess, location), _target(target), _name(std::move(name)) {}

    auto target() const { return _target; }
    auto name() const -> const std::string& { return _name; }
    auto member() const { return _member; }
    void member(StructMember* member) { _member = member; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::MemberAccess; }
};
} // namespace lcc::intercept

#endif // INTERCEPT_AST_HH
