#ifndef INTERCEPT_AST_HH
#define INTERCEPT_AST_HH

#include <lcc/core.hh>
#include <lcc/diags.hh>
#include <lcc/syntax/token.hh>
#include <lcc/utils.hh>
#include <lcc/utils/result.hh>
#include <span>

namespace lcc::intercept {
enum struct IntrinsicKind {
    BuiltinDebugtrap,
    BuiltinFilename,
    BuiltinInline,
    BuiltinLine,
    BuiltinMemCopy,
    BuiltinMemSet,
    BuiltinSyscall,
};

enum struct FuncAttr {
    Const,
    Discardable,
    Flatten,
    Inline,
    NoInline,
    NoMangle,
    NoOpt,
    NoReturn,
    Pure,
    ReturnsTwice,
    Used,
};

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
    Bool,
    IntKw,
    ArbitraryInt,
    For,
    Return,
    Export,
    Struct,
    Lambda,

    Gensym,
    MacroArg,

    Expression,
};

/// Convert a token kind to a string representation.
auto ToString(TokenKind kind) -> std::string_view;

class Scope;
class Expr;
class Decl;
class FuncDecl;
class Type;
class ObjectDecl;
class Parser;

class Module {
    std::string name{};

    FuncDecl* top_level_function{};
    bool is_module;
    File* file;

    std::vector<Expr*> top_level_nodes;
    std::vector<std::pair<std::string, Module*>> imports;
    std::vector<Decl*> exports;
    std::vector<FuncDecl*> functions;

public:
    Module(File* file, std::string module_name, bool is_logical_module);

    ~Module();

    /// Add an export.
    void add_export(Decl* decl) { exports.push_back(decl); }

    /// Add an import.
    void add_import(std::string module_name) {
        imports.emplace_back(std::move(module_name), nullptr);
    }

    /// Add a top-level expression.
    void add_top_level_expr(Expr* node) { top_level_nodes.push_back(node); }

    /// Intern a string and return its index.
    usz intern(std::string_view str);

    /// Get the top-level function.
    auto top_level_func() const -> FuncDecl* { return top_level_function; }

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

    bool operator==(const InterceptToken& rhs) const;
};

struct Macro {
    std::string name;
    std::vector<InterceptToken*> parameters;
    std::vector<InterceptToken*> expansion;
    Location location;
    usz gensym_count;
};

class Scope {
    Scope* _parent;
    std::unordered_multimap<std::string, Decl*, detail::StringHash, std::equal_to<>> symbols;
    bool is_function_scope = false;

public:
    Scope(Scope* parent) : _parent(parent) {}

    /// Disallow creating scopes without a module reference.
    void* operator new(size_t) = delete;
    void* operator new(size_t sz, Module& mod) {
        auto ptr = ::operator new(sz);
        mod.scopes.push_back(static_cast<Scope*>(ptr));
        return ptr;
    }

    /// Declare a symbol in this scope.
    ///
    /// If the name doesn’t already exist in this scope, it is
    /// added. If the name already exists, and the declaration
    /// is not a function declaration, this returns a diagnostic.
    ///
    /// \param parser The Intercept parser.
    /// \param name The name of the declared symbol.
    /// \param decl The declaration to bind to the symbol.
    /// \return The same declaration, or an error.
    auto declare(
        Parser* ctx,
        std::string&& name,
        Decl* decl
    ) -> Result<Decl*>;

    /// Get the parent scope.
    auto parent() const { return _parent; }

    /// Mark this scope as a function scope.
    void set_function_scope() { is_function_scope = true; }
};

class Type {
    friend class lcc::Context;

public:
    enum struct Kind {
        Builtin,
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
    void* operator new(size_t, Module&);

    auto kind() const { return _kind; }
    auto location() const { return _location; }

    /// Return this type stripped of any aliases.
    auto strip_aliases() -> Type*;

    /// Use these only if there is no location information
    /// available (e.g. for default initialisers etc.). In
    /// any other case, prefer to create an instance of a
    /// BuiltinType instead.
    ///
    /// When adding a type here, don’t forget to initialise
    /// it in Context::InitialiseLCCData().
    static Type* Unknown;
    static Type* Integer;
};

/// This only holds a kind.
///
/// Builtin types are not singletons because they need to carry
/// location information. However, the size, alignment, etc of a
/// primitive type all depend on the target, and there is no point
/// in storing their names in each of them, so an instance of one
/// only ever stores its primitive type kind.
class BuiltinType : public Type {
public:
    enum struct BuiltinKind {
        Bool,
        Byte,
        CChar,
        CInt,
        Integer,
        Unknown,
        Void,
    };

private:
    /// Shorten long signatures w/ this.
    using K = BuiltinKind;

    const BuiltinKind _kind;

    static auto Create(Module* mod, K k, Location l) -> BuiltinType* {
        return new (*mod) BuiltinType(k, l);
    }

    BuiltinType(K k, Location location)
        : Type(Kind::Builtin, location), _kind(k) {}

public:
    /// Get the kind of this builtin.
    auto builtin_kind() -> BuiltinKind { return _kind; }

    bool operator==(const BuiltinType& other) const { return _kind == other._kind; }
    bool operator==(BuiltinKind k) const { return _kind == k; }

    /// Get instances of primitive types.
    static auto Bool(Module* mod, Location l = {}) -> BuiltinType* { return Create(mod, K::Bool, l); }
    static auto Byte(Module* mod, Location l = {}) -> BuiltinType* { return Create(mod, K::Byte, l); }
    static auto CChar(Module* mod, Location l = {}) -> BuiltinType* { return Create(mod, K::CChar, l); }
    static auto CInt(Module* mod, Location l = {}) -> BuiltinType* { return Create(mod, K::CInt, l); }
    static auto Integer(Module* mod, Location l = {}) -> BuiltinType* { return Create(mod, K::Integer, l); }
    static auto Unknown(Module* mod, Location l = {}) -> BuiltinType* { return Create(mod, K::Unknown, l); }
    static auto Void(Module* mod, Location l = {}) -> BuiltinType* { return Create(mod, K::Void, l); }

    static bool classof(Type* type) { return type->kind() == Kind::Builtin; }
};

class NamedType : public Type {
    std::string _name;

public:
    NamedType(std::string name, Location location)
        : Type(Kind::Named, location), _name(std::move(name)) {}

    auto name() const -> const std::string& { return _name; }

    static bool classof(Type* type) { return type->kind() == Kind::Named; }
};

class TypeWithOneElement : public Type {
    Type* _element_type;

protected:
    TypeWithOneElement(Kind kind, Location location, Type* element_type)
        : Type(kind, location), _element_type(element_type) {}

public:
    auto element_type() const { return _element_type; }
};

class PointerType : public TypeWithOneElement {
public:
    PointerType(Type* element_type, Location location = {})
        : TypeWithOneElement(Kind::Pointer, location, element_type) {}

    static bool classof(Type* type) { return type->kind() == Kind::Pointer; }
};

class ReferenceType : public TypeWithOneElement {
public:
    ReferenceType(Type* element_type, Location location)
        : TypeWithOneElement(Kind::Reference, location, element_type) {}

    static bool classof(Type* type) { return type->kind() == Kind::Reference; }
};

class ArrayType : public TypeWithOneElement {
    Expr* _size;

public:
    ArrayType(Type* element_type, Expr* size, Location location = {})
        : TypeWithOneElement(Kind::Array, location, element_type), _size(size) {}

    Expr* size() const { return _size; }

    static bool classof(Type* type) { return type->kind() == Kind::Array; }
};

class FuncType : public Type {
public:
    /// This should allow us to easily support attributes that
    /// take arguments once we need those.
    using Attributes = std::unordered_map<FuncAttr, bool>;

    struct Param {
        std::string name;
        Type* type;
        Location location;

        Param(std::string name, Type* type, Location location)
            : name(std::move(name)), type(type), location(location) {}
    };

private:
    Type* _return_type;
    std::vector<Param> _params;
    Attributes _attributes;

public:
    FuncType(
        std::vector<Param> params,
        Type* return_type,
        Attributes attrs,
        Location location
    ) : Type(Kind::Function, location),
        _return_type(return_type),
        _params(std::move(params)),
        _attributes(std::move(attrs)) {}

    /// Query whether this function has an attribute.
    bool has_attr(FuncAttr attr) const { return _attributes.contains(attr); }

    /// Get the parameters of this function.
    auto params() -> std::span<Param const> const { return _params; }

    /// Remove an attribute from this function.
    void remove_attr(FuncAttr attr) { _attributes.erase(attr); }

    /// Get the return type of this function.
    auto return_type() const { return _return_type; }

    /// Set an attribute on this function.
    void set_attr(FuncAttr attr) { _attributes[attr] = true; }

    static bool classof(Type* type) { return type->kind() == Kind::Function; }
};

class StructDecl;

class StructType : public Type {
public:
    struct Member {
        Type* type;
        std::string name;
        Location location;
        usz byte_offset{};

        Member(std::string name, Type* type, Location location)
            : type(type), name(std::move(name)), location(location) {}
    };

private:
    StructDecl* _struct_decl{};
    std::vector<Member> _members;

    usz _byte_size{};
    usz _alignment{};

public:
    StructType(std::vector<Member> members, Location location)
        : Type(Kind::Struct, location), _members(std::move(members)) {}

    usz alignment() const { return _alignment; }
    void alignment(usz alignment) { _alignment = alignment; }

    /// Associate this type with a declaration.
    void associate_with_decl(StructDecl* decl) {
        LCC_ASSERT(not _struct_decl, "Cannot associate a struct type with two struct decls");
        _struct_decl = decl;
    }

    usz byte_size() const { return _byte_size; }
    void byte_size(usz byteSize) { _byte_size = byteSize; }

    /// Get the declaration of this type.
    ///
    /// If this is an anonymous type, this returns null.
    auto decl() const -> StructDecl* { return _struct_decl; }

    std::span<Member const> members() { return _members; }

    static bool classof(Type* type) { return type->kind() == Kind::Struct; }
};

class IntegerType : public Type {
    usz _bit_width;
    bool _is_signed;

public:
    IntegerType(usz bitWidth, bool isSigned, Location location)
        : Type(Kind::Integer, location), _bit_width(bitWidth), _is_signed(isSigned) {}

    bool is_signed() const { return _is_signed; }
    usz bit_width() const { return _bit_width; }

    static bool classof(Type* type) { return type->kind() == Kind::Integer; }
};

/// \brief Base class for expression syntax nodes.
class Expr {
public:
    /// Do NOT reorder these!
    enum struct Kind {
        StructDecl,
        TypeAliasDecl,
        VarDecl,
        FuncDecl,

        IntegerLiteral,
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

        NameRef,
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

    /// Access the location of this expression.
    auto location() const { return _location; }
    auto location(Location location) { _location = location; }

    /// Deep-copy an expression.
    static Expr* Clone(Module& mod, Expr* expr);
};

class TypedExpr : public Expr {
    Type* _type;

protected:
    TypedExpr(Kind kind, Location location, Type* type = Type::Unknown)
        : Expr(kind, location), _type(type) {}

public:
    void type(Type* type) { _type = type; }
};

/// Base class for declarations. Declarations have a name.
class Decl : public TypedExpr {
    std::string _name;

protected:
    Decl(Kind kind, std::string name, Type* type, Location location)
        : TypedExpr(kind, location, type), _name(std::move(name)) {}

public:
    auto name() const -> const std::string& { return _name; }

    static bool classof(Expr* expr) {
        return expr->kind() >= Kind::StructDecl and expr->kind() <= Kind::FuncDecl;
    }
};

/// A declaration that has linkage.
class ObjectDecl : public Decl {
    Module* _mod;
    Linkage _linkage;

protected:
    ObjectDecl(
        Kind kind,
        Type* type,
        std::string name,
        Module* mod,
        Linkage linkage,
        Location location
    ) : Decl(kind, std::move(name), type, location),
        _mod(mod),
        _linkage(linkage) {}

public:
    /// Get the mangled name of this declaration.
    auto mangled_name() const -> std::string;

    /// Get the module this declaration is in.
    auto module() const -> Module* { return _mod; }

    /// Get the linkage of this declaration.
    auto linkage() const { return _linkage; }

    /// Set the linkage of this declaration.
    void linkage(Linkage linkage) { _linkage = linkage; }

    /// RTTI.
    static bool classof(Expr* expr) {
        return expr->kind() >= Kind::VarDecl and expr->kind() <= Kind::FuncDecl;
    }
};

class VarDecl : public ObjectDecl {
    Expr* _init;

public:
    VarDecl(
        std::string name,
        Type* type,
        Expr* init,
        Module* mod,
        Linkage linkage,
        Location location
    ) : ObjectDecl(Kind::VarDecl, type, std::move(name), mod, linkage, location),
        _init(init) {}

    auto init() const { return _init; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::VarDecl; }
};

class FuncDecl : public ObjectDecl {
    Expr* _body;

    /// Only present if this is not an imported function. Sema
    /// fills these in.
    std::vector<VarDecl*> _params;

public:
    FuncDecl(
        std::string name,
        FuncType* type,
        Expr* body,
        Module* mod,
        Linkage linkage,
        Location location
    ) : ObjectDecl(Kind::FuncDecl, type, std::move(name), mod, linkage, location),
        _body(body) {}

    auto params() const -> std::span<VarDecl* const> { return _params; }
    auto body() const { return _body; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::FuncDecl; }
};

class StructDecl : public Decl {
    /// The module this struct is declared in.
    Module* _module;

public:
    StructDecl(Module* mod, std::string name, StructType* declared_type, Location location)
        : Decl(Kind::StructDecl, std::move(name), declared_type, location), _module(mod) {
        declared_type->associate_with_decl(this);
    }

    /// Get the module this struct is declared in.
    auto module() const -> Module* { return _module; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::StructDecl; }
};

class TypeAliasDecl : public Decl {
public:
    TypeAliasDecl(std::string name, Type* aliased_type, Location location)
        : Decl(Kind::TypeAliasDecl, std::move(name), aliased_type, location) {}

    static bool classof(Expr* expr) { return expr->kind() == Kind::TypeAliasDecl; }
};

class IntegerLiteral : public TypedExpr {
    u64 _value;

public:
    IntegerLiteral(u64 value, Location location, Type* ty = Type::Integer)
        : TypedExpr(Kind::IntegerLiteral, location, ty), _value(value) {}

    u64 value() const { return _value; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::IntegerLiteral; }
};

class StringLiteral : public TypedExpr {
    usz _index;

public:
    /// Intern the given string and create a string literal for it.
    StringLiteral(Module& mod, std::string_view value, Location location);

    usz string_index() const { return _index; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::StringLiteral; }
};

class CompoundLiteral : public TypedExpr {
    std::vector<Expr*> _values;

public:
    CompoundLiteral(std::vector<Expr*> values, Location location)
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
    Expr* _value;

public:
    CastExpr(Expr* value, Type* ty, Location location)
        : TypedExpr(Kind::Cast, location, ty), _value(value) {}

    /// Get the operand of this expression.
    auto operand() const { return _value; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Cast; }
};

class UnaryExpr : public TypedExpr {
    Expr* _operand;
    TokenKind _op;
    bool _postfix;

public:
    UnaryExpr(TokenKind op, Expr* operand, bool is_postfix, Location location)
        : TypedExpr(Kind::Unary, location), _operand(operand), _op(op), _postfix(is_postfix) {}

    /// Check if this is a postfix unary expression.
    bool is_postfix() const { return _postfix; }

    /// Get the operand of this expression.
    auto operand() const { return _operand; }

    /// Get the unary operator.
    auto op() const { return _op; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Unary; }
};

class BinaryExpr : public TypedExpr {
    Expr* _lhs;
    Expr* _rhs;
    TokenKind _op;

public:
    BinaryExpr(TokenKind op, Expr* lhs, Expr* rhs, Location location)
        : TypedExpr(Kind::Binary, location), _lhs(lhs), _rhs(rhs), _op(op) {}

    /// Get the left-hand side of this expression.
    auto lhs() const { return _lhs; }

    /// Get the right-hand side of this expression.
    auto rhs() const { return _rhs; }

    /// Get the binary operator.
    auto op() const { return _op; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Binary; }
};

class NameRefExpr : public TypedExpr {
    std::string _name;
    Expr* _target;

public:
    NameRefExpr(std::string name, Location location)
        : TypedExpr(Kind::NameRef, location), _name(std::move(name)) {}

    auto name() const -> const std::string& { return _name; }
    auto target() const { return _target; }
    void target(Expr* target) { _target = target; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::NameRef; }
};

class MemberAccessExpr : public TypedExpr {
    Expr* _object;
    std::string _name;
    StructType::Member* _member;

public:
    MemberAccessExpr(Expr* object, std::string name, Location location)
        : TypedExpr(Kind::MemberAccess, location), _object(object), _name(std::move(name)) {}

    auto name() const -> const std::string& { return _name; }
    auto member() const { return _member; }
    void member(StructType::Member* member) { _member = member; }
    auto object() const { return _object; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::MemberAccess; }
};
} // namespace lcc::intercept

#endif // INTERCEPT_AST_HH
