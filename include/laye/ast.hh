#ifndef LAYE_AST_HH
#define LAYE_AST_HH

#include <lcc/core.hh>
#include <lcc/diags.hh>
#include <lcc/syntax/token.hh>
#include <lcc/utils.hh>
#include <span>

namespace lcc::laye {
class ModuleHeader;
class Statement;
class Decl;
class Expr;
class Type;
class Symbol;
class Scope;

struct Module {
    std::vector<ModuleHeader*> headers;
    std::vector<Decl*> topLevelDecls;

private:
    std::vector<Statement*> statements;
    std::vector<Expr*> exprs;
    std::vector<Scope*> scopes;

    friend Statement;
    friend Expr;
    friend Scope;
};

enum struct TokenKind {
    Invalid,
};

using LayeToken = syntax::Token<TokenKind>;

class Symbol {
public:
    enum struct Kind {
        Function,
        Binding,
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

class BindingSymbol : public Symbol {
    Expr* _value;

public:
    BindingSymbol(std::string name, Scope* scope, Expr* value)
        : Symbol(Kind::Binding, name, scope), _value(value) {}

    auto value() const { return _value; }

    static bool classof(Symbol* symbol) { return symbol->kind() == Kind::Binding; }
};

class FunctionSymbol : public Symbol {
    Expr* _value;

public:
    FunctionSymbol(std::string name, Scope* scope, Expr* value)
        : Symbol(Kind::Function, name, scope), _value(value) {}

    auto value() const { return _value; }

    static bool classof(Symbol* symbol) { return symbol->kind() == Kind::Function; }
};

class TypeSymbol : public Symbol {
    Type* _type;

public:
    TypeSymbol(std::string name, Scope* scope, Type* type)
        : Symbol(Kind::Type, name, scope), _type(type) {}

    auto type() const { return _type; }

    static bool classof(Symbol* symbol) { return symbol->kind() == Kind::Type; }
};

class Scope {
    Scope* _parent;
    std::vector<Symbol*> _symbols;

public:
    Scope(Scope* parent)
        : _parent(parent) {}

    /// Disallow creating scopes without a module reference.
    void* operator new(size_t) = delete;
    void* operator new(size_t sz, Module& mod) {
        auto ptr = ::operator new(sz);
        mod.scopes.push_back(static_cast<Scope*>(ptr));
        return ptr;
    }

    auto parent() const { return _parent; }
};

/// @brief Base class for statement syntax nodes.
class Statement {
public:
    enum struct Kind {
        // Declarations
        DeclBinding,
        DeclFunction,
        DeclStruct,
        DeclEnum,
        DeclAlias,

        // Module Headers
        DeclImport,
        DeclForeignImport,

        // Simple Statements
        Block,
        Assign,
        Expr,

        // Control Flow
        If,
        For,
        While,
        Switch,
        Return,
        Break,
        Continue,
        Yield,
    };

private:
    const Kind _kind;

    Location _location;

protected:
    Statement(Kind kind, Location location)
        : _kind(kind), _location(location) {}

public:
    void* operator new(size_t) = delete;
    void* operator new(size_t sz, Module& mod) {
        auto ptr = ::operator new(sz);
        mod.statements.push_back(static_cast<Statement*>(ptr));
        return ptr;
    }

    auto kind() const { return _kind; }
    auto location() const { return _location; }
};

class Decl : public Statement {
protected:
    Decl(Kind kind, Location location)
        : Statement(kind, location) {}

public:
    static bool classof(Statement* statement) { return +statement->kind() >= +Kind::DeclBinding and +statement->kind() <= +Kind::DeclForeignImport; }
};

class NamedDecl : public Decl {
    std::string _name;

protected:
    NamedDecl(Kind kind, Location location, std::string name)
        : Decl(kind, location), _name(std::move(name)) {}

public:
    const std::string& name() const { return _name; }

    static bool classof(Statement* statement) { return +statement->kind() >= +Kind::DeclBinding and +statement->kind() <= +Kind::DeclAlias; }
};

class BindingDecl : public NamedDecl {
    Type* _type;
    Expr* _init;

public:
    BindingDecl(Location location, Type* type, std::string name, Expr* init)
        : NamedDecl(Kind::DeclBinding, location, name), _type(type), _init(init) {}

    auto type() const { return _type; }
    auto init() const { return _init; }

    static bool classof(Statement* statement) { return statement->kind() == Kind::DeclBinding; }
};

struct FunctionParam {
    Type* type;
    std::string name;
    Expr* init;
};

class FunctionDecl : public NamedDecl {
    Type* _returnType;
    std::vector<FunctionParam> _params;
    Statement* _body;

public:
    FunctionDecl(Location location, Type* returnType, std::vector<FunctionParam> params, std::string name, Statement* body)
        : NamedDecl(Kind::DeclFunction, location, name), _returnType(returnType), _params(std::move(params)), _body(body) {}

    auto return_type() const { return _returnType; }
    auto params() const -> std::span<FunctionParam const> { return _params; }
    auto body() const { return _body; }

    static bool classof(Statement* statement) { return statement->kind() == Kind::DeclFunction; }
};

struct StructField {
    Type* type;
    std::string name;
    Expr* init;
};

struct StructVariant {
    std::string name;
    std::vector<StructField> fields;
};

class StructDecl : public NamedDecl {
    std::vector<StructField> _fields;
    std::vector<StructVariant> _variants;

public:
    StructDecl(Location location, std::string name, std::vector<StructField> fields, std::vector<StructVariant> variants)
        : NamedDecl(Kind::DeclStruct, location, name), _fields(std::move(fields)), _variants(std::move(variants)) {}

    auto fields() const -> std::span<StructField const> { return _fields; }
    auto variants() const -> std::span<StructVariant const> { return _variants; }

    static bool classof(Statement* statement) { return statement->kind() == Kind::DeclStruct; }
};

struct EnumVariant {
    std::string name;
    Expr* init;
};

class EnumDecl : public NamedDecl {
    Type* _underlyingType;
    std::vector<EnumVariant> _variants;

public:
    EnumDecl(Location location, std::string name, Type* underlyingType, std::vector<EnumVariant> variants)
        : NamedDecl(Kind::DeclEnum, location, name), _underlyingType(underlyingType), _variants(std::move(variants)) {}

    auto underlying_type() const { return _underlyingType; }
    auto variants() const -> std::span<EnumVariant const> { return _variants; }

    static bool classof(Statement* statement) { return statement->kind() == Kind::DeclEnum; }
};

class AliasDecl : public NamedDecl {
public:
    AliasDecl(Location location, std::string name)
        : NamedDecl(Kind::DeclAlias, location, name) {}

    static bool classof(Statement* statement) { return statement->kind() == Kind::DeclAlias; }
};

/// @brief Base class for file header nodes.
class ModuleHeader : public Decl {
protected:
    ModuleHeader(Kind kind, Location location)
        : Decl(kind, location) {}

public:
    static bool classof(Statement* statement) { return +statement->kind() >= +Kind::DeclImport and +statement->kind() <= +Kind::DeclForeignImport; }
};

class ImportHeader : public ModuleHeader {
public:
    ImportHeader(Location location)
        : ModuleHeader(Kind::DeclImport, location) {}

    static bool classof(Statement* statement) { return statement->kind() == Kind::DeclImport; }
};

class ForeignImportHeader : public ModuleHeader {
public:
    ForeignImportHeader(Location location)
        : ModuleHeader(Kind::DeclForeignImport, location) {}

    static bool classof(Statement* statement) { return statement->kind() == Kind::DeclForeignImport; }
};

/// @brief Base class for expression syntax nodes.
class Expr {
public:
    enum struct Kind {
        Unary,
        Binary,

        // Lookups etc.
        LookupName,
        LookupPath,
        FieldIndex,
        ValueIndex,
        Slice,
        Invoke,
        Ctor,

        // Keyword Prefixed
        Cast,
        New,
        Try,
        Catch,

        // Literals
        LitNil,
        LitBool,
        LitString,
        LitInt,
        LitFloat,

        // Types
        TypeInfer,
        TypeErrUnion,

        TypeArray,
        TypeSlice,
        TypePointer,
        TypeBuffer,

        TypeFunc,

        TypeNoreturn,
        TypeRawptr,
        TypeVoid,
        TypeString,
        TypeRune,
        TypeBool,
        TypeInt,
        TypeFloat,

        TypeC,
    };

private:
    const Kind _kind;

    Location _location;

protected:
    Expr(Kind kind, Location location)
        : _kind(kind), _location(location) {}

public:
    void* operator new(size_t) = delete;
    void* operator new(size_t sz, Module& mod) {
        auto ptr = ::operator new(sz);
        mod.exprs.push_back(static_cast<Expr*>(ptr));
        return ptr;
    }

    auto kind() const { return _kind; }
    auto location() const { return _location; }
};

class Unary : public Expr {
    Expr* _value;

public:
    Unary(Location location, Expr* value)
        : Expr(Kind::Unary, location), _value(value) {}

    auto value() const { return _value; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Unary; }
};

/// @brief Base class for type syntax nodes (which we're trying to make also Exprs.)
class Type : public Expr {
protected:
    Type(Kind kind, Location location)
        : Expr(kind, location) {}

    static bool classof(Expr* expr) { return +expr->kind() >= +Kind::TypeInfer && +expr->kind() <= +Kind::TypeC; }
};
} // namespace lcc::laye

#endif // LAYE_AST_HH
