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

struct Module {
    std::vector<ModuleHeader*> headers;
    std::vector<Decl*> topLevelDecls;

private:
    std::vector<Statement*> _statements_;
    std::vector<Expr*> _exprs_;
};

enum struct TokenKind {
    Invalid,
};

using LayeToken = syntax::Token<TokenKind>;

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
    void* operator new(size_t, Module*);

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
public:
    BindingDecl(Location location, std::string name)
        : NamedDecl(Kind::DeclBinding, location, name) {}

    static bool classof(Statement* statement) { return statement->kind() == Kind::DeclBinding; }
};

class FunctionDecl : public NamedDecl {
public:
    FunctionDecl(Location location, std::string name)
        : NamedDecl(Kind::DeclFunction, location, name) {}

    static bool classof(Statement* statement) { return statement->kind() == Kind::DeclFunction; }
};

class StructDecl : public NamedDecl {
public:
    StructDecl(Location location, std::string name)
        : NamedDecl(Kind::DeclStruct, location, name) {}

    static bool classof(Statement* statement) { return statement->kind() == Kind::DeclStruct; }
};

class EnumDecl : public NamedDecl {
public:
    EnumDecl(Location location, std::string name)
        : NamedDecl(Kind::DeclEnum, location, name) {}

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
    void* operator new(size_t, Module*);

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
