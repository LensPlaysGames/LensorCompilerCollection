#ifndef LAYE_AST_HH
#define LAYE_AST_HH

#include "lcc/location.hh"

#include <lcc/utils.hh>

namespace lcc::laye {
class ModuleHeader;
class Decl;

class Module {
    std::vector<ModuleHeader*> _headers;
    std::vector<Decl*> _topLevelDecls;
};

class Token {
public:
    enum struct Kind {
    };

private:
    Kind _kind;
    lcc::Location _location;

protected:
    Token(Kind kind, lcc::Location location)
        : _kind(kind), _location(location) {}

public:
    void* operator new(size_t) = delete;
    void* operator new(size_t, Module*);

    auto kind() const { return _kind; }
    auto location() const { return _location; }
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

    lcc::Location _location;

protected:
    Statement(Kind kind, lcc::Location location)
        : _kind(kind), _location(location) {}

public:
    void* operator new(size_t) = delete;
    void* operator new(size_t, Module*);

    auto kind() const { return _kind; }
    auto location() const { return _location; }
};

class Decl : public Statement {
protected:
    Decl(Kind kind, lcc::Location location)
        : Statement(kind, location) {}

public:
    static bool classof(Statement* statement) { return +statement->kind() >= +Kind::DeclBinding and +statement->kind() <= +Kind::DeclForeignImport; }
};

class NamedDecl : public Decl {
    std::string _name;

protected:
    NamedDecl(Kind kind, lcc::Location location, std::string name)
        : Decl(kind, location), _name(std::move(name)) {}

public:
    const std::string& name() const { return _name; }

    static bool classof(Statement* statement) { return +statement->kind() >= +Kind::DeclBinding and +statement->kind() <= +Kind::DeclAlias; }
};

class BindingDecl : public NamedDecl {
public:
    BindingDecl(lcc::Location location, std::string name)
        : NamedDecl(Kind::DeclBinding, location, name) {}

    static bool classof(Statement* statement) { return statement->kind() == Kind::DeclBinding; }
};

class FunctionDecl : public NamedDecl {
public:
    FunctionDecl(lcc::Location location, std::string name)
        : NamedDecl(Kind::DeclFunction, location, name) {}

    static bool classof(Statement* statement) { return statement->kind() == Kind::DeclFunction; }
};

class StructDecl : public NamedDecl {
public:
    StructDecl(lcc::Location location, std::string name)
        : NamedDecl(Kind::DeclStruct, location, name) {}

    static bool classof(Statement* statement) { return statement->kind() == Kind::DeclStruct; }
};

class EnumDecl : public NamedDecl {
public:
    EnumDecl(lcc::Location location, std::string name)
        : NamedDecl(Kind::DeclEnum, location, name) {}

    static bool classof(Statement* statement) { return statement->kind() == Kind::DeclEnum; }
};

class AliasDecl : public NamedDecl {
public:
    AliasDecl(lcc::Location location, std::string name)
        : NamedDecl(Kind::DeclAlias, location, name) {}

    static bool classof(Statement* statement) { return statement->kind() == Kind::DeclAlias; }
};

/// @brief Base class for file header nodes.
class ModuleHeader : public Decl {
protected:
    ModuleHeader(Kind kind, lcc::Location location)
        : Decl(kind, location) {}

public:
    static bool classof(Statement* statement) { return +statement->kind() >= +Kind::DeclImport and +statement->kind() <= +Kind::DeclForeignImport; }
};

class ImportHeader : public ModuleHeader {
public:
    ImportHeader(lcc::Location location)
        : ModuleHeader(Kind::DeclImport, location) {}

    static bool classof(Statement* statement) { return statement->kind() == Kind::DeclImport; }
};

class ForeignImportHeader : public ModuleHeader {
public:
    ForeignImportHeader(lcc::Location location)
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

    lcc::Location _location;

protected:
    Expr(Kind kind, lcc::Location location)
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
    Unary(lcc::Location location, Expr* value)
        : Expr(Kind::Unary, location), _value(value) {}

    auto value() const { return _value; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Unary; }
};

/// @brief Base class for type syntax nodes (which we're trying to make also Exprs.)
class Type : public Expr {
protected:
    Type(Kind kind, lcc::Location location)
        : Expr(kind, location) {}

    static bool classof(Expr* expr) { return +expr->kind() >= +Kind::TypeInfer && +expr->kind() <= +Kind::TypeC; }
};
} // namespace lcc::laye

#endif // LAYE_AST_HH
