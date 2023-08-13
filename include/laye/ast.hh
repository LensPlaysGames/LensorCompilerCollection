#ifndef LAYE_AST_HH
#define LAYE_AST_HH

#include "lcc/utils/result.hh"
#include "lcc/utils/rtti.hh"

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
class Parser;

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
    Eof,

    Tilde,
    Bang,
    Percent,
    Ampersand,
    Star,
    OpenParen,
    CloseParen,
    Minus,
    Equal,
    Plus,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,
    Pipe,
    SemiColon,
    Colon,
    Comma,
    Less,
    Greater,
    Dot,
    Slash,
    Question,

    Ident,
    LitInt,
    LitFloat,
    LitString,
    LitRune,

    LessLess,
    GreaterGreater,
    EqualEqual,
    BangEqual,
    PlusEqual,
    MinusEqual,
    SlashEqual,
    StarEqual,
    PercentEqual,
    LessEqual,
    GreaterEqual,
    AmpersandEqual,
    PipeEqual,
    TildeEqual,
    LessLessEqual,
    GreaterGreaterEqual,
    EqualGreater,
    ColonColon,

    Bool,
    Int,
    UInt,
    Float,

    // Context,

    True,
    False,
    Nil,
    Global,

    If,
    Then,
    Else,
    For,
    Do,
    Switch,
    Case,
    Default,
    Return,
    Break,
    Continue,
    Goto,

    Struct,
    Variant,
    Enum,
    Alias,
    // Test,
    Import,
    Export,
    Operator,
    Readonly,
    Writeonly,

    New,
    Delete,
    Cast,
    Try,
    Catch,
    // Discard,
    Sizeof,
    Alignof,
    Offsetof,
    Not,
    And,
    Or,
    Xor,
    Varargs,
    Const,
    Foreign,
    Inline,
    Callconv,
    // Impure,

    Void,
    Var,
    Noreturn,
    Rawptr,
    String,

    CChar,
    CSChar,
    CUChar,
    CString,
    CShort,
    CUShort,
    CInt,
    CUInt,
    CLong,
    CULong,
    CLongLong,
    CULongLong,
    CSizeT,
    CISizeT,
    CPtrDiffT,
    CFloat,
    CDouble,
    CLongDouble,
    CBool,
};

using LayeToken = syntax::Token<TokenKind>;

enum struct OperatorKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Greater,
    Less,
    Equal,
    NotEqual,

    Compl,
    And,
    Or,
    Xor,
    Lsh,
    Rsh,

    AddEqual,
    SubEqual,
    DivEqual,
    MulEqual,
    ModEqual,
    LessEqual,
    GreaterEqual,
    AndEqual,
    OrEqual,
    XorEqual,
    LshEqual,
    RshEqual,

    // Invoke,
    Index,
};

struct DeclModifier {
    TokenKind decl_kind{};
    std::string string_value{};
    Expr* expr_value{};
};

struct TemplateParam {
    std::string name;
    Type* value_type;

    bool is_value_param() const { return value_type != nullptr; }
};

struct FunctionParam {
    Type* type;
    std::string name;
    Expr* init;
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

struct EnumVariant {
    std::string name;
    Expr* init;
};

enum struct TypeAccess {
    Default,
    ReadOnly,
    WriteOnly,
};

class Scope {
    Scope* _parent;
    StringMap<Expr*> _symbols;

public:
    Scope(Scope* parent)
        : _parent(parent) {}

    /// Disallow creating scopes without a module reference.
    void* operator new(size_t) = delete;
    void* operator new(size_t sz, Parser& parser);

    auto declare(
        const Context* ctx,
        std::string&& name,
        Expr* expr
    ) -> Result<Expr*>;

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

        // Simple Statements
        Block,
        Assign,
        Delete,
        Expr,

        // Control Flow
        If,
        For,
        ForEach,
        DoFor,
        Switch,
        Return,
        Break,
        Continue,
        Fallthrough,
        Defer,
        Goto,
    };

private:
    const Kind _kind;

    Location _location;

protected:
    Statement(Kind kind, Location location)
        : _kind(kind), _location(location) {}

public:
    void* operator new(size_t) = delete;
    void* operator new(size_t sz, Parser& parser);

    auto kind() const { return _kind; }
    auto location() const { return _location; }
};

/// @brief Base class for expression syntax nodes.
class Expr {
public:
    enum struct Kind {
        Unary,
        Binary,
        And,
        Or,
        Xor,

        UnwrapNilable,

        // Lookups etc.
        LookupName,
        LookupPath,
        FieldIndex,
        ValueIndex,
        Slice,
        Call,
        Ctor,

        // Keyword Prefixed
        Not,
        Cast,
        New,
        Try,
        Catch,
        Do,
        Sizeof,
        Offsetof,
        Alignof,

        // Literals
        LitNil,
        LitBool,
        LitString,
        LitInt,
        LitFloat,

        // Types
        TypeInfer,
        TypeErrUnion,

        TypeLookupName,
        TypeLookupPath,

        TypeArray,
        TypeSlice,
        TypePointer,
        TypeBuffer,

        TypeFunc,

        TypeNoreturn,
        TypeRawptr,
        TypeVoid,
        TypeString,
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
    void* operator new(size_t sz, Parser& parser);

    auto kind() const { return _kind; }
    auto location() const { return _location; }
};

class Decl : public Statement {
protected:
    Decl(Kind kind, Location location)
        : Statement(kind, location) {}

public:
    static bool classof(Statement* statement) { return +statement->kind() >= +Kind::DeclBinding and +statement->kind() <= +Kind::DeclImport; }
};

class NamedDecl : public Decl {
    std::vector<DeclModifier> _mods;
    std::string _name;
    std::vector<TemplateParam> _template_params{};

protected:
    NamedDecl(Kind kind, Location location, std::vector<DeclModifier> mods, std::string name)
        : Decl(kind, location), _mods(std::move(mods)), _name(std::move(name)) {}

    NamedDecl(Kind kind, Location location, std::vector<DeclModifier> mods, std::string name, std::vector<TemplateParam> template_params)
        : Decl(kind, location), _mods(std::move(mods)), _name(std::move(name)), _template_params(std::move(template_params)) {}

public:
    auto mods() const -> std::span<DeclModifier const> { return _mods; }
    auto name() const -> const std::string& { return _name; }
    auto template_params() const -> std::span<TemplateParam const> { return _template_params; }

    static bool classof(Statement* statement) { return +statement->kind() >= +Kind::DeclBinding and +statement->kind() <= +Kind::DeclAlias; }
};

class BindingDecl : public NamedDecl {
    Type* _type;
    Expr* _init;

public:
    BindingDecl(Location location, std::vector<DeclModifier> mods, Type* type, std::string name, Expr* init)
        : NamedDecl(Kind::DeclBinding, location, mods, name), _type(type), _init(init) {}

    auto type() const { return _type; }
    auto init() const { return _init; }

    static bool classof(Statement* statement) { return statement->kind() == Kind::DeclBinding; }
};

class FunctionDecl : public NamedDecl {
    Type* _returnType;
    std::vector<FunctionParam> _params;
    Statement* _body;

public:
    FunctionDecl(Location location, std::vector<DeclModifier> mods, Type* returnType, std::string name, std::vector<TemplateParam> template_params, std::vector<FunctionParam> params, Statement* body)
        : NamedDecl(Kind::DeclFunction, location, mods, name, template_params), _returnType(returnType), _params(std::move(params)), _body(body) {}

    auto return_type() const { return _returnType; }
    auto params() const -> std::span<FunctionParam const> { return _params; }
    auto body() const { return _body; }

    static bool classof(Statement* statement) { return statement->kind() == Kind::DeclFunction; }
};

class StructDecl : public NamedDecl {
    std::vector<StructField> _fields;
    std::vector<StructVariant> _variants;

public:
    StructDecl(Location location, std::vector<DeclModifier> mods, std::string name, std::vector<TemplateParam> template_params, std::vector<StructField> fields, std::vector<StructVariant> variants)
        : NamedDecl(Kind::DeclStruct, location, mods, name, template_params), _fields(std::move(fields)), _variants(std::move(variants)) {}

    auto fields() const -> std::span<StructField const> { return _fields; }
    auto variants() const -> std::span<StructVariant const> { return _variants; }

    static bool classof(Statement* statement) { return statement->kind() == Kind::DeclStruct; }
};

class EnumDecl : public NamedDecl {
    Type* _underlying_type;
    std::vector<EnumVariant> _variants;

public:
    EnumDecl(Location location, std::vector<DeclModifier> mods, std::string name, Type* underlying_type, std::vector<EnumVariant> variants)
        : NamedDecl(Kind::DeclEnum, location, mods, name), _underlying_type(underlying_type), _variants(std::move(variants)) {}

    auto underlying_type() const { return _underlying_type; }
    auto variants() const -> std::span<EnumVariant const> { return _variants; }

    static bool classof(Statement* statement) { return statement->kind() == Kind::DeclEnum; }
};

class AliasDecl : public NamedDecl {
    Type* _type;

public:
    AliasDecl(Location location, std::vector<DeclModifier> mods, std::string name, Type* type)
        : NamedDecl(Kind::DeclAlias, location, mods, name), _type(type) {}

    auto type() const { return _type; }

    static bool classof(Statement* statement) { return statement->kind() == Kind::DeclAlias; }
};

/// @brief Base class for file header nodes.
class ModuleHeader : public Decl {
protected:
    ModuleHeader(Kind kind, Location location)
        : Decl(kind, location) {}

public:
    static bool classof(Statement* statement) { return +statement->kind() >= +Kind::DeclImport and +statement->kind() <= +Kind::DeclImport; }
};

// import "file";
// import * from "file";
// import foo, bar from "file";
// import "file" as file;
class ImportHeader : public ModuleHeader {
    bool _exported;
    std::string _import_name;
    bool _is_wildcard = false;
    std::vector<std::string> _import_names{};
    std::string _alias;

public:
    ImportHeader(Location location, bool exported, std::string import_name, bool is_wildcard = false, std::string alias = {})
        : ModuleHeader(Kind::DeclImport, location), _exported(exported), _import_name(import_name), _is_wildcard(is_wildcard), _alias(std::move(alias)) {}

    ImportHeader(Location location, bool exported, std::string import_name, std::vector<std::string> import_names, std::string alias = {})
        : ModuleHeader(Kind::DeclImport, location), _exported(exported), _import_name(import_name), _import_names(std::move(import_names)), _alias(std::move(alias)) {}

    bool exported() const { return _exported; }
    auto import_name() const -> const std::string& { return _import_name; }
    bool is_wildcard() const { return _is_wildcard; }
    bool has_import_names() const { return not _import_names.empty(); }
    auto import_names() const -> std::span<std::string const> { return _import_names; }
    auto alias() const -> const std::string& { return _alias; }

    static bool classof(Statement* statement) { return statement->kind() == Kind::DeclImport; }
};

class BlockStatement : public Statement {
    std::vector<Statement*> _children;

public:
    BlockStatement(Location location, std::vector<Statement*> children)
        : Statement(Kind::Block, location), _children(std::move(children)) {}

    auto children() const -> std::span<Statement* const> { return _children; }

    static bool classof(Statement* statement) { return statement->kind() == Kind::Block; }
};

class AssignStatement : public Statement {
    Expr* _target;
    Expr* _value;

public:
    AssignStatement(Location location, Expr* target, Expr* value)
        : Statement(Kind::Assign, location), _target(target), _value(value) {}

    auto target() const { return _target; }
    auto value() const { return _value; }

    static bool classof(Statement* statement) { return statement->kind() == Kind::Assign; }
};

class DeleteStatement : public Statement {
    Expr* _expr;

public:
    DeleteStatement(Expr* expr)
        : Statement(Kind::Delete, expr->location()), _expr(expr) {}

    auto expr() const { return _expr; }

    static bool classof(Statement* statement) { return statement->kind() == Kind::Delete; }
};

class ExprStatement : public Statement {
    Expr* _expr;

public:
    ExprStatement(Expr* expr)
        : Statement(Kind::Expr, expr->location()), _expr(expr) {}

    auto expr() const { return _expr; }

    static bool classof(Statement* statement) { return statement->kind() == Kind::Assign; }
};

class IfStatement : public Statement {
    std::string _label{};

    Expr* _condition;
    Statement* _pass;
    Statement* _fail;

public:
    IfStatement(Location location, Expr* condition, Statement* pass, Statement* fail)
        : Statement(Kind::If, location), _condition(condition), _pass(pass), _fail(fail) {}

    bool has_label() const { return not _label.empty(); }
    auto label() const -> std::string_view { return _label; }
    void set_label(std::string label) { _label = std::move(label); }

    auto condition() const { return _condition; }
    auto pass() const { return _pass; }
    auto fail() const { return _fail; }

    static bool classof(Statement* statement) { return statement->kind() == Kind::If; }
};

class ForStatement : public Statement {
    std::string _label{};

    Statement* _init;
    Expr* _condition;
    Expr* _increment;

    Statement* _pass;
    Statement* _fail;

public:
    ForStatement(Location location, Expr* condition, Statement* pass, Statement* fail)
        : Statement(Kind::For, location), _init(nullptr), _condition(condition), _increment(nullptr), _pass(pass), _fail(fail) {}

    ForStatement(Location location, Statement* init, Expr* condition, Expr* increment, Statement* pass, Statement* fail)
        : Statement(Kind::For, location), _init(init), _condition(condition), _increment(increment), _pass(pass), _fail(fail) {}

    bool has_label() const { return not _label.empty(); }
    auto label() const -> std::string_view { return _label; }
    void set_label(std::string label) { _label = std::move(label); }

    auto init() const { return _init; }
    auto condition() const { return _condition; }
    auto increment() const { return _increment; }

    auto pass() const { return _pass; }
    auto fail() const { return _fail; }

    static bool classof(Statement* statement) { return statement->kind() == Kind::For; }
};

class ForEachStatement : public Statement {
    std::string _label{};

    Type* _type;
    std::string _name;
    Expr* _sequence;

    Statement* _pass;
    Statement* _fail;

public:
    ForEachStatement(Location location, Type* type, std::string name, Expr* sequence, Statement* pass, Statement* fail)
        : Statement(Kind::ForEach, location), _type(type), _name(name), _sequence(sequence), _pass(pass), _fail(fail) {}

    bool has_label() const { return not _label.empty(); }
    auto label() const -> std::string_view { return _label; }
    void set_label(std::string label) { _label = std::move(label); }

    auto type() const { return _type; }
    auto name() const -> std::string_view { return _name; }
    auto sequence() const { return _sequence; }

    auto pass() const { return _pass; }
    auto fail() const { return _fail; }

    static bool classof(Statement* statement) { return statement->kind() == Kind::ForEach; }
};

class DoForStatement : public Statement {
    std::string _label{};

    Expr* _condition;
    Statement* _body;

public:
    DoForStatement(Location location, Expr* condition, Statement* body)
        : Statement(Kind::DoFor, location), _condition(condition), _body(body) {}

    bool has_label() const { return not _label.empty(); }
    auto label() const -> std::string_view { return _label; }
    void set_label(std::string label) { _label = std::move(label); }

    auto condition() const { return _condition; }
    auto body() const { return _body; }

    static bool classof(Statement* statement) { return statement->kind() == Kind::DoFor; }
};

class FallthroughStatement : public Statement {
public:
    FallthroughStatement(Location location)
        : Statement(Kind::Fallthrough, location) {}

    static bool classof(Statement* statement) { return statement->kind() == Kind::Fallthrough; }
};

struct SwitchCase {
    Expr* value;
    Statement* body;

    bool is_default() const { return value == nullptr; }
    bool is_fallthrough() const { return is<FallthroughStatement>(body); }
};

class SwitchStatement : public Statement {
    Expr* _target;
    std::vector<SwitchCase> _cases;

public:
    SwitchStatement(Location location, Expr* target, std::vector<SwitchCase> cases)
        : Statement(Kind::Switch, location), _target(target), _cases(std::move(cases)) {}

    auto target() const { return _target; }
    auto cases() const -> std::span<SwitchCase const> { return _cases; }

    static bool classof(Statement* statement) { return statement->kind() == Kind::Switch; }
};

class ReturnStatement : public Statement {
    Expr* _value;

public:
    ReturnStatement(Location location, Expr* value)
        : Statement(Kind::Return, location), _value(value) {}

    auto value() const { return _value; }

    static bool classof(Statement* statement) { return statement->kind() == Kind::Return; }
};

class BreakStatement : public Statement {
    std::string _target{};

public:
    BreakStatement(Location location)
        : Statement(Kind::Break, location) {}

    BreakStatement(Location location, std::string target)
        : Statement(Kind::Continue, location), _target(std::move(target)) {}

    bool has_target() const { return not _target.empty(); }
    auto target() const -> std::string_view { return _target; }

    static bool classof(Statement* statement) { return statement->kind() == Kind::Break; }
};

class ContinueStatement : public Statement {
    std::string _target{};

public:
    ContinueStatement(Location location)
        : Statement(Kind::Continue, location) {}

    ContinueStatement(Location location, std::string target)
        : Statement(Kind::Continue, location), _target(std::move(target)) {}

    bool has_target() const { return not _target.empty(); }
    auto target() const -> std::string_view { return _target; }

    static bool classof(Statement* statement) { return statement->kind() == Kind::Continue; }
};

class DeferStatement : public Statement {
    Statement* _statement;

public:
    DeferStatement(Location location, Statement* statement)
        : Statement(Kind::Defer, location), _statement(statement) {}

    auto statement() const { return _statement; }

    static bool classof(Statement* statement) { return statement->kind() == Kind::Defer; }
};

class GotoStatement : public Statement {
    std::string _target{};

public:
    GotoStatement(Location location, std::string target)
        : Statement(Kind::Continue, location), _target(std::move(target)) {}

    auto target() const -> std::string_view { return _target; }

    static bool classof(Statement* statement) { return statement->kind() == Kind::Goto; }
};

class UnaryExpr : public Expr {
    OperatorKind _operator_kind;
    Expr* _value;

public:
    UnaryExpr(Location location, OperatorKind operator_kind, Expr* value)
        : Expr(Kind::Unary, location), _operator_kind(operator_kind), _value(value) {}

    auto operator_kind() const { return _operator_kind; }
    auto value() const { return _value; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Unary; }
};

class BinaryExpr : public Expr {
    OperatorKind _operator_kind;
    Expr* _lhs;
    Expr* _rhs;

public:
    BinaryExpr(Location location, OperatorKind operator_kind, Expr* lhs, Expr* rhs)
        : Expr(Kind::Binary, location), _operator_kind(operator_kind), _lhs(lhs), _rhs(rhs) {}

    auto operator_kind() const { return _operator_kind; }
    auto lhs() const { return _lhs; }
    auto rhs() const { return _rhs; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Binary; }
};

class AndExpr : public Expr {
    Expr* _lhs;
    Expr* _rhs;

public:
    AndExpr(Location location, Expr* lhs, Expr* rhs)
        : Expr(Kind::And, location), _lhs(lhs), _rhs(rhs) {}

    auto lhs() const { return _lhs; }
    auto rhs() const { return _rhs; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::And; }
};

class OrExpr : public Expr {
    Expr* _lhs;
    Expr* _rhs;

public:
    OrExpr(Location location, Expr* lhs, Expr* rhs)
        : Expr(Kind::Or, location), _lhs(lhs), _rhs(rhs) {}

    auto lhs() const { return _lhs; }
    auto rhs() const { return _rhs; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Or; }
};

class XorExpr : public Expr {
    Expr* _lhs;
    Expr* _rhs;

public:
    XorExpr(Location location, Expr* lhs, Expr* rhs)
        : Expr(Kind::Xor, location), _lhs(lhs), _rhs(rhs) {}

    auto lhs() const { return _lhs; }
    auto rhs() const { return _rhs; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Xor; }
};

class UnwrapNilableExpr : public Expr {
    Expr* _value;

public:
    UnwrapNilableExpr(Location location, Expr* value)
        : Expr(Kind::UnwrapNilable, location), _value(value) {}

    auto value() const { return _value; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::UnwrapNilable; }
};

class NameExpr : public Expr {
    std::string _name{};

public:
    NameExpr(Location location, std::string name)
        : Expr(Kind::LookupName, location), _name(std::move(name)) {}

    auto name() const -> std::string_view { return _name; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::LookupName; }
};

class PathExpr : public Expr {
    bool _is_headless;
    std::vector<std::string> _names;

public:
    PathExpr(Location location, bool is_headless, std::vector<std::string> names)
        : Expr(Kind::LookupPath, location), _is_headless(is_headless), _names(std::move(names)) {}

    bool is_headless() const { return _is_headless; }
    auto name() const -> std::span<std::string const> { return _names; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::LookupPath; }
};

class FieldIndexExpr : public Expr {
    Expr* _target;
    std::string _field_name;

public:
    FieldIndexExpr(Location location, Expr* target, std::string field_name)
        : Expr(Kind::FieldIndex, location), _target(target), _field_name(field_name) {}

    auto target() const { return _target; }
    auto field_name() const -> std::string_view { return _field_name; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::FieldIndex; }
};

class ValueIndexExpr : public Expr {
    Expr* _target;
    std::vector<Expr*> _indices;

public:
    ValueIndexExpr(Location location, Expr* target, std::vector<Expr*> indices)
        : Expr(Kind::ValueIndex, location), _target(target), _indices(std::move(indices)) {}

    auto target() const { return _target; }
    auto indices() const -> std::span<Expr* const> { return _indices; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::ValueIndex; }
};

class SliceExpr : public Expr {
    Expr* _target;
    Expr* _offset;
    Expr* _length;

public:
    SliceExpr(Location location, Expr* target, Expr* offset, Expr* length)
        : Expr(Kind::Slice, location), _target(target), _offset(offset), _length(length) {}

    auto target() const { return _target; }
    auto offset() const { return _offset; }
    auto length() const { return _length; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Slice; }
};

class CallExpr : public Expr {
    Expr* _target;
    std::vector<Expr*> _args;

public:
    CallExpr(Location location, Expr* target, std::vector<Expr*> args)
        : Expr(Kind::Call, location), _target(target), _args(std::move(args)) {}

    auto target() const { return _target; }
    auto args() const { return _args; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Call; }
};

struct CtorFieldInit {
    std::string field_name{};
    Location location{};
    Expr* value{};
};

class CtorExpr : public Expr {
    Type* _type;
    std::vector<CtorFieldInit> _inits;

public:
    CtorExpr(Location location, Type* type, std::vector<CtorFieldInit> inits)
        : Expr(Kind::Ctor, location), _type(type), _inits(std::move(inits)) {}

    auto type() const { return _type; }
    auto inits() const -> std::span<CtorFieldInit const> { return _inits; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Ctor; }
};

class NotExpr : public Expr {
    Expr* _value;

public:
    NotExpr(Location location, Expr* value)
        : Expr(Kind::Not, location), _value(value) {}

    auto value() const { return _value; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Not; }
};

class CastExpr : public Expr {
    Type* _type;
    Expr* _value;

public:
    CastExpr(Location location, Type* type, Expr* value)
        : Expr(Kind::Cast, location), _type(type), _value(value) {}

    auto type() const { return _type; }
    auto value() const { return _value; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Cast; }
};

class NewExpr : public Expr {
    Expr* _alloc;
    Type* _type;
    std::vector<CtorFieldInit> _inits;

public:
    NewExpr(Location location, Type* type, std::vector<CtorFieldInit> inits)
        : Expr(Kind::New, location), _alloc(nullptr), _type(type), _inits(std::move(inits)) {}

    NewExpr(Location location, Expr* alloc, Type* type, std::vector<CtorFieldInit> inits)
        : Expr(Kind::New, location), _alloc(alloc), _type(type), _inits(std::move(inits)) {}

    bool has_alloc() const { return _alloc != nullptr; }
    auto alloc() const { return _alloc; }
    auto type() const { return _type; }
    auto inits() const -> std::span<CtorFieldInit const> { return _inits; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::New; }
};

class TryExpr : public Expr {
    Expr* _value;

public:
    TryExpr(Location location, Expr* value)
        : Expr(Kind::Try, location), _value(value) {}

    auto value() const { return _value; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Try; }
};

class CatchExpr : public Expr {
    Expr* _value;
    std::string _error_name;
    Statement* _body;

public:
    CatchExpr(Location location, Expr* value, std::string error_name, Statement* body)
        : Expr(Kind::Catch, location), _value(value), _error_name(std::move(error_name)), _body(body) {}

    auto value() const { return _value; }
    auto error_name() const -> std::string_view { return _error_name; }
    auto body() const { return _body; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Catch; }
};

//
// NOTE(local): Laye monadic types need the same functions that all monads do, and
//  they will look something like this:
//
// // construct the monadic type, dunno what to call it
// M<T> operator return <T>(T t);
//
// // bind (convert from M<A> to M<B>, contained type may change)
// M<B> operator >>= <A, B>(M<A> m, M<B>(A) f);
//
// Do notation could mirror Haskell?
//
// T? operator return <T>(T t) => t; // what would this be called?
// B? operator >>= <A, B>(A? a, B?(A) f) // or just `bind`
// {
//     if a is nil then return nil;
//     return f(a!); // note that a! is only required if we don't have good flow logic
// }
//
// User? get_user(string userId);
// Address? get_user_address(User user);
// StreetAddress? get_street_address(Address address);
//
// return do
//     user = get_user(id)
//     addr = get_user_address(user)
//     get_street_address(addr);
//
// error unions may also be monadic, and have special semantics, but the explicit use of
//  try/catch should probably probably encouraged.
//
// var user = try get_user(id);
// var addr = try get_user_address(user);
// return try get_street_address(addr);
//

class DoExpr : public Expr {
    std::vector<Statement*> _statements;

public:
    DoExpr(Location location, std::vector<Statement*> statements)
        : Expr(Kind::Do, location), _statements(std::move(statements)) {}

    auto statements() const -> std::span<Statement* const> { return _statements; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Do; }
};

class SizeofExpr : public Expr {
    Type* _type;

public:
    SizeofExpr(Location location, Type* type)
        : Expr(Kind::Sizeof, location), _type(type) {}

    auto type() const { return _type; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Sizeof; }
};

class OffsetofExpr : public Expr {
    Type* _type;
    std::string _field_name;

public:
    OffsetofExpr(Location location, Type* type, std::string field_name)
        : Expr(Kind::Offsetof, location), _type(type), _field_name(std::move(field_name)) {}

    auto type() const { return _type; }
    auto field_name() const -> const std::string& { return _field_name; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Offsetof; }
};

class AlignofExpr : public Expr {
    Type* _type;
    std::string _field_name;

public:
    AlignofExpr(Location location, Type* type, std::string field_name)
        : Expr(Kind::Alignof, location), _type(type), _field_name(std::move(field_name)) {}

    auto type() const { return _type; }
    auto field_name() const -> const std::string& { return _field_name; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::Alignof; }
};

class LitNilExpr : public Expr {
public:
    LitNilExpr(Location location)
        : Expr(Kind::LitNil, location) {}

    static bool classof(Expr* expr) { return expr->kind() == Kind::LitNil; }
};

class LitBoolExpr : public Expr {
    bool _value;

public:
    LitBoolExpr(Location location, bool value)
        : Expr(Kind::LitBool, location), _value(value) {}

    auto value() const { return _value; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::LitBool; }
};

class LitStringExpr : public Expr {
    std::string _value;

public:
    LitStringExpr(Location location, std::string value)
        : Expr(Kind::LitString, location), _value(std::move(value)) {}

    auto value() const -> std::string_view { return _value; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::LitString; }
};

class LitIntExpr : public Expr {
    i64 _value;

public:
    LitIntExpr(Location location, i64 value)
        : Expr(Kind::LitInt, location), _value(value) {}

    auto value() const { return _value; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::LitInt; }
};

class LitFloatExpr : public Expr {
    long double _value;

public:
    LitFloatExpr(Location location, long double value)
        : Expr(Kind::LitFloat, location), _value(value) {}

    auto value() const { return _value; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::LitFloat; }
};

/// @brief Base class for type syntax nodes (which we're trying to make also Exprs.)
class Type : public Expr {
protected:
    Type(Kind kind, Location location)
        : Expr(kind, location) {}

    static bool classof(Expr* expr) { return +expr->kind() >= +Kind::TypeInfer && +expr->kind() <= +Kind::TypeC; }
};

class InferType : public Type {
public:
    InferType(Location location)
        : Type(Kind::TypeInfer, location) {}

    static bool classof(Expr* expr) { return expr->kind() == Kind::TypeInfer; }
};

// ErrName!ValueType
class ErrUnionType : public Type {
    std::string _error_name{};
    Type* _value_type;

public:
    ErrUnionType(Location location, std::string error_name, Type* value_type)
        : Type(Kind::TypeErrUnion, location), _error_name(std::move(error_name)), _value_type(value_type) {}

    bool has_error_name() const { return not _error_name.empty(); }
    auto error_name() const -> const std::string& { return _error_name; }
    auto value_type() const { return _value_type; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::TypeErrUnion; }
};

class NameType : public Type {
    std::string _name;

public:
    NameType(Location location, std::string name)
        : Type(Kind::TypeLookupName, location), _name(std::move(name)) {}

    auto name() const -> const std::string& { return _name; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::TypeLookupName; }
};

class PathType : public Type {
    bool _is_headless;
    std::vector<std::string> _path;

public:
    PathType(Location location, bool is_headless, std::vector<std::string> path)
        : Type(Kind::TypeLookupPath, location), _is_headless(is_headless), _path(std::move(path)) {}

    auto is_headless() const { return _is_headless; }
    auto path() const -> const std::span<std::string const> { return _path; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::TypeLookupPath; }
};

class SingleElementType : public Type {
    Type* _elem_type;

public:
    SingleElementType(Kind kind, Location location, Type* elem_type)
        : Type(kind, location), _elem_type(elem_type) {}

    auto elem_type() const { return _elem_type; }

    static bool classof(Expr* expr) { return +expr->kind() >= +Kind::TypeArray and +expr->kind() <= +Kind::TypeBuffer; }
};

class ArrayType : public SingleElementType {
    std::vector<Expr*> _rank_lengths;

public:
    ArrayType(Location location, Type* elem_type, std::vector<Expr*> rank_lengths)
        : SingleElementType(Kind::TypeArray, location, elem_type), _rank_lengths(std::move(rank_lengths)) {}

    auto rank_lengths() const -> std::span<Expr* const> { return _rank_lengths; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::TypeArray; }
};

class SliceType : public SingleElementType {
public:
    SliceType(Location location, Type* elem_type)
        : SingleElementType(Kind::TypeSlice, location, elem_type) {}

    static bool classof(Expr* expr) { return expr->kind() == Kind::TypeSlice; }
};

class PointerType : public SingleElementType {
public:
    PointerType(Location location, Type* elem_type)
        : SingleElementType(Kind::TypePointer, location, elem_type) {}

    static bool classof(Expr* expr) { return expr->kind() == Kind::TypePointer; }
};

class BufferType : public SingleElementType {
public:
    BufferType(Location location, Type* elem_type)
        : SingleElementType(Kind::TypeBuffer, location, elem_type) {}

    static bool classof(Expr* expr) { return expr->kind() == Kind::TypeBuffer; }
};

class FuncType : public Type {
    Expr* _calling_convention{};
    Type* _return_type;
    std::vector<Expr*> _param_types;

public:
    FuncType(Location location, Type* return_type, std::vector<Expr*> param_types)
        : Type(Kind::TypeFunc, location), _return_type(return_type), _param_types(std::move(param_types)) {}

    FuncType(Location location, Expr* calling_convention, Type* return_type, std::vector<Expr*> param_types)
        : Type(Kind::TypeFunc, location), _calling_convention(calling_convention), _return_type(return_type), _param_types(std::move(param_types)) {}

    auto calling_convention() const { return _calling_convention; }
    auto return_type() const { return _return_type; }
    auto param_types() const -> std::span<Expr* const> { return _param_types; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::TypeFunc; }
};

class NoreturnType : public Type {
public:
    NoreturnType(Location location)
        : Type(Kind::TypeNoreturn, location) {}

    static bool classof(Expr* expr) { return expr->kind() == Kind::TypeNoreturn; }
};

class RawptrType : public Type {
public:
    RawptrType(Location location)
        : Type(Kind::TypeRawptr, location) {}

    static bool classof(Expr* expr) { return expr->kind() == Kind::TypeRawptr; }
};

class VoidType : public Type {
public:
    VoidType(Location location)
        : Type(Kind::TypeVoid, location) {}

    static bool classof(Expr* expr) { return expr->kind() == Kind::TypeVoid; }
};

class StringType : public Type {
    TypeAccess _access;

public:
    StringType(Location location, TypeAccess access = TypeAccess::Default)
        : Type(Kind::TypeString, location), _access(access) {}

    auto access() const { return _access; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::TypeString; }
};

class SizableType : public Type {
    int _size;

protected:
    SizableType(Kind kind, Location location, int size)
        : Type(kind, location), _size(size) {}

    bool is_sized() const { return _size > 0; }
    int size() const { return _size; }

public:
    static bool classof(Expr* expr) { return +expr->kind() >= +Kind::TypeBool and +expr->kind() <= +Kind::TypeFloat; }
};

class BoolType : public SizableType {
public:
    BoolType(Location location, int size = 0)
        : SizableType(Kind::TypeBool, location, size) {}

    static bool classof(Expr* expr) { return expr->kind() == Kind::TypeBool; }
};

class IntType : public SizableType {
    bool _is_signed;

public:
    IntType(Location location, bool is_signed, int size = 0)
        : SizableType(Kind::TypeInt, location, size), _is_signed(is_signed) {}

    bool is_signed() const { return _is_signed; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::TypeInt; }
};

class FloatType : public SizableType {
public:
    FloatType(Location location, int size = 0)
        : SizableType(Kind::TypeFloat, location, size) {}

    static bool classof(Expr* expr) { return expr->kind() == Kind::TypeFloat; }
};

class CType : public Type {
    TokenKind _kind;
    TypeAccess _access;

public:
    CType(Location location, TokenKind kind)
        : Type(Kind::TypeC, location), _kind(kind) {
        LCC_ASSERT(+kind >= +TokenKind::CChar && +kind <= +TokenKind::CBool);
    }

    CType(Location location, TokenKind kind, TypeAccess access)
        : Type(Kind::TypeC, location), _kind(kind), _access(access) {
        LCC_ASSERT(+kind >= +TokenKind::CChar && +kind <= +TokenKind::CBool);
    }

    auto kind() const { return _kind; }
    auto access() const { return _access; }

    static bool classof(Expr* expr) { return expr->kind() == Kind::TypeC; }
};
} // namespace lcc::laye

#endif // LAYE_AST_HH
