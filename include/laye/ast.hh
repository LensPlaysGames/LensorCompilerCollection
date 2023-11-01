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
class Module;
class SemaNode;
class Statement;
class Decl;
class NamedDecl;
class Expr;
class Type;
class Symbol;
class Scope;
class Parser;
struct FunctionParam;

/// State of semantic analysis for an expression or type.
enum struct SemaState {
    /// Not yet analysed by sema. The type of this node is unspecified.
    NotAnalysed,

    /// Sema is currently in progress. This can be used to detect cycles.
    InProgress,

    /// There was an error analysing this expression. Any expression that
    /// depends on the type of this expression should not check it, nor try
    /// to convert it or issue an error about it.
    Errored,

    /// Sema is done with this expression, and there are no errors that could
    /// affect surrounding code. Note that this does not mean that it contains
    /// no errors at all! For instance, a while loop is never marked as \c Errored
    /// even if e.g. its condition is invalid or if there is an error in its body,
    /// simply because a while loop does not return a value and can thus never
    /// affect the types of the surrounding code.
    Done,
};

class LayeContext {
    Context* _context;

    StringMap<Module*> _modules{};

public:
    LayeContext(Context* context)
        : _context(context) {}

    void add_module(std::string abs_name, Module* module) { _modules.emplace(std::move(abs_name), module); }

    auto context() const { return _context; }
    auto lookup_module(std::string abs_name) -> Module* {
        if (auto it = _modules.find(abs_name); it != _modules.end()) {
            return it->second;
        }

        return nullptr;
    }

    auto parse_laye_file(File& file) -> Module*;

    void print_modules();
};

class Module {
public:
    struct Ref {
        std::string name;
        Location location;
        Module* module;
        bool is_exported;

        Ref(std::string name, Location location, Module* module, bool is_exported)
            : name(std::move(name)), location(location), module(module), is_exported(is_exported) {}
    };

private:
    LayeContext* _laye_context;

    File* _file;

    usz unique_name_counter = 0;

    std::vector<ModuleHeader*> _headers{};

    std::vector<Ref> _imports{};
    Scope* _exports{};

    std::vector<Decl*> _top_level_decls{};

    std::vector<SemaNode*> nodes{};
    std::vector<FunctionParam*> params{};
    std::vector<Scope*> scopes{};

    SemaState _state = SemaState::NotAnalysed;

public:
    Module(LayeContext* laye_context, File* file);

    auto laye_context() const { return _laye_context; }
    auto context() const { return _laye_context->context(); }

    auto file() const { return _file; }

    auto add_header(ModuleHeader* header) { _headers.push_back(header); }
    auto add_import(std::string name, Location location, Module* module, bool is_exported) {
        //Diag::Note("Adding an imported module with name '{}'", name);
        _imports.push_back(Ref{std::move(name), location, module, is_exported});
    }
    void add_export(NamedDecl* decl);
    auto add_top_level_decl(Decl* decl) { _top_level_decls.push_back(decl); }

    auto imports() -> std::vector<Ref>& { return _imports; }
    auto lookup_import(const std::string& name, bool is_exported = false) const -> std::optional<Ref> {
        auto it = std::find_if(_imports.begin(), _imports.end(), [name, is_exported](const Ref& m) { return (is_exported ? m.is_exported : true) and m.name == name; });
        if (it == _imports.end())
            return std::nullopt;
        return *it;
    }

    auto exports() { return _exports; }
    auto top_level_decls() -> std::vector<Decl*>& { return _top_level_decls; }

    /// Intern a string and return its index.
    usz intern(std::string_view str);

    /// Get a unique function name.
    auto unique_function_name() -> std::string { return fmt::format("_XLaye__func_{}", unique_name_counter++); }

    /// Get the state of semantic analysis for this node.
    /// \see SemaNode::State
    auto sema_state() const { return _state; }
    /// Check if this expression was successfully analysed by sema.
    bool sema_done() const { return _state == SemaState::Done; }
    /// Check if sema has errored.
    bool sema_errored() const { return _state == SemaState::Errored; }
    /// \see SemaNode::State
    bool sema_done_or_errored() const {
        return _state == SemaState::Done or _state == SemaState::Errored;
    }

    /// \see SemaNode::State
    void set_sema_in_progress() {
        LCC_ASSERT(not sema_done_or_errored());
        _state = SemaState::InProgress;
    }
    /// \see SemaNode::State
    constexpr void set_sema_done() {
        LCC_ASSERT(_state != SemaState::Errored);
        _state = SemaState::Done;
    }
    /// \see SemaNode::State
    void set_sema_errored() {
        LCC_ASSERT(_state != SemaState::Done);
        _state = SemaState::Errored;
    }

    void print();

    friend SemaNode;
    friend FunctionParam;
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
    BoolSized,
    Int,
    IntSized,
    UInt,
    UIntSized,
    Float,
    FloatSized,

    // Context,

    True,
    False,
    Nil,
    Global,

    If,
    Else,
    For,
    Do,
    Switch,
    Case,
    Default,
    Return,
    Break,
    Continue,
    Defer,
    Goto,
    Xyzzy,

    Struct,
    Variant,
    Enum,
    Alias,
    Test,
    Import,
    Export,
    From,
    As,
    Operator,
    Mut,

    New,
    Delete,
    Cast,
    Try,
    Catch,
    Discard,
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
    Impure,
    Nodiscard,

    Void,
    Var,
    Noreturn,
    Rawptr,
    String,
};

using LayeToken = syntax::Token<TokenKind>;

enum struct OperatorKind {
    Invalid,

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

    Assign,
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

    // Call,
    Index,
};

std::string ToString(TokenKind kind);
std::string ToString(OperatorKind kind);

struct DeclModifier {
    Location location;
    TokenKind decl_kind;
    std::string string_value{};
    CallConv call_conv{};
};

struct TemplateParam {
    std::string name;
    Location location;
    Type* value_type;

    bool is_value_param() const { return value_type != nullptr; }
};

struct FunctionParam {
    Type* type;
    std::string name;
    Location location;
    Expr* init;

    void* operator new(size_t) = delete;
    void* operator new(size_t sz, Module& module);
};

struct EnumVariant {
    std::string name;
    Location location;
    Expr* init;
};

enum struct TypeAccess {
    ReadOnly,
    Mutable,
};

enum struct PathKind {
    Default,
    Global,
    Headless,
};

enum struct VarargsKind {
    None,
    Laye,
    C,
};

enum class CastKind {
    HardCast,
    ImplicitCast,
};

class Scope {
    Scope* _parent;
    std::unordered_multimap<std::string, NamedDecl*, detail::StringHash, std::equal_to<>> symbols;
    bool _is_function_scope = false;

public:
    Scope(Scope* parent)
        : _parent(parent) {}

    /// Disallow creating scopes without a module reference.
    void* operator new(size_t) = delete;
    void* operator new(size_t sz, Parser& parser);
    void* operator new(size_t sz, Module& module);

    auto declare(
        Module* module,
        std::string name,
        NamedDecl* expr
    ) -> Result<NamedDecl*>;

    auto parent() const { return _parent; }

    auto find(std::string_view name) { return symbols.equal_range(name); }

    /// Mark this scope as a function scope.
    void set_function_scope() { _is_function_scope = true; }

    bool is_function_scope() const { return _is_function_scope; }

    void debug_print() const {
        fmt::print(">> scope ({})\n", (void*) this);
        for (auto [first, second] : symbols) {
            fmt::print("  {} ({})\n", first, (void*) second);
        }
    }
};

class LitStringExpr;

class EvalResult {
    std::variant< // clang-format off
        i64,
        std::nullptr_t,
        LitStringExpr*,
        std::monostate
    > data; // clang-format on
public:
    EvalResult() : data(std::monostate()) {}
    EvalResult(i64 data) : data(data) {}
    EvalResult(bool data) : data(i64(1)) {}
    EvalResult(std::nullptr_t) : data(nullptr) {}
    EvalResult(LitStringExpr* data) : data(data) {}

    bool is_i64() const { return std::holds_alternative<i64>(data); }
    bool is_null() const { return std::holds_alternative<std::nullptr_t>(data); }
    bool is_string() const { return std::holds_alternative<LitStringExpr*>(data); }

    i64 as_i64() const { return std::get<i64>(data); }
    LitStringExpr* as_string() const { return std::get<LitStringExpr*>(data); }
};

class SemaNode {
public:
    enum struct Kind {
        Statement,
        Expr,
    };

private:
    const Kind _kind;

    Location _location;

    /// sema fields
    SemaState _state = SemaState::NotAnalysed;

protected:
    constexpr SemaNode(Kind kind, Location location)
        : _kind(kind), _location(location) {}

public:
    bool is_statement() const { return _kind == Kind::Statement; }
    bool is_expr() const { return _kind == Kind::Expr; }

    auto location() const { return _location; }

    /// Get the state of semantic analysis for this node.
    /// \see SemaNode::State
    auto sema_state() const { return _state; }
    /// Check if this expression was successfully analysed by sema.
    bool sema_ok() const { return _state == SemaState::Done; }
    /// Check if sema has errored.
    bool sema_errored() const { return _state == SemaState::Errored; }
    /// \see SemaNode::State
    bool sema_done_or_errored() const {
        return _state == SemaState::Done or _state == SemaState::Errored;
    }

    /// \see SemaNode::State
    void set_sema_in_progress() {
        LCC_ASSERT(not sema_done_or_errored());
        _state = SemaState::InProgress;
    }
    /// \see SemaNode::State
    constexpr void set_sema_done() {
        LCC_ASSERT(_state != SemaState::Errored);
        _state = SemaState::Done;
    }
    /// \see SemaNode::State
    void set_sema_errored() {
        LCC_ASSERT(_state != SemaState::Done);
        _state = SemaState::Errored;
    }

    void* operator new(size_t) = delete;
    void* operator new(size_t sz, Parser& parser);
    void* operator new(size_t sz, Module& module);
};

/// @brief Base class for statement syntax nodes.
class Statement : public SemaNode {
public:
    enum struct Kind {
        OverloadSet,

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
        Discard,
        Expr,
        Empty,

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
        Xyzzy,

        // Other
        Test,
    };

private:
    const Kind _kind;

protected:
    Statement(Kind kind, Location location)
        : SemaNode(SemaNode::Kind::Statement, location), _kind(kind) {}

public:
    auto kind() const { return _kind; }
};

std::string ToString(Statement::Kind kind);

/// @brief Base class for expression syntax nodes.
class Expr : public SemaNode {
public:
    enum struct Kind {
        Unary,
        Binary,
        And,
        Or,
        Xor,

        UnwrapNilable,
        Constant,

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
        TypePoison,
        TypeInfer,
        TypeNilable,
        TypeErrUnion,

        TypeLookupName,
        TypeLookupPath,
        TypeLiteralString,

        TypeArray,
        TypeSlice,
        TypePointer,
        TypeBuffer,

        TypeFunc,

        TypeNoreturn,
        TypeRawptr,
        TypeVoid,
        TypeBool,
        TypeInt,
        TypeFloat,
    };

private:
    const Kind _kind;

    /// sema fields
    Type* _type = nullptr;

protected:
    constexpr Expr(Kind kind, Location location)
        : SemaNode(SemaNode::Kind::Expr, location), _kind(kind) {}

public:
    auto kind() const { return _kind; }
    auto type() const { return _type; }
    void type(Type* type) { _type = type; }

    bool evaluate(const LayeContext* laye_context, EvalResult& out, bool required);
};

std::string ToString(Expr::Kind kind);

class Decl : public Statement {
protected:
    Decl(Kind kind, Location location)
        : Statement(kind, location) {}

public:
    static bool classof(const Statement* statement) { return +statement->kind() >= +Kind::OverloadSet and +statement->kind() <= +Kind::DeclImport; }
};

class NamedDecl : public Decl {
    Module* _module;
    std::vector<DeclModifier> _mods;
    std::string _name;
    std::vector<TemplateParam> _template_params{};

    std::string _mangled_name;

protected:
    NamedDecl(Kind kind, Module* module, Location location, std::vector<DeclModifier> mods, std::string name)
        : Decl(kind, location), _module(module), _mods(std::move(mods)), _name(std::move(name)) {
        _mangled_name = _name;
    }

    NamedDecl(Kind kind, Module* module, Location location, std::vector<DeclModifier> mods, std::string name, std::vector<TemplateParam> template_params)
        : Decl(kind, location), _mods(std::move(mods)), _name(std::move(name)), _template_params(std::move(template_params)) {
        _mangled_name = _name;
    }

public:
    auto mods() const -> const std::vector<DeclModifier>& { return _mods; }
    auto name() const -> const std::string& { return _name; }
    auto template_params() const -> const std::vector<TemplateParam>& { return _template_params; }

    auto mangled_name() const -> const std::string& { return _mangled_name; }
    void mangled_name(std::string mangled_name) { _mangled_name = std::move(mangled_name); }

    bool has_mod(TokenKind mod_kind) const {
        auto mod_list = mods();
        auto e = std::find_if(mod_list.begin(), mod_list.end(), [mod_kind](const auto& m) { return m.decl_kind == mod_kind; });
        return e != mod_list.end();
    }

    void add_mod(DeclModifier modifier) { _mods.push_back(modifier); }

    bool is_export() const { return has_mod(TokenKind::Export); }
    bool is_foreign() const { return has_mod(TokenKind::Foreign); }

    auto linkage() const {
        auto mod_list = mods();
        if (auto e = std::find_if(mod_list.begin(), mod_list.end(), [](const auto& m) { return m.decl_kind == TokenKind::Export; }); e != mod_list.end())
            return Linkage::Exported;
        return Linkage::Internal;
    }

    auto calling_convention() const {
        auto mod_list = mods();
        if (auto e = std::find_if(mod_list.begin(), mod_list.end(), [](const auto& m) { return m.decl_kind == TokenKind::Callconv; }); e != mod_list.end())
            return e->call_conv;
        return CallConv::Laye;
    }

    static bool classof(const Statement* statement) { return +statement->kind() >= +Kind::OverloadSet and +statement->kind() <= +Kind::DeclAlias; }
};

class BindingDecl : public NamedDecl {
    Type* _type;
    Expr* _init;

public:
    BindingDecl(Module* module, Location location, std::vector<DeclModifier> mods, Type* type, std::string name, Expr* init)
        : NamedDecl(Kind::DeclBinding, module, location, mods, name), _type(type), _init(init) {}

    auto type() const { return _type; }
    auto type() -> Type*& { return _type; }
    auto init() const { return _init; }
    auto init() -> Expr*& { return _init; }

    static bool classof(const Statement* statement) { return statement->kind() == Kind::DeclBinding; }
};

class FunctionDecl : public NamedDecl {
    Type* _function_type;
    Type* _returnType;
    std::vector<FunctionParam*> _params;
    Statement* _body;
    // TODO(local): varargs

public:
    FunctionDecl(Module* module, Location location, std::vector<DeclModifier> mods, Type* returnType, std::string name, std::vector<TemplateParam> template_params, std::vector<FunctionParam*> params, Statement* body);

    auto return_type() const { return _returnType; }
    auto return_type() -> Type*& { return _returnType; }

    auto params() const -> const std::vector<FunctionParam*>& { return _params; }
    auto params() -> std::vector<FunctionParam*>& { return _params; }

    auto body() const { return _body; }
    auto body() -> Statement*& { return _body; }
    void body(Statement* body) { _body = body; }

    auto function_type() const { return _function_type; }

    static bool classof(const Statement* statement) { return statement->kind() == Kind::DeclFunction; }
};

class OverloadSet : public NamedDecl {
    std::vector<FunctionDecl*> _overloads{};

public:
    OverloadSet(Module* module, Location location, std::string name)
        : NamedDecl(Kind::OverloadSet, module, location, {}, std::move(name)) {}

    void add(FunctionDecl* overload) { _overloads.push_back(overload); }
    auto overloads() const -> const decltype(_overloads)& { return _overloads; }
    auto overloads() -> decltype(_overloads)& { return _overloads; }

    static bool classof(const Statement* statement) { return statement->kind() == Kind::OverloadSet; }
};

class StructDecl : public NamedDecl {
    std::vector<BindingDecl*> _fields;
    std::vector<StructDecl*> _variants;

public:
    StructDecl(Module* module, Location location, std::vector<DeclModifier> mods, std::string name, std::vector<TemplateParam> template_params, std::vector<BindingDecl*> fields, std::vector<StructDecl*> variants)
        : NamedDecl(Kind::DeclStruct, module, location, mods, name, template_params), _fields(std::move(fields)), _variants(std::move(variants)) {}

    auto fields() const -> std::span<BindingDecl* const> { return _fields; }
    auto variants() const -> std::span<StructDecl* const> { return _variants; }

    static bool classof(const Statement* statement) { return statement->kind() == Kind::DeclStruct; }
};

class EnumDecl : public NamedDecl {
    Type* _underlying_type;
    std::vector<EnumVariant> _variants;

public:
    EnumDecl(Module* module, Location location, std::vector<DeclModifier> mods, std::string name, Type* underlying_type, std::vector<EnumVariant> variants)
        : NamedDecl(Kind::DeclEnum, module, location, mods, name), _underlying_type(underlying_type), _variants(std::move(variants)) {}

    auto underlying_type() const { return _underlying_type; }
    auto variants() const -> std::span<EnumVariant const> { return _variants; }

    static bool classof(const Statement* statement) { return statement->kind() == Kind::DeclEnum; }
};

class AliasDecl : public NamedDecl {
    bool _is_strict;
    Type* _type;

public:
    AliasDecl(Module* module, Location location, std::vector<DeclModifier> mods, bool is_strict, std::string name, Type* type)
        : NamedDecl(Kind::DeclAlias, module, location, mods, name), _is_strict(is_strict), _type(type) {}

    bool is_strict() { return _is_strict; }

    auto type() const { return _type; }
    auto type() -> Type*& { return _type; }

    static bool classof(const Statement* statement) { return statement->kind() == Kind::DeclAlias; }
};

/// @brief Base class for file header nodes.
class ModuleHeader : public Decl {
protected:
    ModuleHeader(Kind kind, Location location)
        : Decl(kind, location) {}

public:
    static bool classof(const Statement* statement) { return +statement->kind() >= +Kind::DeclImport and +statement->kind() <= +Kind::DeclImport; }
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
    bool has_alias() const { return not _alias.empty(); }
    auto alias() const -> const std::string& { return _alias; }

    static bool classof(const Statement* statement) { return statement->kind() == Kind::DeclImport; }
};

class BlockStatement : public Statement {
    std::vector<Statement*> _children;

public:
    BlockStatement(Location location, std::vector<Statement*> children)
        : Statement(Kind::Block, location), _children(std::move(children)) {}

    auto children() const -> const std::vector<Statement*>& { return _children; }
    auto children() -> std::vector<Statement*>& { return _children; }

    static bool classof(const Statement* statement) { return statement->kind() == Kind::Block; }
};

class AssignStatement : public Statement {
    OperatorKind _assign_op;
    Expr* _target;
    Expr* _value;

public:
    AssignStatement(Location location, OperatorKind assign_op, Expr* target, Expr* value)
        : Statement(Kind::Assign, location), _assign_op(assign_op), _target(target), _value(value) {}

    auto assign_op() const { return _assign_op; }
    auto target() const { return _target; }
    auto value() const { return _value; }

    static bool classof(const Statement* statement) { return statement->kind() == Kind::Assign; }
};

class DeleteStatement : public Statement {
    Expr* _expr;

public:
    DeleteStatement(Expr* expr)
        : Statement(Kind::Delete, expr->location()), _expr(expr) {}

    auto expr() const { return _expr; }

    static bool classof(const Statement* statement) { return statement->kind() == Kind::Delete; }
};

class DiscardStatement : public Statement {
    Expr* _expr;

public:
    DiscardStatement(Expr* expr)
        : Statement(Kind::Discard, expr->location()), _expr(expr) {}

    auto expr() const { return _expr; }

    static bool classof(const Statement* statement) { return statement->kind() == Kind::Discard; }
};

class ExprStatement : public Statement {
    Expr* _expr;

public:
    ExprStatement(Expr* expr)
        : Statement(Kind::Expr, expr->location()), _expr(expr) {}

    auto expr() const { return _expr; }
    auto expr() -> Expr*& { return _expr; }

    static bool classof(const Statement* statement) { return statement->kind() == Kind::Expr; }
};

class EmptyStatement : public Statement {
public:
    EmptyStatement(Location location)
        : Statement(Kind::Expr, location) {}

    static bool classof(const Statement* statement) { return statement->kind() == Kind::Empty; }
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
    auto label() const -> const std::string& { return _label; }
    void set_label(std::string label) { _label = std::move(label); }

    auto condition() const { return _condition; }
    auto condition() -> Expr*& { return _condition; }
    auto pass() const { return _pass; }
    auto pass() -> Statement*& { return _pass; }
    auto fail() const { return _fail; }
    auto fail() -> Statement*& { return _fail; }

    static bool classof(const Statement* statement) { return statement->kind() == Kind::If; }
};

class ForStatement : public Statement {
    std::string _label{};

    Statement* _init;
    Expr* _condition;
    Statement* _increment;

    Statement* _pass;
    Statement* _fail;

public:
    ForStatement(Location location, Expr* condition, Statement* pass, Statement* fail)
        : Statement(Kind::For, location), _init(nullptr), _condition(condition), _increment(nullptr), _pass(pass), _fail(fail) {}

    ForStatement(Location location, Statement* init, Expr* condition, Statement* increment, Statement* pass, Statement* fail)
        : Statement(Kind::For, location), _init(init), _condition(condition), _increment(increment), _pass(pass), _fail(fail) {}

    bool has_label() const { return not _label.empty(); }
    auto label() const -> const std::string& { return _label; }
    void set_label(std::string label) { _label = std::move(label); }

    auto init() const { return _init; }
    auto condition() const { return _condition; }
    auto increment() const { return _increment; }

    auto pass() const { return _pass; }
    auto fail() const { return _fail; }

    static bool classof(const Statement* statement) { return statement->kind() == Kind::For; }
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
    auto label() const -> const std::string& { return _label; }
    void set_label(std::string label) { _label = std::move(label); }

    auto type() const { return _type; }
    auto name() const -> const std::string& { return _name; }
    auto sequence() const { return _sequence; }

    auto pass() const { return _pass; }
    auto fail() const { return _fail; }

    static bool classof(const Statement* statement) { return statement->kind() == Kind::ForEach; }
};

class DoForStatement : public Statement {
    std::string _label{};

    Expr* _condition;
    Statement* _body;

public:
    DoForStatement(Location location, Expr* condition, Statement* body)
        : Statement(Kind::DoFor, location), _condition(condition), _body(body) {}

    bool has_label() const { return not _label.empty(); }
    auto label() const -> const std::string& { return _label; }
    void set_label(std::string label) { _label = std::move(label); }

    auto condition() const { return _condition; }
    auto body() const { return _body; }

    static bool classof(const Statement* statement) { return statement->kind() == Kind::DoFor; }
};

class FallthroughStatement : public Statement {
public:
    FallthroughStatement(Location location)
        : Statement(Kind::Fallthrough, location) {}

    static bool classof(const Statement* statement) { return statement->kind() == Kind::Fallthrough; }
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

    static bool classof(const Statement* statement) { return statement->kind() == Kind::Switch; }
};

class ReturnStatement : public Statement {
    Expr* _value;

public:
    ReturnStatement(Location location, Expr* value)
        : Statement(Kind::Return, location), _value(value) {}

    bool is_void_return() const { return _value == nullptr; }
    auto value() const { return _value; }
    auto value() -> Expr*& { return _value; }

    static bool classof(const Statement* statement) { return statement->kind() == Kind::Return; }
};

class BreakStatement : public Statement {
    std::string _target{};

public:
    BreakStatement(Location location, std::string target = "")
        : Statement(Kind::Break, location), _target(std::move(target)) {}

    bool has_target() const { return not _target.empty(); }
    auto target() const -> const std::string& { return _target; }

    static bool classof(const Statement* statement) { return statement->kind() == Kind::Break; }
};

class ContinueStatement : public Statement {
    std::string _target{};

public:
    ContinueStatement(Location location, std::string target = "")
        : Statement(Kind::Continue, location), _target(std::move(target)) {}

    bool has_target() const { return not _target.empty(); }
    auto target() const -> const std::string& { return _target; }

    static bool classof(const Statement* statement) { return statement->kind() == Kind::Continue; }
};

class DeferStatement : public Statement {
    Statement* _statement;

public:
    DeferStatement(Location location, Statement* statement)
        : Statement(Kind::Defer, location), _statement(statement) {}

    auto statement() const { return _statement; }

    static bool classof(const Statement* statement) { return statement->kind() == Kind::Defer; }
};

class GotoStatement : public Statement {
    std::string _target{};

public:
    GotoStatement(Location location, std::string target)
        : Statement(Kind::Goto, location), _target(std::move(target)) {}

    auto target() const -> const std::string& { return _target; }

    static bool classof(const Statement* statement) { return statement->kind() == Kind::Goto; }
};

class XyzzyStatement : public Statement {
public:
    XyzzyStatement(Location location)
        : Statement(Kind::Xyzzy, location) {}

    static bool classof(const Statement* statement) { return statement->kind() == Kind::Xyzzy; }
};

class TestStatement : public Statement {
    std::string _name;
    std::vector<Statement*> _children;

public:
    TestStatement(Location location, std::string name, std::vector<Statement*> children)
        : Statement(Kind::Test, location), _name(std::move(name)), _children(std::move(children)) {}

    auto name() const -> const std::string& { return _name; }
    auto children() const -> std::span<Statement* const> { return _children; }

    static bool classof(const Statement* statement) { return statement->kind() == Kind::Test; }
};

class UnaryExpr : public Expr {
    OperatorKind _operator_kind;
    Expr* _value;

public:
    UnaryExpr(Location location, OperatorKind operator_kind, Expr* value)
        : Expr(Kind::Unary, location), _operator_kind(operator_kind), _value(value) {}

    auto operator_kind() const { return _operator_kind; }
    auto value() const { return _value; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::Unary; }
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
    auto lhs() -> Expr*& { return _lhs; }
    auto rhs() const { return _rhs; }
    auto rhs() -> Expr*& { return _rhs; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::Binary; }
};

class AndExpr : public Expr {
    Expr* _lhs;
    Expr* _rhs;

public:
    AndExpr(Location location, Expr* lhs, Expr* rhs)
        : Expr(Kind::And, location), _lhs(lhs), _rhs(rhs) {}

    auto lhs() const { return _lhs; }
    auto rhs() const { return _rhs; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::And; }
};

class OrExpr : public Expr {
    Expr* _lhs;
    Expr* _rhs;

public:
    OrExpr(Location location, Expr* lhs, Expr* rhs)
        : Expr(Kind::Or, location), _lhs(lhs), _rhs(rhs) {}

    auto lhs() const { return _lhs; }
    auto rhs() const { return _rhs; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::Or; }
};

class XorExpr : public Expr {
    Expr* _lhs;
    Expr* _rhs;

public:
    XorExpr(Location location, Expr* lhs, Expr* rhs)
        : Expr(Kind::Xor, location), _lhs(lhs), _rhs(rhs) {}

    auto lhs() const { return _lhs; }
    auto rhs() const { return _rhs; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::Xor; }
};

class UnwrapNilableExpr : public Expr {
    Expr* _value;

public:
    UnwrapNilableExpr(Location location, Expr* value)
        : Expr(Kind::UnwrapNilable, location), _value(value) {}

    auto value() const { return _value; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::UnwrapNilable; }
};

class NameExpr : public Expr {
    Scope* _scope;
    std::string _name{};
    std::vector<Expr*> _template_args{};
    NamedDecl* _target{nullptr};

public:
    NameExpr(Location location, Scope* scope, std::string name)
        : Expr(Kind::LookupName, location), _scope(scope), _name(std::move(name)) {}

    NameExpr(Location location, Scope* scope, std::string name, std::vector<Expr*> template_args)
        : Expr(Kind::LookupName, location), _scope(scope), _name(std::move(name)), _template_args(std::move(template_args)) {}

    auto scope() const { return _scope; }
    auto name() const -> const std::string& { return _name; }
    auto template_args() const -> const std::vector<Expr*>& { return _template_args; }

    auto target() const { return _target; }
    auto target() -> NamedDecl*& { return _target; }
    auto target(NamedDecl* t) { _target = t; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::LookupName; }
};

class PathExpr : public Expr {
    PathKind _path_kind;
    Scope* _scope;
    std::vector<std::string> _names;
    std::vector<Location> _locations;
    std::vector<Expr*> _template_args{};
    NamedDecl* _target{nullptr};

public:
    PathExpr(PathKind path_kind, Scope* scope, std::vector<std::string> names, std::vector<Location> locations)
        : Expr(Kind::LookupPath, Location{locations[0], locations.back()}), _path_kind(path_kind), _scope(scope), _names(std::move(names)), _locations(std::move(locations)) {}

    PathExpr(PathKind path_kind, Scope* scope, std::vector<std::string> names, std::vector<Location> locations, std::vector<Expr*> template_args)
        : Expr(Kind::LookupPath, Location{locations[0], locations.back()}), _path_kind(path_kind), _scope(scope), _names(std::move(names)), _locations(std::move(locations)), _template_args(std::move(template_args)) {}

    auto path_kind() const { return _path_kind; }
    auto scope() const { return _scope; }
    auto names() const -> const std::vector<std::string>& { return _names; }
    auto locations() const -> const std::vector<Location>& { return _locations; }
    auto template_args() const -> const std::vector<Expr*>& { return _template_args; }

    auto target() const { return _target; }
    auto target() -> NamedDecl*& { return _target; }
    auto target(NamedDecl* t) { _target = t; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::LookupPath; }
};

class FieldIndexExpr : public Expr {
    Expr* _target;
    std::string _field_name;

public:
    FieldIndexExpr(Location location, Expr* target, std::string field_name)
        : Expr(Kind::FieldIndex, location), _target(target), _field_name(field_name) {}

    auto target() const { return _target; }
    auto field_name() const -> const std::string& { return _field_name; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::FieldIndex; }
};

class ValueIndexExpr : public Expr {
    Expr* _target;
    std::vector<Expr*> _indices;

public:
    ValueIndexExpr(Location location, Expr* target, std::vector<Expr*> indices)
        : Expr(Kind::ValueIndex, location), _target(target), _indices(std::move(indices)) {}

    auto target() const { return _target; }
    auto indices() const -> std::span<Expr* const> { return _indices; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::ValueIndex; }
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

    static bool classof(const Expr* expr) { return expr->kind() == Kind::Slice; }
};

class CallExpr : public Expr {
    Expr* _target;
    std::vector<Expr*> _args;

public:
    CallExpr(Location location, Expr* target, std::vector<Expr*> args)
        : Expr(Kind::Call, location), _target(target), _args(std::move(args)) {}

    auto target() const { return _target; }
    auto target() -> Expr*& { return _target; }
    auto args() const { return _args; }
    auto args() -> decltype(_args)& { return _args; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::Call; }
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

    static bool classof(const Expr* expr) { return expr->kind() == Kind::Ctor; }
};

class NotExpr : public Expr {
    Expr* _value;

public:
    NotExpr(Location location, Expr* value)
        : Expr(Kind::Not, location), _value(value) {}

    auto value() const { return _value; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::Not; }
};

class CastExpr : public Expr {
    Type* _type;
    Expr* _value;
    CastKind _cast_kind;

public:
    CastExpr(Location location, Type* type, Expr* value, CastKind cast_kind)
        : Expr(Kind::Cast, location), _type(type), _value(value), _cast_kind(cast_kind) {}

    auto type() const { return _type; }
    auto value() const { return _value; }
    auto value() -> Expr*& { return _value; }
    auto cast_kind() const { return _cast_kind; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::Cast; }
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

    static bool classof(const Expr* expr) { return expr->kind() == Kind::New; }
};

class TryExpr : public Expr {
    Expr* _value;

public:
    TryExpr(Location location, Expr* value)
        : Expr(Kind::Try, location), _value(value) {}

    auto value() const { return _value; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::Try; }
};

class CatchExpr : public Expr {
    Expr* _value;
    std::string _error_name;
    Statement* _body;

public:
    CatchExpr(Location location, Expr* value, std::string error_name, Statement* body)
        : Expr(Kind::Catch, location), _value(value), _error_name(std::move(error_name)), _body(body) {}

    auto value() const { return _value; }
    auto error_name() const -> const std::string& { return _error_name; }
    auto body() const { return _body; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::Catch; }
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
// T? ret<T>(T t) => t;
// B? bind<A, B>(A? a, B?(A) f)
// {
//     if a is nil then return nil;
//     return f(a!);
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
// // becomes ->  bind(bind(get_user(id), get_user_address), get_street_address);
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

    static bool classof(const Expr* expr) { return expr->kind() == Kind::Do; }
};

class SizeofExpr : public Expr {
    Type* _type;

public:
    SizeofExpr(Location location, Type* type)
        : Expr(Kind::Sizeof, location), _type(type) {}

    auto type() const { return _type; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::Sizeof; }
};

class OffsetofExpr : public Expr {
    Type* _type;
    std::string _field_name;

public:
    OffsetofExpr(Location location, Type* type, std::string field_name)
        : Expr(Kind::Offsetof, location), _type(type), _field_name(std::move(field_name)) {}

    auto type() const { return _type; }
    auto field_name() const -> const std::string& { return _field_name; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::Offsetof; }
};

class AlignofExpr : public Expr {
    Type* _type;
    std::string _field_name;

public:
    AlignofExpr(Location location, Type* type, std::string field_name)
        : Expr(Kind::Alignof, location), _type(type), _field_name(std::move(field_name)) {}

    auto type() const { return _type; }
    auto field_name() const -> const std::string& { return _field_name; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::Alignof; }
};

class LitNilExpr : public Expr {
public:
    LitNilExpr(Location location)
        : Expr(Kind::LitNil, location) {}

    static bool classof(const Expr* expr) { return expr->kind() == Kind::LitNil; }
};

class LitBoolExpr : public Expr {
    bool _value;

public:
    LitBoolExpr(Location location, bool value)
        : Expr(Kind::LitBool, location), _value(value) {}

    auto value() const { return _value; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::LitBool; }
};

class LitStringExpr : public Expr {
    std::string _value;

public:
    LitStringExpr(Location location, std::string value)
        : Expr(Kind::LitString, location), _value(std::move(value)) {}

    auto value() const -> const std::string& { return _value; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::LitString; }
};

class LitIntExpr : public Expr {
    u64 _value;

public:
    LitIntExpr(Location location, u64 value)
        : Expr(Kind::LitInt, location), _value(value) {}

    auto value() const { return _value; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::LitInt; }
};

class LitFloatExpr : public Expr {
    long double _value;

public:
    LitFloatExpr(Location location, long double value)
        : Expr(Kind::LitFloat, location), _value(value) {}

    auto value() const { return _value; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::LitFloat; }
};

class ConstantExpr : public Expr {
    EvalResult _value;
    Expr* _expression;

public:
    ConstantExpr(Expr* expr, EvalResult value)
        : Expr(Kind::Constant, expr->location()), _value(std::move(value)), _expression(expr) {
        LCC_ASSERT(expr->sema_ok());
        expr->set_sema_done();
    }

    auto expr() const { return _expression; }
    auto value() const -> const EvalResult& { return _value; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::Constant; }
};

/// @brief Base class for type syntax nodes (which we're trying to make also Exprs.)
class Type : public Expr {
protected:
    constexpr Type(Kind kind, Location location)
        : Expr(kind, location) {}

public:
    auto string(bool use_colours = false) const -> std::string;

    /// Get the size of this type. It may be target-dependent,
    /// which is why this takes a context parameter.
    ///
    /// \param ctx The context to use.
    /// \return The size of this type, in bits.
    usz size(const Context* ctx) const;
    /// Get the alignment of this type. It may be target-dependent,
    /// which is why this takes a context parameter.
    ///
    /// \param ctx The context to use.
    /// \return The alignment of this type, in bits.
    usz align(const Context* ctx) const;

    bool is_poison() const { return kind() == Kind::TypePoison; }
    /// Check if this is the uninitialised type.
    // bool is_unknown() const;
    /// Check if this is a string type.
    bool is_string() const { return kind() == Kind::TypeLiteralString; }
    /// Check if this is the builtin \c var type.
    bool is_infer() const { return kind() == Kind::TypeInfer; }
    /// Check if this is the builtin \c void type.
    bool is_void() const { return kind() == Kind::TypeVoid; }
    /// Check if this is the builtin \c noreturn type.
    bool is_noreturn() const { return kind() == Kind::TypeNoreturn; }
    /// Check if this is an integer type.
    bool is_integer() const { return kind() == Kind::TypeInt; }
    /// Check if this is an integer type.
    bool is_bool() const { return kind() == Kind::TypeBool; }
    /// Check if this is a slice type.
    bool is_slice() const { return kind() == Kind::TypeSlice; }
    /// Check if this is a array type.
    bool is_array() const { return kind() == Kind::TypeArray; }
    /// Check if this is a pointer type.
    bool is_pointer() const { return kind() == Kind::TypePointer; }
    /// Check if this is a buffer type.
    bool is_buffer() const { return kind() == Kind::TypeBuffer; }
    /// Check if this is a function type.
    bool is_function() const { return kind() == Kind::TypeFunc; }

    static Type* Bool;

    /// Check if types are equal to each other.
    static bool Equal(const Type* a, const Type* b);

    static bool classof(const Expr* expr) { return +expr->kind() >= +Kind::TypePoison && +expr->kind() <= +Kind::TypeFloat; }
};

class PoisonType : public Type {
public:
    PoisonType(Location location)
        : Type(Kind::TypePoison, location) {}

    static bool classof(const Expr* expr) { return expr->kind() == Kind::TypePoison; }
};

class InferType : public Type {
public:
    InferType(Location location)
        : Type(Kind::TypeInfer, location) {}

    static bool classof(const Expr* expr) { return expr->kind() == Kind::TypeInfer; }
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

    static bool classof(const Expr* expr) { return expr->kind() == Kind::TypeErrUnion; }
};

class NameType : public Type {
    TypeAccess _access;
    Scope* _scope;
    std::string _name;
    std::vector<Expr*> _template_args{};

public:
    NameType(Location location, TypeAccess access, Scope* scope, std::string name)
        : Type(Kind::TypeLookupName, location), _access(access), _scope(scope), _name(std::move(name)) {}

    NameType(Location location, TypeAccess access, Scope* scope, std::string name, std::vector<Expr*> template_args)
        : Type(Kind::TypeLookupName, location), _access(access), _scope(scope), _name(std::move(name)), _template_args(std::move(template_args)) {}

    auto access() const { return _access; }
    auto scope() const { return _scope; }
    auto name() const -> const std::string& { return _name; }
    auto template_args() const -> const std::vector<Expr*>& { return _template_args; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::TypeLookupName; }
};

class PathType : public Type {
    PathKind _path_kind;
    TypeAccess _access;
    Scope* _scope;
    std::vector<std::string> _names;
    std::vector<Location> _locations;
    std::vector<Expr*> _template_args{};

public:
    PathType(PathKind path_kind, TypeAccess access, Scope* scope, std::vector<std::string> names, std::vector<Location> locations)
        : Type(Kind::TypeLookupPath, Location{locations[0], locations.back()}), _path_kind(path_kind), _access(access), _scope(scope), _names(std::move(names)), _locations(std::move(locations)) {}

    PathType(PathKind path_kind, TypeAccess access, Scope* scope, std::vector<std::string> names, std::vector<Location> locations, std::vector<Expr*> template_args)
        : Type(Kind::TypeLookupPath, Location{locations[0], locations.back()}), _path_kind(path_kind), _access(access), _scope(scope), _names(std::move(names)), _locations(std::move(locations)), _template_args(std::move(template_args)) {}

    auto path_kind() const { return _path_kind; }
    auto access() const { return _access; }
    auto scope() const { return _scope; }
    auto names() const -> const std::vector<std::string>& { return _names; }
    auto locations() const -> const std::vector<Location>& { return _locations; }
    auto template_args() const -> const std::vector<Expr*>& { return _template_args; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::TypeLookupPath; }
};

class LiteralStringType : public Type {
public:
    LiteralStringType(Location location)
        : Type(Kind::TypeLiteralString, location) {}

    static bool classof(const Expr* expr) { return expr->kind() == Kind::TypeLiteralString; }
};

class SingleElementType : public Type {
    Type* _elem_type;

public:
    SingleElementType(Kind kind, Location location, Type* elem_type)
        : Type(kind, location), _elem_type(elem_type) {}

    auto elem_type() const { return _elem_type; }
    auto elem_type() -> Type*& { return _elem_type; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::TypeNilable or (+expr->kind() >= +Kind::TypeArray and +expr->kind() <= +Kind::TypeBuffer); }
};

class NilableType : public SingleElementType {
public:
    NilableType(Type* elementType)
        : SingleElementType(Kind::TypeNilable, elementType->location(), elementType) {}

    static bool classof(const Expr* expr) { return expr->kind() == Kind::TypeNilable; }
};

class ArrayType : public SingleElementType {
    TypeAccess _access;
    std::vector<Expr*> _rank_lengths;

public:
    ArrayType(Location location, TypeAccess access, Type* elem_type, std::vector<Expr*> rank_lengths)
        : SingleElementType(Kind::TypeArray, location, elem_type), _access(access), _rank_lengths(std::move(rank_lengths)) {}

    auto access() const { return _access; }
    auto rank_lengths() -> decltype(_rank_lengths)& { return _rank_lengths; }
    auto rank_lengths() const -> const decltype(_rank_lengths)& { return _rank_lengths; }
    auto rank() const -> usz { return _rank_lengths.size(); }
    auto nth_length(usz i) const -> usz { return (usz) (as<ConstantExpr>(_rank_lengths[i]))->value().as_i64(); }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::TypeArray; }
};

class SliceType : public SingleElementType {
    TypeAccess _access;

public:
    SliceType(Location location, TypeAccess access, Type* elem_type)
        : SingleElementType(Kind::TypeSlice, location, elem_type), _access(access) {}

    auto access() const { return _access; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::TypeSlice; }
};

class PointerType : public SingleElementType {
    TypeAccess _access;

public:
    PointerType(Location location, TypeAccess access, Type* elem_type)
        : SingleElementType(Kind::TypePointer, location, elem_type), _access(access) {}

    auto access() const { return _access; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::TypePointer; }
};

class BufferType : public SingleElementType {
    TypeAccess _access;

public:
    BufferType(Location location, TypeAccess access, Type* elem_type)
        : SingleElementType(Kind::TypeBuffer, location, elem_type), _access(access) {}

    auto access() const { return _access; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::TypeBuffer; }
};

class FuncType : public Type {
    Expr* _calling_convention{};
    Type* _return_type;
    std::vector<Type*> _param_types;
    // TODO(local): varargs

public:
    FuncType(Location location, Type* return_type, std::vector<Type*> param_types)
        : Type(Kind::TypeFunc, location), _return_type(return_type), _param_types(std::move(param_types)) {}

    FuncType(Location location, Expr* calling_convention, Type* return_type, std::vector<Type*> param_types)
        : Type(Kind::TypeFunc, location), _calling_convention(calling_convention), _return_type(return_type), _param_types(std::move(param_types)) {}

    auto calling_convention() const { return _calling_convention; }
    auto return_type() const { return _return_type; }
    auto return_type() -> Type*& { return _return_type; }
    auto param_types() const -> const decltype(_param_types)& { return _param_types; }
    auto param_types() -> decltype(_param_types)& { return _param_types; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::TypeFunc; }
};

class NoreturnType : public Type {
public:
    NoreturnType(Location location)
        : Type(Kind::TypeNoreturn, location) {}

    static bool classof(const Expr* expr) { return expr->kind() == Kind::TypeNoreturn; }
};

class RawptrType : public Type {
public:
    RawptrType(Location location)
        : Type(Kind::TypeRawptr, location) {}

    static bool classof(const Expr* expr) { return expr->kind() == Kind::TypeRawptr; }
};

class VoidType : public Type {
public:
    VoidType(Location location)
        : Type(Kind::TypeVoid, location) {}

    static bool classof(const Expr* expr) { return expr->kind() == Kind::TypeVoid; }
};

class SizableType : public Type {
    int _bit_width;
    bool _is_platform;

protected:
    constexpr SizableType(Kind kind, Location location, int bit_width, bool is_platform = false)
        : Type(kind, location), _bit_width(bit_width), _is_platform(is_platform) {}

public:
    bool is_sized() const { return _bit_width > 0; }

    int bit_width() const { return _bit_width; }
    void bit_width(int w) { _bit_width = w; }

    bool is_platform() const { return _is_platform; }
    void is_platform(bool p) { _is_platform = p; }

    static bool classof(const Expr* expr) { return +expr->kind() >= +Kind::TypeBool and +expr->kind() <= +Kind::TypeFloat; }
};

class BoolType : public SizableType {
public:
    constexpr BoolType(Location location, int bit_width = 0, bool is_platform = false)
        : SizableType(Kind::TypeBool, location, bit_width, is_platform) {}

    static bool classof(const Expr* expr) { return expr->kind() == Kind::TypeBool; }
};

class IntType : public SizableType {
    bool _is_signed;

public:
    constexpr IntType(Location location, bool is_signed, int bit_width = 0, bool is_platform = false)
        : SizableType(Kind::TypeInt, location, bit_width, is_platform), _is_signed(is_signed) {}

    bool is_signed() const { return _is_signed; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::TypeInt; }
};

class FloatType : public SizableType {
public:
    constexpr FloatType(Location location, int bit_width = 0, bool is_platform = false)
        : SizableType(Kind::TypeFloat, location, bit_width, is_platform) {}

    static bool classof(const Expr* expr) { return expr->kind() == Kind::TypeFloat; }
};
} // namespace lcc::laye

#endif // LAYE_AST_HH
