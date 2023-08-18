#ifndef INTERCEPT_AST_HH
#define INTERCEPT_AST_HH

#include <intercept/eval.hh>
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
    Do,
    Then,
    Extern,
    Static,
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

enum class CastKind {
    SoftCast,           ///< Explicit cast using \c as.
    HardCast,           ///< Explicit cast using \c as!.
    ImplicitCast,       ///< Implicit conversion.
    LValueToRValueConv, ///< Lvalue-to-rvalue conversion.
};

/// Convert a token kind to a string representation.
auto ToString(TokenKind kind) -> std::string_view;

class Module {
public:
    struct Ref {
        std::string name;
        mutable Module* module;
        Ref(std::string name, Module* module) : name(std::move(name)), module(module) {}
    };

private:
    std::string name{};

    FuncDecl* top_level_function{};
    bool is_module;
    File* file;

    std::vector<Ref> _imports;
    std::vector<Decl*> exports;
    std::vector<FuncDecl*> _functions;

    usz lambda_counter = 0;

public:
    Module(File* file, std::string module_name, bool is_logical_module);

    ~Module();

    /// Add an export.
    void add_export(Decl* decl) { exports.push_back(decl); }

    /// Add a function to this module.
    void add_function(FuncDecl* func) { _functions.push_back(func); }

    /// Add an import.
    void add_import(std::string module_name) {
        _imports.emplace_back(std::move(module_name), nullptr);
    }

    /// Add a top-level expression.
    void add_top_level_expr(Expr* node);

    /// Get the functions that are part of this module.
    auto functions() -> std::vector<FuncDecl*>& { return _functions; }

    /// Get the global scope.
    auto global_scope() const -> Scope* { return scopes[0]; }

    /// Get the module imports.
    auto imports() -> std::vector<Ref>& { return _imports; }

    /// Intern a string and return its index.
    usz intern(std::string_view str);

    /// Print the AST of this module.
    void print();

    /// Get the top-level function.
    auto top_level_func() const -> FuncDecl* { return top_level_function; }

    /// Get the top-level scope.
    auto top_level_scope() const -> Scope* { return scopes[1]; }

    /// Get a unique function name.
    auto unique_function_name() -> std::string { return fmt::format("_XInt__lambda_{}", lambda_counter++); }

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
    /// \param ctx The LCC context
    /// \param name The name of the declared symbol.
    /// \param decl The declaration to bind to the symbol.
    /// \return The same declaration, or an error.
    auto declare(
        const Context* ctx,
        std::string&& name,
        Decl* decl
    ) -> Result<Decl*>;

    /// Look up a symbol in this scope.
    auto find(std::string_view name) { return symbols.equal_range(name); }

    /// Get the parent scope.
    auto parent() const { return _parent; }

    /// Mark this scope as a function scope.
    void set_function_scope() { is_function_scope = true; }
};

/// Base class for nodes and types, i.e. for anything that
/// can be analysed in sema.
class SemaNode {
public:
    /// State of semantic analysis for an expression or type.
    enum struct State {
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

private:
    State _state = State::NotAnalysed;

protected:
    constexpr SemaNode() = default;

public:
    /// Check if this expression was successfully analysed by sema.
    bool ok() const { return _state == State::Done; }

    /// Get the state of semantic analysis for this node.
    /// \see SemaNode::State
    auto sema() const -> State { return _state; }

    /// Check if sema has errored.
    bool sema_errored() const { return _state == State::Errored; }

    /// \see SemaNode::State
    bool sema_done_or_errored() const {
        return _state == State::Done or _state == State::Errored;
    }

    /// \see SemaNode::State
    void set_sema_in_progress() {
        LCC_ASSERT(not sema_done_or_errored());
        _state = State::InProgress;
    }

    /// \see SemaNode::State
    constexpr void set_sema_done() {
        LCC_ASSERT(_state != State::Errored);
        _state = State::Done;
    }

    /// \see SemaNode::State
    void set_sema_errored() {
        LCC_ASSERT(_state != State::Done);
        _state = State::Errored;
    }
};

class Type : public SemaNode {
    friend class lcc::Context;

public:
    enum struct Kind {
        Builtin,
        FFIType,
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
    constexpr Type(Kind kind, Location location)
        : _kind(kind), _location(location) {}

public:
    virtual ~Type() = default;

    void* operator new(size_t) = delete;
    void* operator new(size_t sz, Module& mod) {
        auto ptr = ::operator new(sz);
        mod.types.push_back(static_cast<Type*>(ptr));
        return ptr;
    }

    /// Get the alignment of this type. It may be target-dependent,
    /// which is why this takes a context parameter.
    ///
    /// \param ctx The context to use.
    /// \return The alignment of this type, in bits.
    usz align(const Context* ctx) const;

    /// Get the element type of this type. This will assert if this
    /// type does not have an element type.
    auto elem() const -> Type*;

    /// Get the kind of this type.
    auto kind() const { return _kind; }

    /// Check if this is an array type.
    bool is_array() const { return _kind == Kind::Array; }

    /// Check if this is the builtin \c bool type.
    bool is_bool() const;

    /// Check if this is the builtin \c byte type.
    bool is_byte() const;

    /// Check if this is a builtin type.
    bool is_builtin() const { return _kind == Kind::Builtin; }

    /// Check if this is a function type.
    bool is_function() const { return _kind == Kind::Function; }

    /// Returns true if this is a sized integer type, a C
    /// FFI integer type, \c int, or \c byte, or \c bool, if
    /// \c include_bool is true.
    bool is_integer(bool include_bool = false) const;

    /// Check if this is a pointer type.
    bool is_pointer() const { return _kind == Kind::Pointer; }

    /// Check if this is a reference type.
    bool is_reference() const { return _kind == Kind::Reference; }

    /// Check if this is a signed integer type.
    bool is_signed_int(const Context* ctx) const;

    /// Check if this is a sized integer type.
    bool is_sized_integer() const { return _kind == Kind::Integer; }

    /// Check if this is a struct type.
    bool is_struct() const { return _kind == Kind::Struct; }

    /// Check if this is the uninitialised type.
    bool is_unknown() const;

    /// Check if this is an unsigned integer type.
    bool is_unsigned_int(const Context* ctx) const;

    /// Check if this is the builtin \c void type.
    bool is_void() const;

    /// Get the location of this type.
    auto location() const { return _location; }

    /// Get the size of this type. It may be target-dependent,
    /// which is why this takes a context parameter.
    ///
    /// \param ctx The context to use.
    /// \return The size of this type, in bits.
    usz size(const Context* ctx) const;

    /// Get a string representation of this type.
    auto string(bool use_colours = false) const -> std::string;

    /// Return this type stripped of any pointers and references.
    auto strip_pointers_and_references() -> Type*;

    /// Return this type stripped of any references.
    auto strip_references() -> Type*;

    /// It’s way too easy to accidentally write `a == b` when
    /// you really meant `*a == *b`, so we don’t allow this.
    bool operator==(const Type& other) const = delete;

    /// Check if types are equal to each other.
    static bool Equal(const Type* a, const Type* b);

    /// Use these only if there is no location information
    /// available (e.g. for default initialisers etc.). In
    /// any other case, prefer to create an instance of a
    /// BuiltinType instead.
    ///
    /// Do NOT compare against these w/ `==`!
    ///
    /// When adding a type here, don’t forget to initialise
    /// it in Context::InitialiseLCCData().
    static Type* Bool;
    static Type* Byte;
    static Type* Int;
    static Type* Unknown;
    static Type* Void;
    static Type* VoidPtr;

    /// The type of an unresolved overload set.
    static Type* OverloadSet;
};

/// This only holds a kind.
///
/// Builtin types are not singletons because they need to carry
/// location information. However, the size, alignment, etc of a
/// primitive type all depend on the target, and there is no point
/// in storing their names in each of them, so an instance of one
/// only ever stores its primitive type kind.
class BuiltinType : public Type {
    friend lcc::Context;

public:
    enum struct BuiltinKind {
        Bool,
        Byte,
        Int,
        Unknown,
        Void,
        OverloadSet,
    };

private:
    /// Shorten long signatures w/ this.
    using K = BuiltinKind;

    const BuiltinKind _kind;

    static auto Make(Module& mod, K k, Location l) -> BuiltinType* {
        return new (mod) BuiltinType(k, l);
    }

    constexpr BuiltinType(K k, Location location)
        : Type(Kind::Builtin, location), _kind(k) {
        set_sema_done();
    }

public:
    /// Get the kind of this builtin.
    auto builtin_kind() const -> BuiltinKind { return _kind; }

    bool operator==(BuiltinKind k) const { return _kind == k; }

    /// Get instances of primitive types.
    static auto Bool(Module& mod, Location l = {}) -> BuiltinType* { return Make(mod, K::Bool, l); }
    static auto Byte(Module& mod, Location l = {}) -> BuiltinType* { return Make(mod, K::Byte, l); }
    static auto Integer(Module& mod, Location l = {}) -> BuiltinType* { return Make(mod, K::Int, l); }
    static auto Unknown(Module& mod, Location l = {}) -> BuiltinType* { return Make(mod, K::Unknown, l); }
    static auto Void(Module& mod, Location l = {}) -> BuiltinType* { return Make(mod, K::Void, l); }

    static bool classof(const Type* type) { return type->kind() == Kind::Builtin; }
};

/// C FFI integer type.
class FFIType : public Type {
public:
    enum struct FFIKind {
        CChar,
        CSChar,
        CUChar,
        CShort,
        CUShort,
        CInt,
        CUInt,
        CLong,
        CULong,
        CLongLong,
        CULongLong,
    };

private:
    /// Shorten long signatures w/ this.
    using K = FFIKind;

    const K kind;
    FFIType(K k, Location loc) : Type(Kind::FFIType, loc), kind(k) {}

    static auto Make(Module& mod, K k, Location l) -> FFIType* {
        return new (mod) FFIType(k, l);
    }

public:
    /// Get the kind of this C FFI type.
    auto ffi_kind() const -> FFIKind { return kind; }

    bool operator==(FFIKind k) const { return kind == k; }

    /// Get instances of C FFI types.
    static auto CChar(Module& mod, Location l = {}) -> FFIType* { return Make(mod, K::CChar, l); }
    static auto CSChar(Module& mod, Location l = {}) -> FFIType* { return Make(mod, K::CSChar, l); }
    static auto CUChar(Module& mod, Location l = {}) -> FFIType* { return Make(mod, K::CUChar, l); }
    static auto CShort(Module& mod, Location l = {}) -> FFIType* { return Make(mod, K::CShort, l); }
    static auto CUShort(Module& mod, Location l = {}) -> FFIType* { return Make(mod, K::CUShort, l); }
    static auto CInt(Module& mod, Location l = {}) -> FFIType* { return Make(mod, K::CInt, l); }
    static auto CUInt(Module& mod, Location l = {}) -> FFIType* { return Make(mod, K::CUInt, l); }
    static auto CLong(Module& mod, Location l = {}) -> FFIType* { return Make(mod, K::CLong, l); }
    static auto CULong(Module& mod, Location l = {}) -> FFIType* { return Make(mod, K::CULong, l); }
    static auto CLongLong(Module& mod, Location l = {}) -> FFIType* { return Make(mod, K::CLongLong, l); }
    static auto CULongLong(Module& mod, Location l = {}) -> FFIType* { return Make(mod, K::CULongLong, l); }

    static bool classof(const Type* type) { return type->kind() == Kind::FFIType; }
};

class NamedType : public Type {
    std::string _name;
    Scope* _scope;

public:
    NamedType(std::string name, Scope* name_scope, Location location)
        : Type(Kind::Named, location), _name(std::move(name)), _scope(name_scope) {}

    auto name() const -> const std::string& { return _name; }

    auto scope() const -> Scope* { return _scope; }

    static bool classof(const Type* type) { return type->kind() == Kind::Named; }
};

class TypeWithOneElement : public Type {
    Type* _element_type;

protected:
    constexpr TypeWithOneElement(Kind kind, Location location, Type* element_type)
        : Type(kind, location), _element_type(element_type) {}

public:
    auto element_type() -> Type*& { return _element_type; }
    auto element_type() const -> Type* { return _element_type; }
    void element_type(Type* ty) { _element_type = ty; }

    static bool classof(const Type* type) {
        return type->kind() >= Kind::Pointer and type->kind() <= Kind::Array;
    }
};

class PointerType : public TypeWithOneElement {
public:
    constexpr PointerType(Type* element_type, Location location = {})
        : TypeWithOneElement(Kind::Pointer, location, element_type) {}

    static bool classof(const Type* type) { return type->kind() == Kind::Pointer; }
};

class ReferenceType : public TypeWithOneElement {
public:
    ReferenceType(Type* element_type, Location location)
        : TypeWithOneElement(Kind::Reference, location, element_type) {}

    static bool classof(const Type* type) { return type->kind() == Kind::Reference; }
};

class ArrayType : public TypeWithOneElement {
    Expr* _size;

public:
    ArrayType(Type* element_type, Expr* size, Location location = {})
        : TypeWithOneElement(Kind::Array, location, element_type), _size(size) {}

    /// Get the dimension of this array.
    auto dimension() const -> usz;

    auto size() -> Expr*& { return _size; }
    Expr* size() const { return _size; }

    static bool classof(const Type* type) { return type->kind() == Kind::Array; }
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
    auto params() -> std::vector<Param>& { return _params; }
    auto params() const -> const std::vector<Param>& { return _params; }

    /// Remove an attribute from this function.
    void remove_attr(FuncAttr attr) { _attributes.erase(attr); }

    /// Get the return type of this function.
    auto return_type() -> Type*& { return _return_type; }
    auto return_type() const { return _return_type; }

    /// Set an attribute on this function.
    void set_attr(FuncAttr attr) { _attributes[attr] = true; }

    static bool classof(const Type* type) { return type->kind() == Kind::Function; }
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

    auto members() -> std::vector<Member>& { return _members; }
    auto members() const -> const std::vector<Member>& { return _members; }

    static bool classof(const Type* type) { return type->kind() == Kind::Struct; }
};

class IntegerType : public Type {
    usz _bit_width;
    bool _is_signed;

public:
    IntegerType(usz bitWidth, bool isSigned, Location location)
        : Type(Kind::Integer, location), _bit_width(bitWidth), _is_signed(isSigned) {}

    bool is_signed() const { return _is_signed; }
    usz bit_width() const { return _bit_width; }

    static bool classof(const Type* type) { return type->kind() == Kind::Integer; }
};

/// \brief Base class for expression syntax nodes.
class Expr : public SemaNode {
public:
    /// Do NOT reorder these!
    enum struct Kind {
        While,
        For,
        Return,

        StructDecl,
        TypeAliasDecl,
        VarDecl,
        FuncDecl,

        IntegerLiteral,
        StringLiteral,
        CompoundLiteral,
        OverloadSet,
        EvaluatedConstant,

        If,
        Block,

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

    /// Try to evaluate this expression.
    ///
    /// \param ctx The context to use.
    /// \param out Outparameter for the result of the evaluation.
    /// \param required Whether to error if evaluation fails.
    /// \return Whether evaluation succeeded.
    bool evaluate(const Context* ctx, EvalResult& out, bool required);

    Kind kind() const { return _kind; }

    /// Check if this is an lvalue that can be assigned to.
    bool is_assignable_lvalue() const;

    /// Check if this is an lvalue. Only lvalues can have their
    /// address taken or be converted to references.
    bool is_lvalue() const;

    /// Access the location of this expression.
    auto location() const { return _location; }
    auto location(Location location) { _location = location; }

    auto type() const -> Type*;

    /// Deep-copy an expression.
    static Expr* Clone(Module& mod, Expr* expr);
};

class TypedExpr : public Expr {
    Type* _type;

protected:
    TypedExpr(Kind kind, Location location, Type* type = Type::Unknown)
        : Expr(kind, location), _type(type) {}

public:
    auto type() const -> Type* { return _type; }
    void type(Type* type) { _type = type; }

    /// Get a reference to the type of this expression.
    auto type_ref() -> Type** { return &_type; }

    static bool classof(const Expr* expr) {
        return expr->kind() >= Kind::StructDecl and expr->kind() <= Kind::MemberAccess;
    }
};

/// Base class for declarations. Declarations have a name.
class Decl : public TypedExpr {
    std::string _name;

protected:
    Decl(Kind kind, std::string name, Type* type, Location location)
        : TypedExpr(kind, location, type), _name(std::move(name)) {}

public:
    auto name() const -> const std::string& { return _name; }
    auto name(std::string name) { _name = std::move(name); }

    static bool classof(const Expr* expr) {
        return expr->kind() >= Kind::StructDecl and expr->kind() <= Kind::FuncDecl;
    }
};

/// A declaration that has linkage.
class ObjectDecl : public Decl {
    Module* _mod{};
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
    static bool classof(const Expr* expr) {
        return expr->kind() >= Kind::VarDecl and expr->kind() <= Kind::FuncDecl;
    }
};

class VarDecl : public ObjectDecl {
    Expr* _init{};

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

    auto init() -> Expr*& { return _init; }
    auto init() const { return _init; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::VarDecl; }
};

class FuncDecl : public ObjectDecl {
    Expr* _body{};
    Scope* _scope{};

    /// Only present if this is not an imported function. Sema
    /// fills these in. Furthermore, parameters with empty names
    /// do not get a decl.
    std::vector<VarDecl*> _params;

public:
    FuncDecl(
        std::string name,
        FuncType* type,
        Expr* body,
        Scope* scope,
        Module* mod,
        Linkage linkage,
        Location location
    ) : ObjectDecl(Kind::FuncDecl, type, std::move(name), mod, linkage, location),
        _body(body), _scope(scope) {
        mod->add_function(this);

        /// Functions receive special handling in sema and their types are
        /// always known when we actually start analysing code.
        set_sema_done();
    }

    auto body() -> Expr*& { return _body; }

    auto param_types() const {
        return as<FuncType>(type())->params() | vws::transform([](auto& p) { return p.type; });
    }

    auto param_decls() -> std::vector<VarDecl*>& { return _params; }
    auto param_decls() const -> const std::vector<VarDecl*>& { return _params; }

    auto scope(Scope* scope) { _scope = scope; }
    auto scope() const -> Scope* { return _scope; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::FuncDecl; }
};

class StructDecl : public Decl {
    /// The module this struct is declared in.
    Module* _module{};

public:
    StructDecl(Module* mod, std::string name, StructType* declared_type, Location location)
        : Decl(Kind::StructDecl, std::move(name), declared_type, location), _module(mod) {
        declared_type->associate_with_decl(this);
    }

    /// Get the module this struct is declared in.
    auto module() const -> Module* { return _module; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::StructDecl; }
};

class TypeAliasDecl : public Decl {
public:
    TypeAliasDecl(std::string name, Type* aliased_type, Location location)
        : Decl(Kind::TypeAliasDecl, std::move(name), aliased_type, location) {}

    static bool classof(const Expr* expr) { return expr->kind() == Kind::TypeAliasDecl; }
};

class IntegerLiteral : public TypedExpr {
    u64 _value;

public:
    IntegerLiteral(u64 value, Location location)
        : TypedExpr(Kind::IntegerLiteral, location, Type::Int), _value(value) {
        /// For now, there should be no way that the value could be out of range.
        set_sema_done();
    }

    u64 value() const { return _value; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::IntegerLiteral; }
};

class StringLiteral : public TypedExpr {
    usz _index;

public:
    /// Intern the given string and create a string literal for it.
    StringLiteral(Module& mod, std::string_view value, Location location);

    usz string_index() const { return _index; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::StringLiteral; }
};

class CompoundLiteral : public TypedExpr {
    std::vector<Expr*> _values;

public:
    CompoundLiteral(std::vector<Expr*> values, Location location)
        : TypedExpr(Kind::CompoundLiteral, location), _values(std::move(values)) {}

    auto values() -> std::vector<Expr*>& { return _values; }
    auto values() const -> const std::vector<Expr*>& { return _values; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::CompoundLiteral; }
};

/// A set of function overloads.
class OverloadSet : public TypedExpr {
    std::vector<FuncDecl*> _overloads;

public:
    OverloadSet(std::vector<FuncDecl*> overloads, Location location)
        : TypedExpr(Kind::OverloadSet, location), _overloads(std::move(overloads)) {}

    auto overloads() const -> const std::vector<FuncDecl*>& { return _overloads; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::OverloadSet; }
};

class IfExpr : public TypedExpr {
    Expr* _condition;
    Expr* _then;
    Expr* _else{};

public:
    IfExpr(Expr* condition, Expr* then, Expr* else_, Location location)
        : TypedExpr(Kind::If, location), _condition(condition), _then(then), _else(else_) {}

    auto condition() -> Expr*& { return _condition; }
    auto condition() const { return _condition; }

    auto then() -> Expr*& { return _then; }
    auto then() const { return _then; }

    auto else_() -> Expr*& { return _else; }
    auto else_() const { return _else; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::If; }
};

class Loop : public Expr {
    Expr* _body;
    Expr* _condition;

public:
    Loop(Kind kind, Expr* condition, Expr* body, Location location)
        : Expr(kind, location), _body(body), _condition(condition) {}

    auto body() const { return _body; }
    auto body() -> Expr*& { return _body; }

    auto condition() const { return _condition; }
    auto condition() -> Expr*& { return _condition; }

    static bool classof(const Expr* expr) {
        return expr->kind() >= Kind::While and expr->kind() <= Kind::For;
    }
};

class WhileExpr : public Loop {
public:
    WhileExpr(Expr* condition, Expr* body, Location location)
        : Loop(Kind::While, condition, body, location) {}

    static bool classof(const Expr* expr) { return expr->kind() == Kind::While; }
};

class ForExpr : public Loop {
    Expr* _init{};
    Expr* _increment{};

public:
    ForExpr(Expr* init, Expr* condition, Expr* increment, Expr* body, Location location)
        : Loop(Kind::For, condition, body, location), _init(init), _increment(increment) {}

    auto init() -> Expr*& { return _init; }
    auto init() const { return _init; }

    auto increment() -> Expr*& { return _increment; }
    auto increment() const { return _increment; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::For; }
};

class BlockExpr : public TypedExpr {
    std::vector<Expr*> _children;

public:
    BlockExpr(std::vector<Expr*> children, Location location)
        : TypedExpr(Kind::Block, location), _children(std::move(children)) {}

    /// Add an expression to this block.
    void add(Expr* expr) { _children.push_back(expr); }

    auto children() -> std::vector<Expr*>& { return _children; }
    auto children() const -> const std::vector<Expr*>& { return _children; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::Block; }
};

class ReturnExpr : public Expr {
    Expr* _value{};

public:
    ReturnExpr(Expr* value, Location location)
        : Expr(Kind::Return, location), _value(value) {}

    auto value() -> Expr*& { return _value; }
    auto value() const { return _value; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::Return; }
};

/// Expression that has been evaluated by sema, together w/ a cached value.
class ConstantExpr : public TypedExpr {
    EvalResult _value;
    Expr* _expression;

public:
    ConstantExpr(Expr* expr, EvalResult value)
        : TypedExpr(Kind::EvaluatedConstant, expr->location(), expr->type()),
          _value(std::move(value)),
          _expression(expr) {
        LCC_ASSERT(expr->ok());
        expr->set_sema_done();
    }

    auto expr() const { return _expression; }
    auto value() -> EvalResult& { return _value; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::EvaluatedConstant; }
};

class CallExpr : public TypedExpr {
    Expr* _callee;
    std::vector<Expr*> _args;

public:
    CallExpr(Expr* callee, std::vector<Expr*> args, Location location)
        : TypedExpr(Kind::Call, location), _callee(callee), _args(std::move(args)) {}

    auto callee() -> Expr*& { return _callee; }
    auto callee() const { return _callee; }

    auto args() -> std::vector<Expr*>& { return _args; }
    auto args() const -> const std::vector<Expr*>& { return _args; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::Call; }
};

class IntrinsicCallExpr : public TypedExpr {
    IntrinsicKind _kind;
    std::vector<Expr*> _args;

public:
    IntrinsicCallExpr(IntrinsicKind kind, std::vector<Expr*> args)
        : TypedExpr(Kind::IntrinsicCall, {}), _kind(kind), _args(std::move(args)) {}

    auto args() -> std::vector<Expr*>& { return _args; }
    auto args() const -> const std::vector<Expr*>& { return _args; }

    auto intrinsic_kind() const { return _kind; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::IntrinsicCall; }
};

class CastExpr : public TypedExpr {
    Expr* _value;
    CastKind _cast_kind;

public:
    CastExpr(Expr* value, Type* ty, CastKind k, Location location)
        : TypedExpr(Kind::Cast, location, ty), _value(value), _cast_kind(k) {}

    auto cast_kind() const { return _cast_kind; }

    /// Check cast kinds.
    bool is_hard_cast() const { return _cast_kind == CastKind::HardCast; }
    bool is_implicit_cast() const { return _cast_kind == CastKind::ImplicitCast; }
    bool is_lvalue_to_rvalue() const { return _cast_kind == CastKind::LValueToRValueConv; }
    bool is_soft_cast() const { return _cast_kind == CastKind::SoftCast; }

    /// Get the operand of this expression.
    auto operand() -> Expr*& { return _value; }
    auto operand() const { return _value; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::Cast; }
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
    auto operand() -> Expr*& { return _operand; }
    auto operand() const { return _operand; }

    /// Get the unary operator.
    auto op() const { return _op; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::Unary; }
};

class BinaryExpr : public TypedExpr {
    Expr* _lhs;
    Expr* _rhs;
    TokenKind _op;

public:
    BinaryExpr(TokenKind op, Expr* lhs, Expr* rhs, Location location)
        : TypedExpr(Kind::Binary, location), _lhs(lhs), _rhs(rhs), _op(op) {}

    /// Get the left-hand side of this expression.
    auto lhs() -> Expr*& { return _lhs; }
    auto lhs() const { return _lhs; }

    /// Get the right-hand side of this expression.
    auto rhs() -> Expr*& { return _rhs; }
    auto rhs() const { return _rhs; }

    /// Get the binary operator.
    auto op() const { return _op; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::Binary; }
};

class NameRefExpr : public TypedExpr {
    std::string _name;
    Scope* _scope;
    Expr* _target{};

public:
    NameRefExpr(std::string name, Scope* name_scope, Location location)
        : TypedExpr(Kind::NameRef, location), _name(std::move(name)), _scope(name_scope) {}

    auto name() const -> const std::string& { return _name; }
    auto scope() const -> Scope* { return _scope; }
    auto target() const -> Expr* { return _target; }
    void target(Expr* target) { _target = target; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::NameRef; }
};

class MemberAccessExpr : public TypedExpr {
    Expr* _object;
    std::string _name;
    StructType* _struct{};
    usz _member_index{};

public:
    MemberAccessExpr(Expr* object, std::string name, Location location)
        : TypedExpr(Kind::MemberAccess, location), _object(object), _name(std::move(name)) {}

    void finalise(StructType* type, usz member_index) {
        _member_index = member_index;
        _struct = type;
    }

    usz member() const { return _member_index; }

    auto name() const -> const std::string& { return _name; }

    auto object() -> Expr*& { return _object; }
    auto object() const { return _object; }
    auto object(Expr* object) { _object = object; }

    auto struct_type() const { return _struct; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::MemberAccess; }
};
} // namespace lcc::intercept

#endif // INTERCEPT_AST_HH
