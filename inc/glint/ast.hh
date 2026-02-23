#ifndef GLINT_AST_HH
#define GLINT_AST_HH

#include <lcc/core.hh>
#include <lcc/diags.hh>
#include <lcc/syntax/token.hh>
#include <lcc/utils.hh>
#include <lcc/utils/aint.hh>
#include <lcc/utils/fractionals.hh>
#include <lcc/utils/result.hh>

#include <glint/eval.hh>

#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

namespace lcc::glint {
static constexpr std::string_view metadata_section_name{".glint"};
static constexpr std::string_view metadata_file_extension{".gmeta"};

// Forward declaration
class Init;

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
    COUNT
};

enum struct TokenKind {
    Invalid,
    Eof,

    LParen,
    RParen,
    LBrack,
    Subscript = LBrack,
    RBrack,
    LBrace,
    RBrace,

    BangLBrace, // !{
    // HashLBrace, // #{

    Comma,     // ,
    Colon,     // :
    Semicolon, // ;
    Dot,       // .

    Plus, // +
    Add = Plus,
    Minus, // -
    Subtract = Minus,
    Star, // *
    Multiply = Star,
    Slash, // /
    Divide = Slash,
    Percent, // %
    Modulo = Percent,
    BitAND,    // bit&
    BitOR,     // bit|
    BitXOR,    // bit^
    BitNOT,    // bit~
    Ampersand, // &
    Pipe,      // |
    Caret,     // ^
    Tilde,     // ~
    Exclam,    // !
    At,        // @
    Dereference = At,
    Hash, // #

    Shl,
    Shr,

    Eq, // =
    Ne, // !=
    Lt, // <
    Gt, // >
    Le, // <=
    Ge, // >=

    // DotDot,      // ..  range binary?
    // HashPlus,    // #+  compile-time unary prefix?
    // DoubleArrow, // =>
    // PipeGt,      // |>
    // PipeLt,      // <|
    // DotEq,       // .=
    // AtEq,        // @=
    // HashEq,      // #=
    // VCaret,      // v^  zip binary?
    // BangGt,      // !>
    // LParenGt,    // (>
    // RParenLt,    // <)
    // LParenPipe,  // (|
    // RParenPipe,  // |)
    // LBrackGt,    // [>
    // RBrackLt,    // <]
    // LBraceGt,    // {>
    // RBraceLt,    // <}
    // PipeShip,    // <|>
    // EqShip,      // <=>
    // BangShip,    // <!>
    PlusPlus, // ++
    Increment = PlusPlus,
    MinusMinus, // --
    Decrement = MinusMinus,
    StarStar,    // **
    PlusEq,      // +=
    MinusEq,     // -=
    StarEq,      // *=
    SlashEq,     // /=
    PercentEq,   // %=
    AmpersandEq, // &=
    PipeEq,      // |=
    CaretEq,     // ^=
    TildeEq,     // ~=
    LBrackEq,    // [=
    ColonEq,     // :=
    ColonColon,  // ::
    RightArrow,  // ->

    Ident,
    Integer,
    Fractional,
    String,
    ByteLiteral,

    Apply,
    If,
    Else,
    While,
    Void,
    Byte,
    Bool,
    External, // external
    True,     // true
    False,    // false
    And,      // and
    Or,       // or
    Int,      // int
    Float,    // float
    UInt,     // uint
    ArbitraryInt,
    Sizeof,
    Alignof,
    Typeof,
    Has,
    For,       // cfor
    RangedFor, // for
    Return,
    Export,
    Struct,
    Enum,
    Union,
    Sum,
    Lambda,
    Supplant,
    Match,
    Switch,
    Print,
    Template,

    CShort,     // cshort
    CUShort,    // cushort
    CInt,       // cint
    CUInt,      // cuint
    CLong,      // clong
    CULong,     // culong
    CLongLong,  // clonglong
    CULongLong, // culonglong

    Gensym,
    MacroArg,

    Expression,
};

enum class CastKind {
    SoftCast,           ///< Explicit cast.
    HardCast,           ///< Explicit cast.
    ImplicitCast,       ///< Implicit conversion.
    LValueToRValueConv, ///< Lvalue-to-rvalue conversion.
    LValueToReference,  ///< Lvalue-to-reference conversion.
    ReferenceToLValue,  ///< Reference-to-lvalue conversion.
};

/// Convert a token kind to a human-readable string representation.
auto ToString(TokenKind kind) -> std::string_view;

class Module {
public:
    struct Ref {
        std::string name;
        Module* module;
        Location location;
        std::string aliased_name;

        // STYLE NOTE: Because this is a struct where we are meant to access the members
        // directly, we use a trailing underscore to disambiguate.
        Ref(std::string name_, Module* module_, Location location_)
            : name(std::move(name_)),
              module(module_),
              location(location_),
              aliased_name("") {}

        Ref(std::string name_, Module* module_, std::string alias_, Location location_)
            : name(std::move(name_)),
              module(module_),
              location(location_),
              aliased_name(alias_) {}
    };

private:
    // STYLE NOTE: Private members with public accessors should be prefixed
    // with an underscore to disambiguate them.
    std::string _name{};

    FuncDecl* _top_level_function{};

    // In Glint, there is a distinction between a module which may be imported
    // and an executable program.
    bool _is_module{false};

    File* _file;

    std::vector<Ref> _imports{};
    std::vector<Decl*> _exports{};
    std::vector<FuncDecl*> _functions{};

    usz _unique_counter = 0;

public:
    enum ModuleStatus : bool {
        IsNotAModule = false,
        IsAModule = true,

        IsNotAnExecutable = true,
        IsAnExecutable = false
    };
    Module(
        File* file,
        std::string module_name,
        ModuleStatus is_logical_module
    );

    ~Module();

    /// Add an export.
    void add_export(Decl* decl) { _exports.push_back(decl); }

    /// Add a function to this module.
    /// TODO: I don't think this should be public, since we call it from the
    /// FuncDecl constructor...
    void add_function(FuncDecl* func) { _functions.push_back(func); }

    // Add an import
    void add_import(std::string module_name, Location location = {}) {
        _imports.emplace_back(std::move(module_name), nullptr, location);
    }

    // Add an import
    void add_import(std::string module_name, std::string alias, Location location = {}) {
        _imports.emplace_back(std::move(module_name), nullptr, alias, location);
    }

    /// Add a top-level expression.
    void add_top_level_expr(Expr* node);

    /// Get the functions that are part of this module.
    auto functions() -> decltype(_functions)& { return _functions; }
    auto functions() const -> const decltype(_functions)& { return _functions; }

    // Get a list of functions by their name
    auto function(std::string_view name) -> std::vector<FuncDecl*>;

    /// Get the module imports.
    auto imports() -> decltype(_imports)& { return _imports; }
    auto imports() const -> const decltype(_imports)& {
        return _imports;
    }

    /// Get the module imports.
    auto exports() -> std::vector<Decl*>& { return _exports; }

    /// Intern a string and return its index.
    [[nodiscard]]
    auto intern(std::string_view str) -> usz;

    /// Print this module.
    auto string(bool use_colour) const -> std::string;

    /// Print this module.
    void print(bool use_colour);

    /// Get the global scope.
    [[nodiscard]]
    auto global_scope() const -> Scope* { return scopes.at(0); }

    /// Get the top-level scope.
    [[nodiscard]]
    auto top_level_scope() const -> Scope* { return scopes.at(1); }

    /// Get the scope that opens the least distance preceding the given location.
    [[nodiscard]]
    auto enclosing_scope(Location l) const -> Scope*;

    /// Get the top-level function.
    [[nodiscard]]
    auto top_level_function() const { return _top_level_function; }

    // Get a variable name that is unique within this module.
    // If passed a value, it is guaranteed to show up somewhere in the output.
    // Useful for giving at least /some/ semantic meaning to the guaranteed-
    // unique name.
    auto unique_name(std::string v = "tmp") { return fmt::format("_XGlint__{}{}", v, _unique_counter++); }

    /// Get a function name that is unique within this module.
    auto unique_function_name() { return unique_name("lambda_"); }

    // Get the name of this module
    [[nodiscard]]
    auto name() const -> std::string_view { return _name; }

    [[nodiscard]]
    auto is_module() const { return _is_module; }

    /// Obtain a binary blob describing this Glint module.
    [[nodiscard]]
    auto serialise() -> std::vector<u8>;

    [[nodiscard]]
    auto serialise(
        std::vector<u8>& out,
        std::vector<Type*>& cache,
        std::vector<Type*>& current,
        // indices calculated ahead of time
        std::unordered_map<Type*, u16> indices,
        Type*
    ) -> lcc::u16;

    struct TypeSerialisationContext {
        std::vector<u8>& out;
        std::vector<Type*>& cache;
        std::vector<Type*>& current;
        const std::unordered_map<Type*, u16>& indices;
    };

    [[nodiscard]]
    auto serialise_expr(
        std::vector<u8>& out,
        std::vector<Expr*>& cache,
        std::vector<Expr*>& current,
        // indices calculated ahead of time
        std::unordered_map<Expr*, u16> indices,
        TypeSerialisationContext& type_context,
        Expr*
    ) -> lcc::u16;

    /// Deserialise a module metadata blob into `this`.
    /// \return a boolean value denoting `true` iff deserialisation succeeded.
    /// NOTE: Does not clear out module before deserialising into `this`.
    [[nodiscard]]
    auto deserialise(
        lcc::Context*,
        std::vector<u8> module_metadata_blob
    ) -> bool;

    // After deserialising all expressions, walk each one and build/fixup
    // scopes.
    void scope_walk(
        lcc::Context*,
        std::unordered_set<Expr*>&,
        Expr*,
        Scope*
    );

    /// Convert a type back to the source it may have been lexed from.
    [[nodiscard]]
    auto ToSource(const Type& t) -> Result<std::string>;

    /// Convert an AST expression node into the Glint source it may have been
    /// parsed from.
    [[nodiscard]]
    auto ToSource(const Expr& e) -> Result<std::string>;

    [[nodiscard]]
    static auto InitFunctionName(std::string_view module_name) -> std::string {
        return fmt::format("_XGlint__init_{}", module_name);
    }

    std::vector<Expr*> nodes;
    std::vector<Type*> types;
    std::vector<Scope*> scopes;
    std::vector<std::string> strings;
};

struct GlintToken : public syntax::Token<TokenKind> {
    Expr* expression{};

    /// Whether the expression bound by this token should
    /// only be evaluated once.
    bool eval_once = true;

    // Whether the token was expanded from a macro.
    bool from_macro = false;

    bool operator==(const GlintToken& rhs) const;
};

/// Convert a token back to the source it may have been lexed from.
[[nodiscard]]
auto ToSource(const GlintToken& t) -> Result<std::string>;

struct Macro {
    std::string name;
    std::vector<GlintToken*> parameters;
    std::vector<GlintToken*> expansion;
    Location location;
    usz gensym_count;
};

class Scope {
    Scope* _parent;
    // This has to be a multimap to support function overloads having the same
    // name yet resolving to different declarations.
    std::unordered_multimap<std::string, Decl*, detail::StringHash, std::equal_to<>>
        symbols;
    bool is_function_scope = false;

    Location _location;

public:
    explicit Scope(Scope* parent) : _parent(parent) {
        // Assert no recursive scopes.
        auto p = parent;
        while (p) {
            LCC_ASSERT(
                p != this,
                "Stopping attempt to create recursive scope"
            );
            p = p->_parent;
        }
    }

    /// Disallow creating scopes without a module reference.
    void* operator new(size_t) = delete;
    void* operator new(size_t sz, Module& mod) {
        auto ptr = ::operator new(sz);
        mod.scopes.push_back(static_cast<Scope*>(ptr));
        return ptr;
    }

    /// Get the parent scope.
    auto parent() const { return _parent; }

    /// Get the location.
    auto location() const { return _location; }

    /// Get the location.
    auto location() -> Location& { return _location; }

    // FIXME: The entire decl system is a fucking mess, an absolute joke.
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
        Context* ctx,
        std::string&& name,
        Decl* decl
    ) -> Result<Decl*>;

    // Look up a symbol in this scope.
    std::vector<Decl*> find(std::string_view name) const {
        // std::pair can go die in a fucking hole. If you want to be a LISP so bad
        // just fucking be one, loser.
        auto it = symbols.equal_range(name);
        // From the standard:
        // "If there are no such elements, past-the-end (see end()) iterators are
        // returned as both elements of the pair."
        if (it.first == it.second) return {};

        std::vector<Decl*> out{};
        for (auto decl = it.first; decl != it.second; ++decl)
            out.emplace_back(decl->second);

        return out;
    }

    std::vector<Decl*> find_recursive(std::string_view name) const {
        auto f = find(name);
        if (f.empty() and parent())
            return parent()->find_recursive(name);
        return f;
    }

    // Get a list of symbols defined in this scope (for calculating
    // levenshtein distance on an unknown symbol, for example).
    // NOTE: This returns a view of the data in the symbols, so if you alter
    // those you may very well have a bad time.
    auto all_symbols() const -> const std::unordered_set<Decl*> {
        std::unordered_set<Decl*> out{};
        for (auto& [_, decl] : symbols)
            out.emplace(decl);
        return out;
    }

    // \see all_symbols()
    auto all_symbols_recursive() const -> const std::unordered_set<Decl*> {
        auto out = all_symbols();
        if (parent()) {
            auto parent_symbols = parent()->all_symbols_recursive();
            out.insert(parent_symbols.begin(), parent_symbols.end());
        }
        return out;
    }

    /// Mark this scope as a function scope.
    void set_function_scope() { is_function_scope = true; }
};

// Base class for nodes and types, i.e. for anything that
// can be analysed in sema.
class SemaNode {
public:
    /// State of semantic analysis for an expression or type.
    enum struct State {
        /// Not yet analysed by sema. The type of this node is unspecified.
        NotAnalysed,

        /// Sema is currently in progress. This can be used to detect cycles.
        InProgress,

        // Something has happened in the language that makes referring to this
        // node an error; this is applied to the target of variables that have
        // been freed, for example.
        NoLongerViable,

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

    Location _location = {};

protected:
    constexpr SemaNode() = default;
    constexpr SemaNode(Location location) : _location(location) {}

public:
    // Check if this expression was successfully analysed by sema.
    bool ok() const {
        return _state == State::Done
            or _state == State::NoLongerViable;
    }

    Location location() const {
        return _location;
    }

    Location location(Location new_location) {
        _location = new_location;
        return _location;
    }

    // Get the state of semantic analysis for this node.
    // \see SemaNode::State
    constexpr auto sema() const -> State { return _state; }

    // Check if sema has errored.
    constexpr bool sema_errored() const { return _state == State::Errored; }

    // \see SemaNode::State
    constexpr bool sema_done_or_errored() const {
        return _state == State::Done or _state == State::Errored;
    }

    // \see SemaNode::State
    constexpr bool sema_no_longer_viable() const {
        return _state == State::NoLongerViable;
    }

    // \see SemaNode::State
    constexpr void set_sema_in_progress() {
        LCC_ASSERT(not sema_done_or_errored());
        _state = State::InProgress;
    }

    // \see SemaNode::State
    constexpr void set_sema_no_longer_viable() {
        LCC_ASSERT(ok(), "Cannot mark non-checked node as no longer viable");
        _state = State::NoLongerViable;
    }

    // \see SemaNode::State
    constexpr void set_sema_done() {
        LCC_ASSERT(not sema_errored());
        _state = State::Done;
    }

    // \see SemaNode::State
    constexpr void set_sema_errored() {
        LCC_ASSERT(_state != State::Done);
        _state = State::Errored;
    }
};

class Type : public SemaNode {
    friend lcc::glint::Init;

public:
    enum struct Kind {
        // Glint's built-in types.
        Builtin,
        // "foreign function interface" types; types for interacting with other
        // languages (like C).
        FFIType,
        // A type that represents another type.
        // Basically, a TypeExpr is of `type` type, and contains a type of some
        // other type. This allows unique operations on types as values vs values
        // of those types.
        Type,
        // Named types get resolved to the type the name is bound to.
        Named,
        // A (possibly null) memory address.
        Pointer,
        // Like a pointer, but non-nullable.
        Reference,
        // Like a struct with .data, .size, and .capacity members, allocates.
        DynamicArray,
        // Fixed size array.
        Array,
        // Like a struct with .data and .size members, no allocations.
        ArrayView,
        // A callable.
        Function,
        // SAFE Single memory location shared by multiple types.
        Sum,
        // Single memory location shared by multiple types.
        Union,
        // Named constants.
        Enum,
        // A structure composed of other types.
        Struct,
        TemplatedStruct,
        // Regular-old integer type; what you get when you type int.
        Integer,
        // A type that corresponds to the type of the expression inside; used to
        // transfer the expression through to sema, where it may be properly
        // replaced with the type expression of it's expression's type.
        Typeof,
    };

private:
    Kind _kind;

protected:
    constexpr Type(Kind kind, Location location)
        : SemaNode(location), _kind(kind) {}

public:
    virtual ~Type() = default;

    auto operator new(size_t) -> void* = delete;
    auto operator new(size_t sz, Module& mod) -> void* {
        auto* ptr = ::operator new(sz);
        mod.types.push_back(static_cast<Type*>(ptr));
        return ptr;
    }

    /// Get the alignment of this type. It may be target-dependent,
    /// which is why this takes a context parameter.
    ///
    /// \param ctx The context to use.
    /// \return The alignment of this type, in bits.
    usz align(const Context* ctx) const;

    /// Get the element type of this type. This will assert that this type does
    /// have an element type.
    [[nodiscard]]
    auto elem() const -> Type*;

    /// Get a reference to the element type of this type. This will assert that
    /// this type does have an element type.
    [[nodiscard]]
    auto elem_ref() -> Type**;

    /// Get the kind of this type.
    [[nodiscard]]
    auto kind() const { return _kind; }

    /// Check if this is an array type.
    [[nodiscard]]
    auto is_array() const -> bool { return _kind == Kind::Array; }

    /// Check if this is a dynamic array type.
    [[nodiscard]]
    auto is_dynamic_array() const -> bool { return _kind == Kind::DynamicArray; }

    /// Check if this is a view type.
    [[nodiscard]]
    auto is_view() const -> bool { return _kind == Kind::ArrayView; }

    /// Check if this is the builtin \c bool type.
    [[nodiscard]]
    auto is_bool() const -> bool;

    /// Check if this is the builtin \c byte type.
    [[nodiscard]]
    auto is_byte() const -> bool;

    /// Check if this is a builtin type.
    [[nodiscard]]
    auto is_builtin() const -> bool { return _kind == Kind::Builtin; }

    /// Check if this is an enum type.
    [[nodiscard]]
    auto is_enum() const -> bool { return _kind == Kind::Enum; }

    /// Check if this is a function type.
    [[nodiscard]]
    auto is_function() const -> bool { return _kind == Kind::Function; }

    /// Returns true if this is a sized integer type, a C
    /// FFI integer type, \c int, or \c byte, or \c bool, if
    /// \c include_bool is true.
    [[nodiscard]]
    auto is_integer(bool include_bool = false) const -> bool;

    /// Check if this is a pointer type.
    [[nodiscard]]
    auto is_pointer() const -> bool { return _kind == Kind::Pointer; }

    /// Check if this is a reference type.
    [[nodiscard]]
    auto is_reference() const -> bool { return _kind == Kind::Reference; }

    /// Check if this is a signed integer type.
    [[nodiscard]]
    auto is_signed_int(const Context* ctx) const -> bool;

    /// Check if this is a sized integer type.
    [[nodiscard]]
    auto is_sized_integer() const -> bool { return _kind == Kind::Integer; }

    /// Check if this is a struct type.
    [[nodiscard]]
    auto is_struct() const -> bool { return _kind == Kind::Struct; }

    /// Check if this is a struct type.
    [[nodiscard]]
    auto is_sum_type() const -> bool { return _kind == Kind::Sum; }

    /// Check if this is the uninitialised type.
    [[nodiscard]]
    auto is_unknown() const -> bool;

    /// Check if this is an unsigned integer type.
    [[nodiscard]]
    auto is_unsigned_int(const Context* ctx) const -> bool;

    /// Check if this is the builtin void type.
    [[nodiscard]]
    auto is_void() const -> bool;

    /// Check if this is the builtin overload set type.
    [[nodiscard]]
    auto is_overload_set() const -> bool;

    /// Check if this is a compound type (contains other types).
    [[nodiscard]]
    auto is_compound_type() const -> bool;

    /// Return types contained by this type, if any.
    /// If \c is_compound_type() returns true, this will return a non-zero
    /// amount of types.
    /// NOTE: NOT SUITABLE FOR PROPER CONVERSION (i.e. a struct of four bytes
    /// and an array of byte of size four will not return the same thing).
    [[nodiscard]]
    auto types() const -> std::vector<Type*>;

    /// Return referenced to types contained by this type, if any.
    /// @see types()
    [[nodiscard]]
    auto types_ref() -> std::vector<Type**>;

    // Get the identifier-friendly encoding of this type.
    auto representation(
        std::unordered_set<const Type*> containing_types = {}
    ) const -> std::string;

    /// Get the size of this type. It may be target-dependent,
    /// which is why this takes a context parameter.
    ///
    /// \param ctx The context to use.
    /// \return The size of this type, in bits.
    [[nodiscard]]
    auto size(const Context* ctx) const -> usz;

    /// Get the minimum amount of bytes required to represent this type.
    /// Implemented in terms of `size()`.
    ///
    /// \param ctx The context to use.
    /// \return The minimum amount of bytes required to represent an
    ///         instance of this type.
    [[nodiscard]]
    auto size_in_bytes(const Context* ctx) const -> usz {
        return (size(ctx) / 8) + (size(ctx) % 8 ? 1 : 0);
    }

    /// Get a string representation of this type.
    [[nodiscard]]
    auto string(bool use_colours = false) const -> std::string;

    /// Return this type stripped of any pointers and references.
    [[nodiscard]]
    auto strip_pointers_and_references() -> Type*;

    /// Return this type stripped of any references.
    [[nodiscard]]
    auto strip_references() -> Type*;

    /// It’s way too easy to accidentally write `a == b` when
    /// you really meant `*a == *b`, so we don’t allow this.
    [[nodiscard]] auto operator==(const Type& other) const -> bool
        = delete;

    /// Check if types are equal to each other.
    [[nodiscard]]
    static auto Equal(const Type* a, const Type* b) -> bool;

    /// Use these only if there is no location information
    /// available (e.g. for default initialisers etc.). In
    /// any other case, prefer to create an instance of a
    /// BuiltinType instead.
    ///
    /// Do NOT compare against these w/ `==`!
    /// Use Type::Equal() static method instead.
    ///
    /// When adding a type here, don’t forget to initialise
    /// it in Context::InitialiseLCCData().
    static Type* const Bool;
    static Type* const Byte;
    static Type* const Int;
    static Type* const UInt;
    static Type* const Float;
    static Type* const Unknown;
    static Type* const Void;
    static Type* const VoidPtr;

    /// The type of an unresolved overload set.
    static Type* const OverloadSet;
};

static constexpr auto ToString(Type::Kind k) {
    switch (k) {
        case Type::Kind::Builtin: return "builtin";
        case Type::Kind::FFIType: return "ffi";
        case Type::Kind::Type: return "type";
        case Type::Kind::Named: return "named";
        case Type::Kind::Pointer: return "ptr";
        case Type::Kind::Reference: return "ref";
        case Type::Kind::DynamicArray: return "dynamic_array";
        case Type::Kind::Array: return "array";
        case Type::Kind::ArrayView: return "array_view";
        case Type::Kind::Function: return "function";
        case Type::Kind::Sum: return "sum";
        case Type::Kind::Union: return "union";
        case Type::Kind::Enum: return "enum";
        case Type::Kind::Struct: return "struct";
        case Type::Kind::TemplatedStruct: return "templated_struct";
        case Type::Kind::Integer: return "integer";
        case Type::Kind::Typeof: return "typeof";
    }
    LCC_UNREACHABLE();
}

/// The size, alignment, etc of a primitive type all depend on the target,
/// and there is no point in storing their names in each of them, so an
/// instance of one only ever stores its primitive type kind.
class BuiltinType : public Type {
    friend lcc::glint::Init;

public:
    enum struct BuiltinKind {
        Unknown,
        Bool,
        Byte,
        Int,
        UInt,
        Float,
        Void,
        OverloadSet,
    };

private:
    /// Shorten long signatures w/ this.
    using K = BuiltinKind;

    BuiltinKind _kind;

    constexpr BuiltinType(K k, Location location)
        : Type(Kind::Builtin, location), _kind(k) {
        set_sema_done();
    }

public:
    /// Get the kind of this builtin.
    [[nodiscard]]
    auto builtin_kind() const -> BuiltinKind { return _kind; }

    [[nodiscard]]
    auto
    operator==(BuiltinKind k) const -> bool { return _kind == k; }

    [[nodiscard]]
    static auto Make(Module& mod, K k, Location location) -> BuiltinType* {
        return new (mod) BuiltinType(k, location);
    }

    /// Get instances of primitive types.
    [[nodiscard]]
    static auto Unknown(Module& mod, Location l = {}) -> BuiltinType* { return Make(mod, K::Unknown, l); }
    [[nodiscard]]
    static auto Bool(Module& mod, Location l = {}) -> BuiltinType* { return Make(mod, K::Bool, l); }
    [[nodiscard]]
    static auto Byte(Module& mod, Location l = {}) -> BuiltinType* { return Make(mod, K::Byte, l); }
    [[nodiscard]]
    static auto Int(Module& mod, Location l = {}) -> BuiltinType* { return Make(mod, K::Int, l); }
    [[nodiscard]]
    static auto UInt(Module& mod, Location l = {}) -> BuiltinType* { return Make(mod, K::UInt, l); }
    [[nodiscard]]
    static auto Float(Module& mod, Location l = {}) -> BuiltinType* { return Make(mod, K::Float, l); }
    [[nodiscard]]
    static auto Void(Module& mod, Location l = {}) -> BuiltinType* { return Make(mod, K::Void, l); }

    [[nodiscard]]
    static auto classof(const Type* type) -> bool {
        return type->kind() == Kind::Builtin;
    }
};

class IntegerType : public Type {
    usz _bit_width;
    bool _is_signed;

public:
    constexpr IntegerType(usz bitWidth, bool isSigned, Location location)
        : Type(Kind::Integer, location), _bit_width(bitWidth), _is_signed(isSigned) {}

    [[nodiscard]]
    auto is_signed() const { return _is_signed; }
    [[nodiscard]]
    auto bit_width() const { return _bit_width; }

    [[nodiscard]]
    static auto classof(const Type* type) -> bool {
        return type->kind() == Kind::Integer;
    }
};

class TypeofType : public Type {
    Expr* _expression;

public:
    constexpr TypeofType(Expr* e, Location location)
        : Type(Kind::Typeof, location), _expression(e) {}

    [[nodiscard]]
    auto expression() const { return _expression; }

    [[nodiscard]]
    auto expression() -> Expr*& { return _expression; }

    [[nodiscard]]
    static auto classof(const Type* type) -> bool {
        return type->kind() == Kind::Typeof;
    }
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

    K kind;
    constexpr FFIType(K k, Location location) : Type(Kind::FFIType, location), kind(k) {}

public:
    /// Get the kind of this C FFI type.
    [[nodiscard]]
    auto ffi_kind() const -> FFIKind { return kind; }

    [[nodiscard]] auto operator==(FFIKind k) const -> bool {
        return kind == k;
    }

    [[nodiscard]]
    static auto Make(Module& mod, K k, Location location) -> FFIType* {
        return new (mod) FFIType(k, location);
    }

    /// Get instances of C FFI types.
    [[nodiscard]]
    static auto CChar(Module& mod, Location l = {}) -> FFIType* { return Make(mod, K::CChar, l); }
    [[nodiscard]]
    static auto CSChar(Module& mod, Location l = {}) -> FFIType* { return Make(mod, K::CSChar, l); }
    [[nodiscard]]
    static auto CUChar(Module& mod, Location l = {}) -> FFIType* { return Make(mod, K::CUChar, l); }
    [[nodiscard]]
    static auto CShort(Module& mod, Location l = {}) -> FFIType* { return Make(mod, K::CShort, l); }
    [[nodiscard]]
    static auto CUShort(Module& mod, Location l = {}) -> FFIType* { return Make(mod, K::CUShort, l); }
    [[nodiscard]]
    static auto CInt(Module& mod, Location l = {}) -> FFIType* { return Make(mod, K::CInt, l); }
    [[nodiscard]]
    static auto CUInt(Module& mod, Location l = {}) -> FFIType* { return Make(mod, K::CUInt, l); }
    [[nodiscard]]
    static auto CLong(Module& mod, Location l = {}) -> FFIType* { return Make(mod, K::CLong, l); }
    [[nodiscard]]
    static auto CULong(Module& mod, Location l = {}) -> FFIType* { return Make(mod, K::CULong, l); }
    [[nodiscard]]
    static auto CLongLong(Module& mod, Location l = {}) -> FFIType* { return Make(mod, K::CLongLong, l); }
    [[nodiscard]]
    static auto CULongLong(Module& mod, Location l = {}) -> FFIType* { return Make(mod, K::CULongLong, l); }

    [[nodiscard]]
    static auto classof(const Type* type) -> bool {
        return type->kind() == Kind::FFIType;
    }
};

class NamedType : public Type {
    std::string _name;
    Scope* _scope;

public:
    NamedType(std::string name, Scope* name_scope, Location location)
        : Type(Kind::Named, location), _name(std::move(name)), _scope(name_scope) {}

    [[nodiscard]]
    auto name() const { return _name; }

    [[nodiscard]]
    auto scope() const { return _scope; }

    [[nodiscard]]
    static auto classof(const Type* type) -> bool {
        return type->kind() == Kind::Named;
    }
};

class TypeWithOneElement : public Type {
    Type* _element_type;

protected:
    constexpr TypeWithOneElement(Kind kind, Location location, Type* element_type)
        : Type(kind, location), _element_type(element_type) {}

public:
    [[nodiscard]]
    auto element_type() -> Type*& { return _element_type; }
    [[nodiscard]]
    auto element_type() const -> Type* { return _element_type; }
    void element_type(Type* ty) { _element_type = ty; }

    [[nodiscard]]
    static auto classof(const Type* type) -> bool {
        return type->kind() == Kind::Pointer
            or type->kind() == Kind::Reference
            or type->kind() == Kind::DynamicArray
            or type->kind() == Kind::Array;
    }
};

class TypeType : public Type {
public:
    constexpr TypeType(Location location)
        : Type(Kind::Type, location) {
        set_sema_done();
    }

    [[nodiscard]]
    static auto classof(const Type* type) -> bool {
        return type->kind() == Kind::Type;
    }
};

class PointerType : public TypeWithOneElement {
public:
    explicit constexpr PointerType(Type* element_type, Location location = {})
        : TypeWithOneElement(Kind::Pointer, location, element_type) {}

    static auto classof(const Type* type) -> bool {
        return type->kind() == Kind::Pointer;
    }
};

class ReferenceType : public TypeWithOneElement {
public:
    ReferenceType(Type* element_type, Location location)
        : TypeWithOneElement(Kind::Reference, location, element_type) {}

    [[nodiscard]]
    static auto classof(const Type* type) -> bool {
        return type->kind() == Kind::Reference;
    }
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

        Param(std::string name_, Type* type_, Location location_)
            : name(std::move(name_)), type(type_), location(location_) {}
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

    [[nodiscard]]
    auto attributes() const -> const Attributes& {
        return _attributes;
    }

    /// Query whether this function has an attribute.
    [[nodiscard]]
    auto has_attr(FuncAttr attr) const -> bool {
        return _attributes.contains(attr);
    }

    /// Get the parameters of this function.
    [[nodiscard]]
    auto params() -> std::vector<Param>& { return _params; }
    [[nodiscard]]
    auto params() const -> const std::vector<Param>& { return _params; }

    /// Remove an attribute from this function.
    void remove_attr(FuncAttr attr) { _attributes.erase(attr); }

    /// Get the return type of this function.
    [[nodiscard]]
    auto return_type() -> Type*& { return _return_type; }
    [[nodiscard]]
    auto return_type() const { return _return_type; }

    /// Set an attribute on this function.
    void set_attr(FuncAttr attr) { _attributes[attr] = true; }

    [[nodiscard]]
    bool has_auto();

    [[nodiscard]]
    static auto classof(const Type* type) -> bool {
        return type->kind() == Kind::Function;
    }
};

class TypeDecl;

class DeclaredType : public Type {
    TypeDecl* _decl{};

    /// The scope for the contents of this type.
    Scope* _scope{};

public:
    DeclaredType(Kind kind, Scope* scope, Location location)
        : Type(kind, location), _scope(scope) {}

    /// Associate this type with a declaration.
    void associate_with_decl(TypeDecl* decl) {
        LCC_ASSERT(
            not _decl,
            "Cannot associate a single declared type with two type declarations (instead, make two declared types)"
        );
        _decl = decl;
    }

    /// Get the declaration of this type.
    ///
    /// If this is an anonymous type, this returns null.
    [[nodiscard]]
    auto decl() const -> TypeDecl* { return _decl; }

    /// Get the scope for the contents of this type.
    [[nodiscard]]
    auto scope() -> Scope* { return _scope; }

    [[nodiscard]]
    static auto classof(const Type* type) -> bool {
        return type->kind() == Kind::Enum
            or type->kind() == Kind::Union
            or type->kind() == Kind::Sum
            or type->kind() == Kind::Struct
            or type->kind() == Kind::TemplatedStruct;
    }
};

class StructType : public DeclaredType {
public:
    struct Member {
        static constexpr isz BadIndex = -1;

        Type* type;
        std::string name;
        Location location;
        usz byte_offset{};
        bool supplanted{false};

        Member(std::string name_, Type* type_, Location location_)
            : type(type_), name(std::move(name_)), location(location_) {}

        // For when you already know everything about a member (i.e. when
        // deserialising a struct from module metadata).
        Member(std::string name_, Type* type_, Location location_, usz byte_offset_, bool supplanted_)
            : type(type_),
              name(std::move(name_)),
              location(location_),
              byte_offset(byte_offset_),
              supplanted(supplanted_) {}
    };

private:
    std::vector<Member> _members;
    usz _byte_size{};
    usz _alignment{};

public:
    StructType(Scope* scope, std::vector<Member> members, Location location)
        : DeclaredType(Kind::Struct, scope, location), _members(std::move(members)) {}

    // Alignment of this struct type in bytes
    [[nodiscard]]
    auto alignment() const -> usz { return _alignment; }
    void alignment(usz alignment) { _alignment = alignment; }

    // Size of this struct type in bytes
    [[nodiscard]]
    auto byte_size() const -> usz { return _byte_size; }
    void byte_size(usz byteSize) { _byte_size = byteSize; }

    [[nodiscard]]
    auto members() -> std::vector<Member>& { return _members; }
    [[nodiscard]]
    auto members() const -> const std::vector<Member>& { return _members; }

    /// Caller should check return value is not nullptr.
    [[nodiscard]]
    auto member_by_name(std::string_view name) -> Member* {
        auto found = rgs::find_if(_members, [name](const Member& m) {
            return m.name == name;
        });
        if (found == _members.end()) return nullptr;
        return &*found;
    }
    /// Caller should check return value is not nullptr.
    [[nodiscard]]
    auto member_by_name(std::string_view name) const -> const Member* {
        return const_cast<StructType*>(this)->member_by_name(name);
    }

    /// Caller should check return value is not negative.
    [[nodiscard]]
    auto member_index_by_name(std::string_view name) -> isz {
        auto found = rgs::find_if(_members, [name](const Member& m) {
            return m.name == name;
        });
        if (found == _members.end()) return -1;
        return std::abs(std::distance(_members.begin(), found));
    }
    /// Caller should check return value is not negative.
    [[nodiscard]]
    auto member_index_by_name(std::string_view name) const {
        return const_cast<StructType*>(this)->member_index_by_name(name);
    }

    [[nodiscard]]
    static auto classof(const Type* type) -> bool { return type->kind() == Kind::Struct; }
};

class ArrayType : public TypeWithOneElement {
    Expr* _size;

public:
    ArrayType(Type* element_type, Expr* size, Location location = {})
        : TypeWithOneElement(Kind::Array, location, element_type), _size(size) {}

    /// Get the dimension of this array.
    [[nodiscard]]
    auto dimension() const -> usz;

    [[nodiscard]]
    auto size() -> Expr*& { return _size; }
    [[nodiscard]]
    auto size() const -> Expr* { return _size; }

    [[nodiscard]]
    static auto classof(const Type* type) -> bool { return type->kind() == Kind::Array; }
};

class ArrayViewType : public TypeWithOneElement {
    static constexpr auto K = Kind::ArrayView;

    StructType* _cached_struct{};

public:
    static constexpr usz IntegerWidth = 64;

    explicit ArrayViewType(Type* element_type, Location location = {})
        : TypeWithOneElement(K, location, element_type) {}

    [[nodiscard]]
    auto struct_type(Module& mod) -> StructType*& {
        if (not _cached_struct) {
            _cached_struct = new (mod) StructType(
                mod.global_scope(),
                {
                    {"data", new (mod) PointerType(element_type()), {}},
                    {"size", new (mod) IntegerType(IntegerWidth, false, {}), {}} //
                },
                location()
            );
            for (auto m : _cached_struct->members())
                m.type->set_sema_done();
            _cached_struct->set_sema_done();
        }

        return _cached_struct;
    }

    /// Caller needs to make sure to check the return value is not nullptr.
    [[nodiscard]]
    auto struct_type() -> StructType*& { return _cached_struct; }

    [[nodiscard]]
    static auto classof(const Type* type) -> bool { return type->kind() == K; }
};

class DynamicArrayType : public TypeWithOneElement {
    static constexpr auto K = Kind::DynamicArray;

    Expr* _initial_size;
    StructType* _cached_struct{nullptr};

public:
    static constexpr usz IntegerWidth = 32;

    DynamicArrayType(Type* element_type, Expr* size, Location location = {})
        : TypeWithOneElement(K, location, element_type), _initial_size(size) {}

    [[nodiscard]]
    auto initial_size() -> Expr*& { return _initial_size; }
    [[nodiscard]]
    auto initial_size() const -> Expr* { return _initial_size; }

    [[nodiscard]]
    auto struct_type(Module& mod) -> StructType*& {
        if (not _cached_struct) {
            _cached_struct = new (mod) StructType(
                mod.global_scope(),
                {{"data", new (mod) PointerType(element_type()), {}},
                 {"size", new (mod) IntegerType(IntegerWidth, false, {}), {}},
                 {"capacity", new (mod) IntegerType(IntegerWidth, false, {}), {}}},
                location()
            );
            for (auto m : _cached_struct->members())
                m.type->set_sema_done();
            _cached_struct->set_sema_done();
        }

        return _cached_struct;
    }

    /// Caller needs to make sure to check the return value is not nullptr.
    [[nodiscard]]
    auto struct_type() -> StructType*& { return _cached_struct; }

    [[nodiscard]]
    static auto classof(const Type* type) -> bool { return type->kind() == K; }
};

// Automagical tagged union, basically.
class SumType : public DeclaredType {
public:
    static constexpr usz IntegerWidth = 64;

    struct Member {
        Type* type;
        Expr* expr;
        std::string name;
        Location location;

        Member(std::string name_, Type* type_, Expr* expr_, Location location_)
            : type(type_), expr(expr_), name(std::move(name_)), location(location_) {}
    };

private:
    std::vector<Member> _members;
    usz _byte_size{};
    usz _alignment{};
    StructType* _cached_struct{};

public:
    SumType(Scope* scope, std::vector<Member> members, Location location)
        : DeclaredType(Kind::Sum, scope, location), _members(std::move(members)) {}

    [[nodiscard]]
    auto alignment() const { return _alignment; }
    void alignment(usz alignment) { _alignment = alignment; }

    [[nodiscard]]
    auto byte_size() const { return _byte_size; }
    void byte_size(usz byteSize) { _byte_size = byteSize; }

    [[nodiscard]]
    auto struct_type(Module& mod) -> StructType*&;

    /// Caller needs to make sure to check the return value is not nullptr.
    [[nodiscard]]
    auto struct_type() -> StructType*& {
        return _cached_struct;
    }

    [[nodiscard]]
    auto members() -> std::vector<Member>& { return _members; }
    [[nodiscard]]
    auto members() const -> const std::vector<Member>& { return _members; }

    /// Caller should check return value is not nullptr.
    [[nodiscard]]
    auto member_by_name(std::string_view name) -> Member* {
        auto found = rgs::find_if(_members, [name](const Member& m) {
            return m.name == name;
        });
        if (found == _members.end()) return nullptr;
        return &*found;
    }
    /// Caller should check return value is not nullptr.
    [[nodiscard]]
    auto member_by_name(std::string_view name) const -> const Member* {
        return const_cast<SumType*>(this)->member_by_name(name);
    }

    [[nodiscard]]
    static auto classof(const Type* type) -> bool { return type->kind() == Kind::Sum; }
};

class UnionType : public DeclaredType {
public:
    struct Member {
        Type* type;
        std::string name;
        Location location;

        Member(std::string name_, Type* type_, Location location_)
            : type(type_), name(std::move(name_)), location(location_) {}
    };

private:
    std::vector<Member> _members;
    // BYTES units, _NOT BITS_
    usz _byte_size{};
    // BYTES units, _NOT BITS_
    usz _alignment{};
    ArrayType* _cached_type{nullptr};

public:
    UnionType(Scope* scope, std::vector<Member> members, Location location)
        : DeclaredType(Kind::Union, scope, location), _members(std::move(members)) {}

    /// BYTES, _NOT BITS_
    [[nodiscard]]
    auto alignment() const { return _alignment; }
    /// BYTES, _NOT BITS_
    void alignment(usz alignment) { _alignment = alignment; }

    [[nodiscard]]
    /// BYTES, _NOT BITS_
    auto byte_size() const { return _byte_size; }
    /// BYTES, _NOT BITS_
    void byte_size(usz byteSize) { _byte_size = byteSize; }

    [[nodiscard]]
    auto array_type(Module& mod) -> ArrayType*&;

    /// Caller needs to make sure to check the return value is not nullptr.
    [[nodiscard]]
    auto array_type() -> ArrayType*& {
        return _cached_type;
    }

    [[nodiscard]]
    auto members() -> std::vector<Member>& { return _members; }
    [[nodiscard]]
    auto members() const -> const std::vector<Member>& { return _members; }

    [[nodiscard]]
    static auto classof(const Type* type) -> bool { return type->kind() == Kind::Union; }
};

class EnumeratorDecl;
class EnumType : public DeclaredType {
    std::vector<EnumeratorDecl*> _enumerators;
    Type* _underlying_type{};

public:
    EnumType(Scope* scope, Type* underlying_type, std::vector<EnumeratorDecl*> enumerators, Location location)
        : DeclaredType(Kind::Enum, scope, location),
          _enumerators(std::move(enumerators)),
          _underlying_type(underlying_type) {}

    [[nodiscard]]
    auto enumerators() -> std::vector<EnumeratorDecl*>& { return _enumerators; }
    [[nodiscard]]
    auto enumerators() const -> const std::vector<EnumeratorDecl*>& { return _enumerators; }

    [[nodiscard]]
    auto underlying_type() -> Type*& { return _underlying_type; }
    [[nodiscard]]
    auto underlying_type() const -> Type* { return _underlying_type; }

    [[nodiscard]]
    auto enumerator_by_value(aint value) -> EnumeratorDecl*;

    [[nodiscard]]
    auto enumerator_by_name(std::string_view name) -> EnumeratorDecl*;

    [[nodiscard]]
    static auto classof(const Type* type) -> bool { return type->kind() == Kind::Enum; }
};

/// \brief Base class for expression syntax nodes.
class Expr : public SemaNode {
protected:
    static auto CloneImpl(
        Module& mod,
        Context* context,
        Expr* expr,
        std::unordered_map<Scope*, Scope*>& scope_fixups
    ) -> Expr*;

public:
    /// Do NOT reorder these!
    /// Search terms: ExprKind, Expr::Kind, ExpressionKind
    enum struct Kind {
        While,
        For,
        Return,

        // BEGIN TypedExpr
        TypeDecl,
        TypeAliasDecl,
        EnumeratorDecl,
        TemplatedFuncDecl,
        VarDecl,
        FuncDecl,

        IntegerLiteral,
        FractionalLiteral,
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
        Type,
        Group,
        MemberAccess,
        // END TypedExpr

        Module,

        // replaced during sema
        Match,
        Switch,
        Sizeof,
        Alignof,
        Template,
        Apply,
    };

private:
    Kind _kind;
    bool _lvalue = false;

protected:
    Expr(Kind kind, Location location)
        : SemaNode(location), _kind(kind) {}

public:
    virtual ~Expr() = default;

    [[nodiscard]] auto operator new(size_t) -> void* = delete;
    [[nodiscard]] auto operator new(size_t sz, Module& mod) -> void* {
        auto* ptr = ::operator new(sz);
        mod.nodes.push_back(static_cast<Expr*>(ptr));
        return ptr;
    }

    /// Try to evaluate this expression.
    ///
    /// \param ctx The context to use.
    /// \param out Outparameter for the result of the evaluation.
    /// \param required Whether to error if evaluation fails.
    /// \return Whether evaluation succeeded.
    [[nodiscard]]
    auto evaluate(Context* ctx, EvalResult& out, bool required) -> bool;

    // A somewhat human readable name that represents this expression.
    [[nodiscard]]
    auto name() const -> std::string;

    [[nodiscard]]
    auto children() const -> std::vector<lcc::glint::Expr*>;

    [[nodiscard]]
    auto children_ref() -> std::vector<lcc::glint::Expr**>;

    [[nodiscard]]
    auto langtest_name() const -> std::string;
    [[nodiscard]]
    auto langtest_children() const -> std::vector<lcc::glint::Expr*>;

    [[nodiscard]]
    auto kind() const -> Kind { return _kind; }

    /// Check if this is an lvalue. Only lvalues can have their
    /// address taken or be converted to references.
    [[nodiscard]]
    auto is_lvalue() const -> bool { return _lvalue; }

    /// Mark this as an lvalue.
    void set_lvalue(bool lvalue = true) { _lvalue = lvalue; }

    [[nodiscard]]
    auto type() const -> Type*;

    // Print this expression.
    auto string(bool use_colour) const -> std::string;
    void print(bool use_colour) const;

    // Deep-copy an expression.
    struct CloneResult {
        Expr* cloned_expr{};
        Scope* cloned_scope{};
    };

    [[nodiscard]]
    static auto Clone(
        Module& mod,
        Context* context,
        Expr* expr
    ) -> Expr*;

    /// Deep copy a vector of expressions.
    [[nodiscard]]
    static auto Clone(
        Module& mod,
        Context* context,
        std::vector<Expr*> exprs
    ) -> std::vector<Expr*> {
        std::vector<Expr*> out{};
        out.reserve(exprs.size());
        for (auto e : exprs)
            out.emplace_back(Clone(mod, context, e));

        return out;
    }
};

[[nodiscard]]
auto ToString(Expr::Kind k) -> std::string;

class TypedExpr : public Expr {
    Type* _type;

protected:
    TypedExpr(Kind kind, Location location, Type* type = Type::Unknown)
        : Expr(kind, location), _type(type) {}

public:
    [[nodiscard]]
    auto type() const -> Type* { return _type; }
    void type(Type* type) { _type = type; }

    /// Get a reference to the type of this expression.
    [[nodiscard]]
    auto type_ref() -> Type** { return &_type; }

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool {
        return expr->kind() >= Kind::TypeDecl and expr->kind() <= Kind::MemberAccess;
    }
};

// A group of expressions that is to be treated as if it is one
// expression; the "result" of an expression group is the result of it's
// last expression (if the expression group isn't being used some other
// way by semantic analysis, like for 'apply').
class GroupExpr : public TypedExpr {
    std::vector<Expr*> _expressions;

public:
    GroupExpr(std::vector<Expr*> exprs, Location location)
        : TypedExpr(Kind::Group, location),
          _expressions(exprs) {}

    [[nodiscard]]
    auto expressions() const { return _expressions; }
    [[nodiscard]]
    auto expressions() -> decltype(_expressions)& {
        return _expressions;
    }

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool {
        return expr->kind() == Kind::Group;
    }
};

/// Base class for declarations. Declarations have a name.
class Decl : public TypedExpr {
    std::string _name;
    Scope* _scope;

protected:
    Decl(Kind kind, std::string name, Type* type, Location location)
        : TypedExpr(kind, location, type), _name(std::move(name)) {}

public:
    [[nodiscard]]
    auto name() const -> const std::string& { return _name; }
    [[nodiscard]]
    auto name(std::string name) { _name = std::move(name); }

    [[nodiscard]]
    auto scope() const -> Scope* { return _scope; }
    void scope(Scope* new_scope) { _scope = new_scope; }

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool {
        return expr->kind() >= Kind::TypeDecl and expr->kind() <= Kind::FuncDecl;
    }
};

class IntegerLiteral : public TypedExpr {
    aint _value;

public:
    IntegerLiteral(aint value, Location location)
        : TypedExpr(Kind::IntegerLiteral, location, Type::Int), _value(value) {
        /// For now, there should be no way that the value could be out of range.
        set_sema_done();
    }

    IntegerLiteral(aint value, Type* ty, Location location)
        : TypedExpr(Kind::IntegerLiteral, location, ty), _value(value) {
        /// For now, there should be no way that the value could be out of range.
        set_sema_done();
    }

    [[nodiscard]]
    auto value() const -> aint { return _value; }

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool {
        return expr->kind() == Kind::IntegerLiteral;
    }
};

class FractionalLiteral : public TypedExpr {
    FixedPointNumber _value;

public:
    FractionalLiteral(FixedPointNumber value, Location location)
        : TypedExpr(Kind::FractionalLiteral, location, Type::Float), _value(value) {
        /// For now, there should be no way that the value could be out of range.
        set_sema_done();
    }

    FractionalLiteral(FixedPointNumber value, Type* ty, Location location)
        : TypedExpr(Kind::FractionalLiteral, location, ty), _value(value) {
        /// For now, there should be no way that the value could be out of range.
        set_sema_done();
    }

    [[nodiscard]]
    auto value() const -> FixedPointNumber { return _value; }

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool {
        return expr->kind() == Kind::FractionalLiteral;
    }
};

class StringLiteral : public TypedExpr {
    usz _index;
    Module* _module;

public:
    /// Intern the given string and create a string literal for it.
    StringLiteral(Module& mod, std::string_view value, Location location);

    [[nodiscard]]
    auto string_index() const -> usz { return _index; }

    [[nodiscard]]
    auto origin_module() const -> Module* { return _module; }

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool {
        return expr->kind() == Kind::StringLiteral;
    }
};

class CompoundLiteral : public TypedExpr {
public:
    struct Member {
        std::string name{};
        Expr* value{};
    };

private:
    std::vector<Member> _values;

public:
    CompoundLiteral(std::vector<Member> values, Location location, Type* type = Type::Unknown)
        : TypedExpr(Kind::CompoundLiteral, location, type), _values(std::move(values)) {}

    CompoundLiteral(const std::vector<Expr*>& values, Location location, Type* type = Type::Unknown)
        : TypedExpr(Kind::CompoundLiteral, location, type) {
        _values.reserve(values.size());
        for (auto* v : values)
            _values.emplace_back("", v);
    }

    [[nodiscard]]
    auto values() -> std::vector<Member>& { return _values; }
    [[nodiscard]]
    auto values() const -> const std::vector<Member>& { return _values; }

    [[nodiscard]]
    auto children() const -> std::vector<Expr*> {
        std::vector<Expr*> out{};
        out.reserve(_values.size());
        for (const auto& m : _values)
            out.emplace_back(m.value);
        return out;
    }

    [[nodiscard]]
    auto children_ref() -> std::vector<Expr**> {
        std::vector<Expr**> out{};
        out.reserve(_values.size());
        for (auto& m : _values)
            out.emplace_back(&m.value);
        return out;
    }

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool {
        return expr->kind() == Kind::CompoundLiteral;
    }
};

/// Enumerator declaration.
///
/// This is a decl so NameRefExpr can refer to it.
class EnumeratorDecl : public Decl {
    Expr* _init;

public:
    EnumeratorDecl(std::string name, Expr* init, Location location)
        : Decl(Kind::EnumeratorDecl, std::move(name), Type::Int, location),
          _init(init) {}

    [[nodiscard]]
    auto init() -> Expr*& { return _init; }
    [[nodiscard]]
    auto init() const { return _init; }

    [[nodiscard]]
    auto value() const -> aint;

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool { return expr->kind() == Kind::EnumeratorDecl; }
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
    [[nodiscard]]
    auto mangled_name() const -> std::string;

    /// Get the module this declaration is in.
    [[nodiscard]]
    auto module() const { return _mod; }

    /// Get the linkage of this declaration.
    [[nodiscard]]
    auto linkage() const { return _linkage; }

    /// Set the linkage of this declaration.
    void linkage(Linkage linkage) { _linkage = linkage; }

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool {
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

    [[nodiscard]]
    auto init() -> Expr*& { return _init; }
    [[nodiscard]]
    auto init() const { return _init; }

    static auto classof(const Expr* expr) -> bool {
        return expr->kind() == Kind::VarDecl;
    }
};

class FuncDecl : public ObjectDecl {
    Expr* _body{};
    Scope* _scope{};

    /// Only present if this is not an imported function. Sema
    /// fills these in. Furthermore, parameters with empty names
    /// do not get a decl.
    std::vector<VarDecl*> _params;

    /// Calling convention.
    CallConv _cc;

    // NOTE: For warnings in sema
    std::vector<Decl*> _dangling_dynarrays{};

protected:
    FuncDecl(
        Kind kind,
        std::string name,
        FuncType* type,
        Expr* body,
        Scope* scope,
        Module* mod,
        Linkage linkage,
        Location location,
        CallConv cc = CallConv::Glint
    ) : ObjectDecl(kind, type, std::move(name), mod, linkage, location),
        _body(body), _scope(scope), _cc(cc) {
        mod->add_function(this);

        // Functions receive special handling in sema and their types are
        // always known when we actually start analysing code.
        set_sema_done();
    }

public:
    // TODO: Document what scope parameter is meant to contain.
    FuncDecl(
        std::string name,
        FuncType* type,
        Expr* body,
        Scope* scope,
        Module* mod,
        Linkage linkage,
        Location location,
        CallConv cc = CallConv::Glint
    ) : FuncDecl(Kind::FuncDecl, name, type, body, scope, mod, linkage, location, cc) {}

    [[nodiscard]]
    auto body() -> Expr*& { return _body; }

    [[nodiscard]]
    auto body() const -> Expr* { return _body; }

    [[nodiscard]]
    auto call_conv() const -> CallConv { return _cc; }

    [[nodiscard]]
    auto function_type() const -> FuncType* {
        // TODO: I don't think a FuncDecl can have a function pointer type, but,
        // if it can, handle that here.
        LCC_ASSERT(
            is<FuncType>(type()),
            "Glint FuncDecl underlying type isn't a FuncType (it's {}). If this is a function pointer, let me know and I'll fix it---TIA.",
            type()->string()
        );
        return as<FuncType>(type());
    }

    [[nodiscard]]
    auto return_type() const -> Type* {
        return function_type()->return_type();
    }

    [[nodiscard]]
    auto param_types() const {
        return as<FuncType>(type())->params() | vws::transform([](auto& p) { return p.type; });
    }

    [[nodiscard]]
    auto param_decls() -> std::vector<VarDecl*>& { return _params; }

    [[nodiscard]]
    auto param_decls() const -> const std::vector<VarDecl*>& { return _params; }

    auto scope(Scope* scope) { _scope = scope; }

    [[nodiscard]]
    auto scope() const -> Scope* { return _scope; }

    [[nodiscard]]
    auto dangling_dynarrays() -> std::vector<Decl*>& { return _dangling_dynarrays; }

    [[nodiscard]]
    auto dangling_dynarrays() const -> std::vector<Decl*> { return _dangling_dynarrays; }

    static auto classof(const Expr* expr) -> bool {
        // Because TemplateFuncDecl inherits from FuncDecl, it is safe to cast
        // between the two.
        return expr->kind() == Kind::FuncDecl
            or expr->kind() == Kind::TemplatedFuncDecl;
    }
};

class TemplatedFuncDecl : public FuncDecl {
    // Mapping of signature to declaration.
    std::unordered_map<FuncType*, FuncDecl*> _instantiation_cache{};

public:
    TemplatedFuncDecl(
        std::string name,
        FuncType* type,
        Expr* body,
        Scope* scope,
        Module* mod,
        Linkage linkage,
        Location location,
        CallConv cc = CallConv::Glint
    ) : FuncDecl(Kind::TemplatedFuncDecl, name, type, body, scope, mod, linkage, location, cc) {}

    [[nodiscard]]
    auto find_instantiation(FuncType* signature) -> FuncDecl*;

    [[nodiscard]]
    auto make_instantiation(
        Module& mod,
        Context& context,
        FuncType* signature
    ) -> FuncDecl*;

    static bool is_auto(const Type& t);

    static void replace_auto(
        Type*& maybe_auto,
        Type* replacement
    );
    static auto deduce(
        Module& mod,
        FuncType* signature,
        std::vector<Expr*> arguments
    ) -> FuncType*;

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool {
        // A TemplatedFuncDecl may be a FuncDecl, but only a TemplatedFuncDecl may
        // be a TemplatedFuncDecl.
        return expr->kind() == Kind::TemplatedFuncDecl;
    }
};

class TypeDecl : public Decl {
    /// The module this struct is declared in.
    Module* _module{};

public:
    TypeDecl(Module* mod, std::string name, DeclaredType* declared_type, Location location)
        : Decl(Kind::TypeDecl, std::move(name), declared_type, location),
          _module(mod) {
        declared_type->associate_with_decl(this);
    }

    /// Get the module this struct is declared in.
    [[nodiscard]]
    auto module() const -> Module* { return _module; }

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool {
        return expr->kind() == Kind::TypeDecl;
    }
};

class TypeAliasDecl : public Decl {
public:
    TypeAliasDecl(std::string name, Type* aliased_type, Location location)
        : Decl(
              Kind::TypeAliasDecl,
              std::move(name),
              aliased_type,
              location
          ) {}

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool {
        return expr->kind() == Kind::TypeAliasDecl;
    }
};

/// A set of function overloads.
class OverloadSet : public TypedExpr {
    std::vector<FuncDecl*> _overloads;

public:
    OverloadSet(std::vector<FuncDecl*> overloads, Location location)
        : TypedExpr(Kind::OverloadSet, location), _overloads(std::move(overloads)) {}

    [[nodiscard]]
    auto overloads() const -> const std::vector<FuncDecl*>& { return _overloads; }

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool {
        return expr->kind() == Kind::OverloadSet;
    }
};

class ApplyExpr : public Expr {
    Expr* _function{nullptr};
    std::vector<Expr*> _argument_lists{};

public:
    ApplyExpr(Expr* function, std::vector<Expr*> argument_lists, Location location)
        : Expr(Kind::Apply, location), _function(function), _argument_lists(argument_lists) {}

    Expr*& function() { return _function; }
    Expr* function() const { return _function; }

    std::vector<Expr*>& argument_lists() { return _argument_lists; }
    std::vector<Expr*> argument_lists() const { return _argument_lists; }

    static auto classof(const Expr* expr) -> bool {
        return expr->kind() == Kind::Apply;
    }
};

class IfExpr : public TypedExpr {
    Expr* _condition;
    Expr* _then;
    Expr* _otherwise{};

public:
    IfExpr(Expr* condition, Expr* then, Expr* otherwise, Location location)
        : TypedExpr(Kind::If, location),
          _condition(condition),
          _then(then), _otherwise(otherwise) {}

    [[nodiscard]]
    auto condition() -> Expr*& { return _condition; }
    [[nodiscard]]
    auto condition() const { return _condition; }

    [[nodiscard]]
    auto then() -> Expr*& { return _then; }
    [[nodiscard]]
    auto then() const { return _then; }

    [[nodiscard]]
    auto otherwise() -> Expr*& { return _otherwise; }
    [[nodiscard]]
    auto otherwise() const { return _otherwise; }

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool {
        return expr->kind() == Kind::If;
    }
};

// "match" <value-of-sum-type> "{"
//     { "."<name-of-sum-type-member> [ ":" ] <body-expression> ";" }
// "}"
//
// foo : sum {
//   x : int;
//   y : [Bool];
// };
// bar : foo;
// bar.x := 69;
//
// out : int;
//
// ;; THE FOLLOWING
// match bar {
//   .x: out := bar.x;
//   .y: out := 42;
// };
// ;; BECOMES
// if (has bar.x) out := bar.x;
// else if (has bar.y) out := 42;
// else exit(7);
class MatchExpr : public Expr {
    Expr* _object;
    std::vector<std::string> _names;
    std::vector<Expr*> _bodies;

public:
    MatchExpr(Expr* object, Location location) : Expr(Kind::Match, location), _object(object) {}

    [[nodiscard]]
    auto object() const { return _object; }
    [[nodiscard]]
    auto object() -> Expr*& { return _object; }

    [[nodiscard]]
    auto names() const { return _names; }
    [[nodiscard]]
    auto names() -> std::vector<std::string>& { return _names; }

    [[nodiscard]]
    auto bodies() const { return _bodies; }
    // Please, please, PLEASE don't use this to add match bodies.
    [[nodiscard]]
    auto bodies() -> std::vector<Expr*>& { return _bodies; }

    auto add_match(std::string name, Expr* body) {
        _names.push_back(std::move(name));
        _bodies.push_back(body);
    }

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool {
        return expr->kind() == Kind::Match;
    }
};

class SwitchExpr : public Expr {
    Expr* _object;
    std::vector<std::string> _names;
    std::vector<Expr*> _bodies;

public:
    SwitchExpr(Expr* object, Location location)
        : Expr(Kind::Switch, location),
          _object(object) {}

    [[nodiscard]]
    auto object() const { return _object; }
    [[nodiscard]]
    auto object() -> Expr*& { return _object; }

    [[nodiscard]]
    auto names() const { return _names; }
    [[nodiscard]]
    auto names() -> std::vector<std::string>& { return _names; }

    [[nodiscard]]
    auto bodies() const { return _bodies; }
    // Please, please, PLEASE don't use this to add match bodies.
    [[nodiscard]]
    auto bodies() -> std::vector<Expr*>& { return _bodies; }

    auto add_match(std::string name, Expr* body) {
        _names.push_back(std::move(name));
        _bodies.push_back(body);
    }

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool {
        return expr->kind() == Kind::Switch;
    }
};

class Loop : public Expr {
    Expr* _body;
    Expr* _condition;

public:
    Loop(Kind kind, Expr* condition, Expr* body, Location location)
        : Expr(kind, location), _body(body), _condition(condition) {}

    [[nodiscard]]
    auto body() const { return _body; }
    [[nodiscard]]
    auto body() -> Expr*& { return _body; }

    [[nodiscard]]
    auto condition() const { return _condition; }
    [[nodiscard]]
    auto condition() -> Expr*& { return _condition; }

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool {
        return expr->kind() >= Kind::While and expr->kind() <= Kind::For;
    }
};

class WhileExpr : public Loop {
public:
    WhileExpr(Expr* condition, Expr* body, Location location)
        : Loop(Kind::While, condition, body, location) {}

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool { return expr->kind() == Kind::While; }
};

class ForExpr : public Loop {
    Expr* _init{};
    Expr* _increment{};

public:
    ForExpr(Expr* init, Expr* condition, Expr* increment, Expr* body, Location location)
        : Loop(Kind::For, condition, body, location), _init(init), _increment(increment) {}

    [[nodiscard]]
    auto init() -> Expr*& { return _init; }
    [[nodiscard]]
    auto init() const { return _init; }

    [[nodiscard]]
    auto increment() -> Expr*& { return _increment; }
    [[nodiscard]]
    auto increment() const { return _increment; }

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool { return expr->kind() == Kind::For; }
};

class BlockExpr : public TypedExpr {
    std::vector<Expr*> _children;

public:
    BlockExpr(std::vector<Expr*> children, Location location)
        : TypedExpr(Kind::Block, location), _children(std::move(children)) {}

    /// Add an expression to this block.
    void add(Expr* expr) { _children.push_back(expr); }

    [[nodiscard]]
    auto children() -> std::vector<Expr*>& { return _children; }
    [[nodiscard]]
    auto children() const -> const std::vector<Expr*>& { return _children; }

    [[nodiscard]]
    auto last_expr() -> Expr** {
        if (_children.empty()) return nullptr;

        // Going in reverse, starting at the end, skip any function declarations.
        // TODO: If every expression is a function declaration, return nullptr.
        auto* last = &_children.back();
        auto last_index = last - _children.data();
        while (is<FuncDecl>(*last) and last_index--)
            last = _children.data() + last_index;

        return last;
    }

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool { return expr->kind() == Kind::Block; }
};

class ReturnExpr : public Expr {
    Expr* _value{};

public:
    ReturnExpr(Expr* value, Location location)
        : Expr(Kind::Return, location), _value(value) {}

    [[nodiscard]]
    auto value() -> Expr*& { return _value; }
    [[nodiscard]]
    auto value() const { return _value; }

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool { return expr->kind() == Kind::Return; }
};

/// Expression that has been evaluated by sema, together w/ a cached value.
class ConstantExpr : public TypedExpr {
    EvalResult _value;
    Expr* _expression{};

public:
    ConstantExpr(Expr* expr, EvalResult value)
        : TypedExpr(Kind::EvaluatedConstant, expr->location(), expr->type()),
          _value(std::move(value)),
          _expression(expr) {
        LCC_ASSERT(expr->ok());
        set_sema_done();
    }

    ConstantExpr(Type* ty, EvalResult value, Location location)
        : TypedExpr(Kind::EvaluatedConstant, location, ty),
          _value(std::move(value)) {
        set_sema_done();
    }

    /// May return null. This is the case if a constant value of non-`int`
    /// type needs to be synthesised by the compiler.
    // NOTE: Technically const because it can't change the pointer in this
    // expression but it definitely can change the expression the pointer
    // points to.
    [[nodiscard]]
    auto expr() const { return _expression; }

    [[nodiscard]]
    auto value() -> EvalResult& { return _value; }

    [[nodiscard]]
    auto value() const -> const EvalResult& { return _value; }

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool { return expr->kind() == Kind::EvaluatedConstant; }
};

class CallExpr : public TypedExpr {
    Expr* _callee;
    std::vector<Expr*> _args;

public:
    CallExpr(Expr* callee, std::vector<Expr*> args, Location location)
        : TypedExpr(Kind::Call, location), _callee(callee), _args(std::move(args)) {}

    [[nodiscard]]
    auto args() -> std::vector<Expr*>& { return _args; }
    [[nodiscard]]
    auto args() const -> const std::vector<Expr*>& { return _args; }

    [[nodiscard]]
    auto callee() -> Expr*& { return _callee; }
    [[nodiscard]]
    auto callee() const { return _callee; }

    /// Get the function type of the callee.
    [[nodiscard]]
    auto callee_type() const -> FuncType*;

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool { return expr->kind() == Kind::Call; }
};

class IntrinsicCallExpr : public TypedExpr {
    IntrinsicKind _kind;
    std::vector<Expr*> _args;

public:
    IntrinsicCallExpr(IntrinsicKind kind, std::vector<Expr*> args)
        : TypedExpr(Kind::IntrinsicCall, {}), _kind(kind), _args(std::move(args)) {}

    [[nodiscard]]
    auto args() -> std::vector<Expr*>& { return _args; }
    [[nodiscard]]
    auto args() const -> const std::vector<Expr*>& { return _args; }

    [[nodiscard]]
    auto intrinsic_kind() const { return _kind; }

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool { return expr->kind() == Kind::IntrinsicCall; }
};

class CastExpr : public TypedExpr {
    Expr* _value;
    CastKind _cast_kind;

public:
    CastExpr(Expr* value, Type* ty, CastKind k, Location location)
        : TypedExpr(Kind::Cast, location, ty), _value(value), _cast_kind(k) {}

    [[nodiscard]]
    auto cast_kind() const { return _cast_kind; }

    /// Check cast kinds.
    [[nodiscard]]
    auto is_hard_cast() const -> bool { return _cast_kind == CastKind::HardCast; }
    [[nodiscard]]
    auto is_implicit_cast() const -> bool { return _cast_kind == CastKind::ImplicitCast; }
    [[nodiscard]]
    auto is_lvalue_to_rvalue() const -> bool { return _cast_kind == CastKind::LValueToRValueConv; }
    [[nodiscard]]
    auto is_lvalue_to_ref() const -> bool { return _cast_kind == CastKind::LValueToReference; }
    [[nodiscard]]
    auto is_ref_to_lvalue() const -> bool { return _cast_kind == CastKind::ReferenceToLValue; }
    [[nodiscard]]
    auto is_soft_cast() const -> bool { return _cast_kind == CastKind::SoftCast; }

    /// Get the operand of this expression.
    [[nodiscard]]
    auto operand() -> Expr*& { return _value; }
    [[nodiscard]]
    auto operand() const { return _value; }

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool { return expr->kind() == Kind::Cast; }
};

class UnaryExpr : public TypedExpr {
    Expr* _operand;
    TokenKind _op;
    bool _postfix;

public:
    UnaryExpr(TokenKind op, Expr* operand, bool is_postfix, Location location)
        : TypedExpr(Kind::Unary, location), _operand(operand), _op(op), _postfix(is_postfix) {}

    /// Check if this is a postfix unary expression.
    [[nodiscard]]
    auto is_postfix() const -> bool { return _postfix; }

    /// Get the operand of this expression.
    [[nodiscard]]
    auto operand() -> Expr*& { return _operand; }
    [[nodiscard]]
    auto operand() const { return _operand; }

    /// Get the unary operator.
    [[nodiscard]]
    auto op() const { return _op; }

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool { return expr->kind() == Kind::Unary; }
};

class BinaryExpr : public TypedExpr {
    Expr* _lhs;
    Expr* _rhs;
    TokenKind _op;

public:
    BinaryExpr(TokenKind op, Expr* lhs, Expr* rhs, Location location)
        : TypedExpr(Kind::Binary, location), _lhs(lhs), _rhs(rhs), _op(op) {}

    /// Get the left-hand side of this expression.
    [[nodiscard]]
    auto lhs() -> Expr*& { return _lhs; }
    [[nodiscard]]
    auto lhs() const { return _lhs; }

    /// Get the right-hand side of this expression.
    [[nodiscard]]
    auto rhs() -> Expr*& { return _rhs; }
    [[nodiscard]]
    auto rhs() const { return _rhs; }

    /// Get the binary operator.
    [[nodiscard]]
    auto op() const { return _op; }

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool { return expr->kind() == Kind::Binary; }
};

class NameRefExpr : public TypedExpr {
    std::string _name;
    Scope* _scope;
    Expr* _target{};

public:
    NameRefExpr(std::string name, Scope* name_scope, Location location)
        : TypedExpr(Kind::NameRef, location), _name(std::move(name)), _scope(name_scope) {}

    [[nodiscard]]
    auto name() const -> const std::string& { return _name; }

    [[nodiscard]]
    auto scope() const -> Scope* { return _scope; }
    void scope(Scope* scope) { _scope = scope; }

    [[nodiscard]]
    auto target() const -> Expr* { return _target; }
    void target(Expr* target) { _target = target; }

    auto target_ref() -> Expr** { return &_target; }

    static auto classof(const Expr* expr) -> bool { return expr->kind() == Kind::NameRef; }
};

class TypeExpr : public TypedExpr {
    Type* _contained_type;

public:
    TypeExpr(Module& mod, Type* _ty, Location location)
        : TypedExpr(
              Kind::Type,
              location,
              new (mod) TypeType(location)
          ),
          _contained_type(_ty) {}

    auto contained_type() { return _contained_type; }
    auto contained_type_ref() { return &_contained_type; }
    auto contained_type() const { return _contained_type; }
    auto contained_type_ref() const { return &_contained_type; }

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool {
        return expr->kind() == Kind::Type;
    }
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

    [[nodiscard]]
    auto member() const -> usz { return _member_index; }

    [[nodiscard]]
    auto name() const -> const std::string& { return _name; }

    [[nodiscard]]
    auto object() -> Expr*& { return _object; }
    [[nodiscard]]
    auto object() const { return _object; }
    auto object(Expr* object) { _object = object; }

    [[nodiscard]]
    auto struct_type() -> StructType*& { return _struct; }

    static auto classof(const Expr* expr) -> bool {
        return expr->kind() == Kind::MemberAccess;
    }
};

class ModuleExpr : public Expr {
    Module* _mod;

public:
    ModuleExpr(Module* _module, Location location)
        : Expr(Kind::Module, location), _mod(_module) {}

    [[nodiscard]]
    auto mod() const -> Module* {
        return _mod;
    }

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool {
        return expr->kind() == Kind::Module;
    }
};

class SizeofExpr : public Expr {
    Expr* _expr;

public:
    SizeofExpr(Expr* _expression, Location location)
        : Expr(Kind::Sizeof, location), _expr(_expression) {}

    [[nodiscard]]
    auto expr() const -> Expr* {
        return _expr;
    }

    [[nodiscard]]
    auto expr_ref() -> Expr** {
        return &_expr;
    }

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool { return expr->kind() == Kind::Sizeof; }
};

class AlignofExpr : public Expr {
    Expr* _expr;

public:
    AlignofExpr(Expr* _expression, Location location)
        : Expr(Kind::Alignof, location), _expr(_expression) {}

    [[nodiscard]]
    auto expr() const -> Expr* {
        return _expr;
    }

    [[nodiscard]]
    auto expr_ref() -> Expr** {
        return &_expr;
    }

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool { return expr->kind() == Kind::Alignof; }
};

class TemplateExpr : public Expr {
public:
    struct Param {
        std::string name{};
        Type* type{nullptr};
        Location location{};
    };

private:
    Expr* _body;
    std::vector<Param> _params;

public:
    TemplateExpr(Expr* body, std::vector<Param> params, Location location)
        : Expr(Kind::Template, location),
          _body(body),
          _params(std::move(params)) {}

    [[nodiscard]]
    auto body() const -> Expr* { return _body; }

    [[nodiscard]]
    auto body_ref() -> Expr** { return &_body; }

    [[nodiscard]]
    auto params() const -> const std::vector<Param>& { return _params; }

    [[nodiscard]]
    auto params_ref() -> std::vector<Param>& { return _params; }

    [[nodiscard]]
    static auto classof(const Expr* expr) -> bool {
        return expr->kind() == Kind::Template;
    }
};

class TemplatedStructType : public DeclaredType {
    using Param = TemplateExpr::Param;
    using Member = StructType::Member;

    std::vector<Param> _params;
    std::vector<Member> _members;

public:
    TemplatedStructType(
        Scope* scope,
        std::vector<Param> params,
        std::vector<Member> members,
        Location location
    ) : DeclaredType(Kind::TemplatedStruct, scope, location),
        _params(std::move(params)),
        _members(std::move(members)) {}

    [[nodiscard]]
    auto params() -> std::vector<Param>& { return _params; }
    [[nodiscard]]
    auto params() const -> const std::vector<Param>& { return _params; }

    [[nodiscard]]
    auto members() -> std::vector<Member>& { return _members; }
    [[nodiscard]]
    auto members() const -> const std::vector<Member>& { return _members; }

    /// Caller should check return value is not nullptr.
    [[nodiscard]]
    auto param_by_name(std::string_view name) -> Param* {
        auto found = rgs::find_if(_params, [name](const Param& m) {
            return m.name == name;
        });
        if (found == _params.end()) return nullptr;
        return &*found;
    }
    /// Caller should check return value is not nullptr.
    [[nodiscard]]
    auto param_by_name(std::string_view name) const -> const Param* {
        return const_cast<TemplatedStructType*>(this)->param_by_name(name);
    }

    /// Caller should check return value is not negative.
    [[nodiscard]]
    auto param_index_by_name(std::string_view name) -> isz {
        auto found = rgs::find_if(_params, [name](const Param& m) {
            return m.name == name;
        });
        if (found == _params.end()) return -1;
        return std::abs(std::distance(_params.begin(), found));
    }
    /// Caller should check return value is not negative.
    [[nodiscard]]
    auto param_index_by_name(std::string_view name) const {
        return const_cast<TemplatedStructType*>(this)->param_index_by_name(name);
    }

    /// Caller should check return value is not nullptr.
    [[nodiscard]]
    auto member_by_name(std::string_view name) -> Member* {
        auto found = rgs::find_if(_members, [name](const Member& m) {
            return m.name == name;
        });
        if (found == _members.end()) return nullptr;
        return &*found;
    }
    /// Caller should check return value is not nullptr.
    [[nodiscard]]
    auto member_by_name(std::string_view name) const -> const Member* {
        // Call the non-const version of the function (and cast everything to const).
        return const_cast<TemplatedStructType*>(this)->member_by_name(name);
    }

    /// Caller should check return value is not negative.
    [[nodiscard]]
    auto member_index_by_name(std::string_view name) -> isz {
        auto found = rgs::find_if(_members, [name](const Member& m) {
            return m.name == name;
        });
        if (found == _members.end()) return -1;
        return std::abs(std::distance(_members.begin(), found));
    }
    /// Caller should check return value is not negative.
    [[nodiscard]]
    auto member_index_by_name(std::string_view name) const {
        // Call the non-const version of the function (and cast everything to const).
        return const_cast<TemplatedStructType*>(this)->member_index_by_name(name);
    }

    [[nodiscard]]
    static auto classof(const Type* type) -> bool { return type->kind() == Kind::TemplatedStruct; }
};

bool IsCallable(Expr* expr);

/// Given an expression, get a location of length one that refers to the
/// most-advanced character position that still lies within the given
/// expression's location.
/// Given identifier node "foo", return location referring to second "o".
///                          ^
/// Given declaration node "foo : int", return location referring to "t".
///                                 ^
auto GetRightmostLocation(lcc::glint::Expr* expr) -> lcc::Location;

/// Given an expression, get a location of length one that refers to the
/// position directly after the expression.
/// NOTE: This location may not be seekable, or it may point to EOF.
/// Given identifier node "foo", return location one past the second "o".
///                           ^
/// Given declaration node "foo : int", return location one past "t".
///                                  ^
auto GetPastLocation(lcc::glint::Expr* expr) -> lcc::Location;

/// Get a location of length one referring to the very end of the file;
/// just past the last character in the file.
auto GetLastLocation(const Context& context, u16 file_id) -> lcc::Location;

} // namespace lcc::glint

/// Formatter for types.
template <>
struct fmt::formatter<lcc::glint::Type> : formatter<string_view> {
    template <typename FormatContext>
    auto format(const lcc::glint::Type& t, FormatContext& ctx) const {
        // TODO: It'd be nice if this was controllable, somehow.
        static constexpr bool use_colour = true;
        return fmt::format_to(ctx.out(), "{}", t.string(use_colour));
    }
};

#endif // GLINT_AST_HH
