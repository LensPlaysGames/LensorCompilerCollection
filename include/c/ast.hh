#ifndef C_AST_HH
#define C_AST_HH

#include "c/opts.hh"
#include "lcc/utils/result.hh"
#include "lcc/utils/rtti.hh"

#include <lcc/core.hh>
#include <lcc/diags.hh>
#include <lcc/syntax/token.hh>
#include <lcc/utils.hh>

namespace lcc::c {
class BaseNode;
class Decl;

class TranslationUnit {
    CContext* _context;
    File* _file;

    std::vector<BaseNode*> all_nodes{};

    std::vector<Decl*> _top_level_decls{};

public:
    TranslationUnit(CContext* context, File* file)
        : _context(context), _file(file) {}

    auto c_context() const { return _context; }
    auto lcc_context() const { return _context->lcc_context(); }
    auto file() const { return _file; }

    auto add_top_level_decl(Decl* decl) { _top_level_decls.push_back(decl); }

    auto top_level_decls() -> const std::vector<Decl*>& { return _top_level_decls; }

    void print();

    friend BaseNode;
};

enum struct TokenKind {
    Invalid,

    EndOfFile,
    EndOfLine,

    // Grouping Delimiters
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,

    // Other Delimiters
    Dot,
    TripleDot,
    Comma,
    Colon,
    SemiColon,

    // Arithmetic Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,

    // Assignment Operators
    Equal,
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    PercentEqual,
    AmpersandEqual,
    PipeEqual,
    CaretEqual,
    LessLessEqual,
    GreaterGreaterEqual,

    // Bitwise Operators
    Tilde,
    Ampersand,
    Pipe,
    Caret,
    LessLess,
    GreaterGreater,

    // Boolean Operators
    Bang,
    AmpersandAmpersand,
    PipePipe,

    // Conditional Evaluation Operators
    Question,
    // Colon, // defined in delimiters

    // Equality Operators
    EqualEqual,
    BangEqual,

    // Relational Operators
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    // Increment/Decrement Operators
    PlusPlus,
    MinusMinus,

    // Member Selection Operators
    // Dot, // defined in delimiters
    MinusGreater,

    // User Tokens
    Ident,
    LitInt,
    LitFloat,
    LitChar,
    LitString,

    // C89 Keywords
    Auto,
    Break,
    Case,
    Char,
    Const,
    Continue,
    Default,
    Do,
    Double,
    Else,
    Enum,
    Extern,
    Float,
    For,
    Goto,
    If,
    Int,
    Long,
    Register,
    Return,
    Short,
    Signed,
    Sizeof,
    Static,
    Struct,
    Switch,
    Typedef,
    Union,
    Unsigned,
    Void,
    Volatile,
    While,

    // C99 Keywords
    Bool_,
    Complex_,
    Imaginary_,
    Inline,
    Restrict,

    // C11 Keywords
    Alignas_,
    Alignof_,
    Atomic_,
    Generic_,
    Noreturn_,
    StaticAssert_,
    ThreadLocal_,

    // C17 Keywords

    // C23 Keywords
    Alignas,
    Alignof,
    Bool,
    Constexpr,
    False,
    Nullptr,
    StaticAssert,
    ThreadLocal,
    True,
    Typeof,
    TypeofUnqual,
    Decimal128_,
    Decimal32_,
    Decimal64_,

    // GNU extension keywords
    GNU__alignof,
    GNU__alignof__,
    GNU__asm,
    GNU__asm__,
    GNU__attribute,
    GNU__attribute__,
    GNU__builtin_offsetof,
    GNU__builtin_va_arg,
    GNU__complex,
    GNU__complex__,
    GNU__const,
    GNU__extension__,
    GNU__func__,
    GNU__FUNCTION__,
    GNU__imag,
    GNU__imag__,
    GNU__inline,
    GNU__inline__,
    GNU__label__,
    GNU__null,
    GNU__PRETTY_FUNCTION__,
    GNU__real,
    GNU__real__,
    GNU__restrict,
    GNU__restrict__,
    GNU__signed,
    GNU__signed__,
    GNU__thread,
    GNU__typeof,
    GNU__volatile,
    GNU__volatile__,
};

enum struct LitIntegerKind {
    Int,
    UnsignedInt,
    Long,
    UnsignedLong,
    LongLong,
    UnsignedLongLong,
};

enum struct LitFloatKind {
    Double,
    Float,
    LongDouble,
};

struct CToken : public syntax::Token<TokenKind> {
    LitIntegerKind integer_kind;
    LitFloatKind float_kind;
    /// If this token is an identifier and its name is a macro argument name,
    /// then this is the index into the macro argument list of that argument.
    isz macro_arg_index = -1;
};

enum StorageClass {
    Auto,
    Register,
    Static,
    Extern,
    ThreadLocal,
    Dynamic,
};

enum struct OperatorKind {
    // Unary Operators
    PlusIdentity,
    Negate,
    AddressOf,
    Dereference,
    Compl,
    LogicalNot,

    // Binary Operators

    // Arithmetic Operators
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    // Assignment Operators
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    AndAssign,
    OrAssign,
    XorAssign,
    LshAssign,
    RshAssign,

    // Bitwise Operators
    And,
    Or,
    Xor,
    Lsh,
    Rsh,

    // Boolean Operators
    LogicalAnd,
    LogicalOr,

    // Equality Operators
    Equal,
    NotEqual,

    // Relational Operators
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    // Increment/Decrement Operators
    PlusPlus,
    MinusMinus,
};

enum struct IntegerKind {
    Char,
    SignedChar,
    UnsignedChar,
    Short,
    UnsignedShort,
    Int,
    UnsignedInt,
    Long,
    UnsignedLong,
    LongLong,
    UnsignedLongLong,
};

enum struct FloatKind {
    Float,
    Double,
    LongDouble,
};

std::string ToString(TokenKind kind);
std::string ToString(OperatorKind kind);

class BaseNode {
public:
    enum struct Kind {
        Statement,
        Expr,
        Type,
    };

private:
    const Kind _kind;

    Location _location;

protected:
    BaseNode(Kind kind, Location location)
        : _kind(kind), _location(location) {}

public:
    void* operator new(size_t) = delete;
    void* operator new(size_t sz, TranslationUnit& cu);

    bool is_statement() const { return _kind == Kind::Statement; }
    bool is_expr() const { return _kind == Kind::Expr; }

    auto location() const { return _location; }
};

class Statement : public BaseNode {
public:
    enum struct Kind {
        DeclStart_,

        Variable,
        Prototype,
        Function,
        Struct,
        Union,
        Enum,
        Typedef,

        DeclEnd_,

        If,
        Switch,
        // Case,
        For,
        While,
        DoWhile,
        Goto,
        Break,
        Continue,

        Compound,
        Expr,
        Empty,

        // Microsoft extensions
        TryExcept,
        TryFinally,
    };

private:
    Kind _kind;

protected:
    Statement(Kind kind, Location location)
        : BaseNode(BaseNode::Kind::Statement, location), _kind(kind) {}

public:
    auto kind() const { return _kind; }
};

class Decl : public Statement {
    std::string _name;

protected:
    Decl(Kind kind, Location location, std::string name)
        : Statement(kind, location), _name(std::move(name)) {}

public:
    auto name() const -> const std::string& { return _name; }

    static bool classof(const Statement* statement) { return +statement->kind() >= +Kind::DeclStart_ and +statement->kind() <= +Kind::DeclEnd_; }
};

class Expr : public BaseNode {
public:
    enum struct Kind {
        Name,
        MemberSelect,
        Subscript,

        Unary,
        Binary,
        Ternary,
        Typecast,
        Comma,
        Grouped,
        // TODO(local): statically know different initializer kinds?
        Initializer,
        Call,
        Assign,

        GenericSelection,

        LitInt,
        LitFloat,
        LitChar,
        LitString,
    };

private:
    Kind _kind;

protected:
    Expr(Kind kind, Location location)
        : BaseNode(BaseNode::Kind::Expr, location), _kind(kind) {}

public:
    auto kind() const { return _kind; }

    bool is_lvalue() const;
};

class UnaryExpr : public Expr {
    OperatorKind _operator_kind;
    Expr* _operand;

public:
    UnaryExpr(Location location, OperatorKind operator_kind, Expr* operand)
        : Expr(Kind::Unary, location), _operator_kind(operator_kind), _operand(operand) {}

    auto operator_kind() const { return _operator_kind; }
    auto operand() const { return _operand; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::Unary; }
};

class GroupedExpr : public Expr {
    Expr* _expr;

public:
    GroupedExpr(Expr* expr)
        : Expr(Kind::Grouped, expr->location()), _expr(expr) {}

    auto expr() const { return _expr; }

    static bool classof(const Expr* expr) { return expr->kind() == Kind::Grouped; }
};

class Type : public BaseNode {
public:
    enum struct Kind {
        Integer,
        Float,

        Pointer,
        Array,
        Struct,
        Union,
        Enum,
        Typedef,
        Function,
    };

private:
    Kind _kind;

protected:
    Type(Kind kind, Location location)
        : BaseNode(BaseNode::Kind::Type, location), _kind(kind) {}

public:
    auto kind() const { return _kind; }
};
} // namespace lcc::c

#endif // C_AST_HH
