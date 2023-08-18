#ifndef C_AST_HH
#define C_AST_HH

#include "lcc/utils/result.hh"
#include "lcc/utils/rtti.hh"

#include <lcc/core.hh>
#include <lcc/diags.hh>
#include <lcc/syntax/token.hh>
#include <lcc/utils.hh>

namespace lcc::c {
class BaseNode;
class Decl;

class CompilationUnit {
    File* _file;

    std::vector<BaseNode*> all_nodes{};

    std::vector<Decl*> _top_level_decls{};

public:
    CompilationUnit(File* file)
        : _file(file) {}
    
    auto file() const { return _file; }
    
    auto add_top_level_decl(Decl* decl) { _top_level_decls.push_back(decl); }
    
    auto top_level_decls() -> const std::vector<Decl*>& { return _top_level_decls; }
    
    void print();

    friend BaseNode;
};

enum struct TokenKind {
    // Grouping Delimiters
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,
    
    // Other Delimiters
    Dot,
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
    LogicalNot,
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

std::string ToString(TokenKind kind);
std::string ToString(OperatorKind kind);

class BaseNode {
public:
    enum struct Kind {
        Statement,
        Expr,
    };

private:
    const Kind _kind;

    Location _location;

protected:
    BaseNode(Kind kind, Location location)
        : _kind(kind), _location(location) {}

public:
    void* operator new(size_t) = delete;
    void* operator new(size_t sz, CompilationUnit& cu);

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

        Block,
        Empty,
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
        FieldIndex,
        DynamicIndex,

        Unary,
        Binary,
        Ternary,
        Typecast,
        Comma,
        Grouped,
        // TODO(local): statically know different initializer kinds?
        Initializer,
        Call,

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
};
}

#endif // C_AST_HH
