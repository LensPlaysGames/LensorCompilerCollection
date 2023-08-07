#ifndef LAYE_AST_HH
#define LAYE_AST_HH

#include <lcc/utils.hh>

namespace lcc::laye {
/// @brief Base class for file header nodes.
class FileHeader {
public:
    enum {
        Import,
        ForeignImport,
    };

    void* operator new(size_t) = delete;
};

/// @brief Base class for statement syntax nodes.
class Statement {
public:
    enum {
        // Declarations
        DeclBinding,
        DeclFunction,
        DeclStruct,
        DeclEnum,
        DeclAlias,

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

    void* operator new(size_t) = delete;
};

/// @brief Base class for expression syntax nodes.
class Expression {
public:
    enum {
        Unary,
        Binary,

        LookupName,
        LookupPath,
        FieldIndex,
        ValueIndex,
        Slice,
        Invoke,
        Ctor,

        Cast,
        New,
        Try,
        Catch,

        LitNil,
        LitBool,
        LitString,
        LitInt,
        LitFloat,
    };

    void* operator new(size_t) = delete;
};

/// @brief Base class for type syntax nodes.
class Type {
public:
    enum struct TypeKind {
        Infer,

        Named,
        ErrUnion,

        Array,
        Slice,
        Pointer,
        Buffer,

        Func,

        LitBool,
        LitString,
        LitInt,
        LitFloat,

        Noreturn,
        Rawptr,
        Void,
        String,
        Rune,
        Bool,
        Int,
        Float,

        C,
    };

    void* operator new(size_t) = delete;
};
} // namespace laye

#endif // LAYE_AST_HH
