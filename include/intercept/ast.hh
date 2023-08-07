#ifndef INTERCEPT_AST_HH
#define INTERCEPT_AST_HH

#include "lcc/utils.hh"

namespace intercept {
    enum struct Linkage {
        /// Local variable.
        ///
        /// This is just a dummy value that is used for local variables
        /// only. In particular, a top-level declaration that is marked
        /// as local is treated as a variable local to the top-level
        /// function.
        LocalVar,

        /// Not exported. Will be deleted if unused.
        ///
        /// This is used for variables and functions that are defined in
        /// and local to this module. A variable or function marked with
        /// this attribute will be *deleted* if it is not used anywhere
        /// and will not be accessible to outside code.
        Internal,

        /// Like internal, but will not be deleted.
        ///
        /// This is for variables and functions that are not really exported
        /// and behave just like internal variables and functions, except that
        /// their name will be included in the object file’s symbol table.
        Used,

        /// Exported. May be used by other modules.
        ///
        /// This is used for variables and functions that are defined in
        /// this module and exported. Variables and functions marked with
        /// this attribute will not be deleted even if they are not
        /// referenced anywhere.
        Exported,

        /// Imported from another module or from C.
        ///
        /// This is used for variables and functions imported from outside
        /// code, whether via importing an Intercept module or simply declaring
        /// an external symbol. This linkage type means that the object is
        /// not defined in this module and that it will be made accessible at
        /// link time only. However, this module will not export the symbol.
        Imported,

        /// Imported *and* exported.
        ///
        /// This sort of combines exported and imported in that it means that
        /// the symbol is exported from this module, which will make it accessible
        /// to other *Intercept modules* that import this module, but unlike
        /// regular exports, this module does not have a definition of the symbol.
        Reexported,
    };

    class Type {
    public:
        enum struct TypeKind {
            Primitive,
            Named,
            Pointer,
            Reference,
            Array,
            Function,
            Struct,
            Integer,
        };

    private:
        const TypeKind _kind;

    protected:
        Type(TypeKind kind) : _kind(kind) { }

    public:
        virtual ~Type() = default;

        void* operator new(size_t) = delete;

        TypeKind kind() const { return _kind; }
    };

    /// @brief Base class for expression syntax nodes.
    class Expr {
    public:
        enum struct ExprKind
        {
            Function,
            Decl,

            Literal,

            If,
            While,
            For,

            Block,
            Return,

            Call,
            IntrinsicCall,
            Cast,
            Unary,
            Binary,
            
            FunctionReference,
            ModuleReference,
            VariableReference,
            MemberAccess,
        };

    private:
        const ExprKind _kind;
    
    protected:
        Expr(ExprKind kind) : _kind(kind) { }

    public:
        virtual ~Expr() = default;

        void* operator new(size_t) = delete;

        ExprKind kind() const { return _kind; }
    };

    class Decl : public Expr {
        Linkage _linkage;
        std::string _name;
        Expr* _init;

    public:
        Decl(Linkage linkage, std::string name, Expr* init)
            : Expr(ExprKind::Decl), _linkage(linkage), _name(name), _init(init) { }
            
        Linkage linkage() const { return _linkage; }
        std::string name() const { return _name; }
        Expr* init() const { return _init; }

        static bool classof(Expr* expr) { return expr->kind() == ExprKind::Decl; }
    };

    class Function : public Expr {
        Linkage _linkage;
        std::string _name;
        std::vector<Decl*> _params;
        Expr* _body;

    public:
        Function(Linkage linkage, std::string name, std::vector<Decl*> params, Expr* body)
            : Expr(ExprKind::Function), _linkage(linkage), _name(name),
              _params(params), _body(body) { }

        Linkage linkage() const { return _linkage; }
        std::string name() const { return _name; }
        std::vector<Decl*> params() const { return _params; }
        Expr* body() const { return _body; }

        static bool classof(Expr* expr) { return expr->kind() == ExprKind::Function; }
    };

    class Literal : public Expr {
    };
}

#endif // INTERCEPT_AST_HH
