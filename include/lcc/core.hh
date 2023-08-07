#ifndef LCC_CORE_HH
#define LCC_CORE_HH

#define LCC_INTRINSICS(X) \
    X(SysCall) \
    X(DebugTrap) \
    X(MemCpy)

#define LCC_FUNC_ATTR(X) \
    X(NoOptimize, no_opt) \
    X(Const, const) \
    X(Flatten, flatten) \
    X(Inline, inline) \
    X(NoInline, no_inline) \
    X(NoMangle, no_mangle) \
    X(NoReturn, no_return) \
    X(Pure, pure) \

namespace lcc {
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

enum struct CallConv {
    /// C calling convention.
    C,

    /// Laye default calling convention.
    Laye,

    /// Intercept internal calling convention.
    Intercept,
};

enum struct IntrinsicKind
{
#define X(I) I,
LCC_INTRINSICS(X)
#undef X
};

enum struct FuncAttr
{
#define X(I, J) I,
LCC_FUNC_ATTR(X)
#undef X
};
}

#endif // LCC_CORE_HH
