#ifndef LCC_CORE_HH
#define LCC_CORE_HH

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

enum struct IntrinsicKind {
    /// Issue a software breakpoint.
    DebugTrap,

    /// Copy memory; similar to C `memmove()`.
    MemCopy,

    /// Fill memory; similar to C `memset()`.
    MemSet,

    /// Perform a system call.
    SystemCall,
};

enum struct FuncAttr {
    /// This function, in addition to being pure, always
    /// returns the same value when passed the same arguments
    /// (so long as the memory pointed to by any pointer arguments
    /// has not changed).
    ///
    /// The intent of this attribute is to facilitate common
    /// subexpression elimination and store forwarding across
    /// calls, which is otherwise impossible. To this end, a
    /// const function additionally may not read from or write
    /// to memory, except that it may access memory that it
    /// itself allocates—though it must free it before returning—
    /// as well as read from, but not write to, any pointers passed
    /// to it.
    ///
    /// This is similar to, but not quite the same as, GCC’s
    /// or const attribute. In the LLVM backend, this is implemented
    /// as `nofree nosync memory(argmem: read, inaccessiblemem: readwrite)`.
    Const,

    /// Inline all callees except recursive tail calls.
    Flatten,

    /// Always inline this function.
    ///
    /// Unlike `always_inline` in GCC or LLVM, this causes
    /// a hard error if a function cannot be inlined.
    Inline,

    /// Never inline this function.
    NoInline,

    /// Do not optimise this function.
    NoOpt,

    /// This function never returns.
    NoReturn,

    /// This function has no side effects and that calls to
    /// it can be removed if the return value is never used.
    ///
    /// Note that this does not mean that duplicate calls to
    /// this function always have the same effect, even if the
    /// functions’ arguments are the same.
    Pure,

    /// Annotate any functions that may return twice with this
    /// (e.g. \c setjmp()). The behaviour is undefined if a
    /// function not annotated with this attribute returns twice.
    ReturnsTwice,
};
} // namespace lcc

#endif // LCC_CORE_HH
