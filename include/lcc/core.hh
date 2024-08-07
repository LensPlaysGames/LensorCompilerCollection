#ifndef LCC_CORE_HH
#define LCC_CORE_HH

#include <lcc/utils.hh>

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

    /// Intercept internal calling convention.
    Intercept,

    /// Glint internal calling convention.
    Glint,
};

enum struct IntrinsicKind {
    /// Issue a software breakpoint.
    /// Operands: none
    DebugTrap,

    /// Copy memory; similar to C `memmove()`.
    /// Operands: ptr %dest, ptr %src, any_int %bytes
    MemCopy,

    /// Fill memory; similar to C `memset()`.
    /// Operands: ptr %dest, i8 %value, any_int %bytes
    MemSet,

    /// Perform a system call.
    /// Operands: any_int %syscall, any_int... %args
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

constexpr auto IsExportedLinkage(Linkage link) -> bool {
    switch (link) {
        case Linkage::LocalVar:
        case Linkage::Internal:
        case Linkage::Imported:
            return false;

        case Linkage::Used:
        case Linkage::Exported:
        case Linkage::Reexported:
            return true;
    }

    LCC_UNREACHABLE();
}

constexpr auto IsImportedLinkage(Linkage link) -> bool {
    switch (link) {
        case Linkage::LocalVar:
        case Linkage::Internal:
        case Linkage::Used:
        case Linkage::Exported:
            return false;

        case Linkage::Imported:
        case Linkage::Reexported:
            return true;
    }

    LCC_UNREACHABLE();
}

constexpr auto StringifyEnum(Linkage l) -> std::string_view {
    switch (l) {
        case Linkage::LocalVar: return "local";
        case Linkage::Internal: return "internal";
        case Linkage::Used: return "used";
        case Linkage::Exported: return "exported";
        case Linkage::Imported: return "imported";
        case Linkage::Reexported: return "reexported";
    }

    LCC_UNREACHABLE();
}

constexpr auto StringifyEnum(CallConv cc) -> std::string_view {
    switch (cc) {
        case CallConv::C: return "ccc";
        case CallConv::Intercept: return "intcc";
        case CallConv::Glint: return "glintcc";
    }

    LCC_UNREACHABLE();
}

} // namespace lcc

#endif // LCC_CORE_HH
