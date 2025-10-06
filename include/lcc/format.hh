#ifndef LCC_FORMAT_HH
#define LCC_FORMAT_HH

// NOTE: I don't know why in the fuck some idiot made this so ass-
// backwards complicated just for an enum.

namespace lcc {

// Forward decl; see bottom of file for real one.
namespace detail {
struct Formats;
}

class Format {
public:
    enum _Format {
        INVALID = 0,

        // Lensor Compiler Collection Intermediate Representation lowered for
        // target architecture.
        // Emits `.lcc` files.
        LCC_IR,

        // Lensor Compiler Collection Intermediate Representation in SSA form.
        // Emits `.lcc` files.
        LCC_SSA_IR,

        // NOTE: No longer maintained.
        // LLVM's Textual IR (as of LLVM 16.0.4)
        LLVM_TEXTUAL_IR,

        // GNU's `as` assembler. Emits `.s` files.
        GNU_AS_ATT_ASSEMBLY,

        // ELF object file; the Executable and Linkable Format
        ELF_OBJECT,

        // COFF object file; the Common Object File Format
        COFF_OBJECT,
    };

private:
    friend lcc::detail::Formats;
    _Format _format{INVALID};

    constexpr Format() = default;

public:
    enum _Format format() const { return _format; }

    static const Format* const lcc_ir;
    static const Format* const lcc_ssa_ir;
    static const Format* const llvm_textual_ir;
    static const Format* const gnu_as_att_assembly;
    static const Format* const elf_object;
    static const Format* const coff_object;
};

namespace detail {
struct Formats {
    static constexpr Format lcc_ir = [] {
        auto f = Format();
        f._format = Format::LCC_IR;
        return f;
    }();

    static constexpr Format lcc_ssa_ir = [] {
        auto f = Format();
        f._format = Format::LCC_SSA_IR;
        return f;
    }();

    static constexpr Format llvm_textual_ir = [] {
        auto f = Format();
        f._format = Format::LLVM_TEXTUAL_IR;
        return f;
    }();

    static constexpr Format gnu_as_att_assembly = [] {
        auto f = Format();
        f._format = Format::GNU_AS_ATT_ASSEMBLY;
        return f;
    }();

    static constexpr Format elf_object = [] {
        auto f = Format();
        f._format = Format::ELF_OBJECT;
        return f;
    }();

    static constexpr Format coff_object = [] {
        auto f = Format();
        f._format = Format::COFF_OBJECT;
        return f;
    }();
};
} // namespace detail

constexpr inline const Format* const Format::lcc_ir = &detail::Formats::lcc_ir;
constexpr inline const Format* const Format::lcc_ssa_ir = &detail::Formats::lcc_ssa_ir;
constexpr inline const Format* const Format::llvm_textual_ir = &detail::Formats::llvm_textual_ir;
constinit inline const Format* const Format::gnu_as_att_assembly = &detail::Formats::gnu_as_att_assembly;
constinit inline const Format* const Format::elf_object = &detail::Formats::elf_object;
constinit inline const Format* const Format::coff_object = &detail::Formats::coff_object;

} // namespace lcc

#endif /* LCC_FORMAT_HH */
