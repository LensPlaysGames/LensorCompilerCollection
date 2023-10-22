#ifndef LCC_FORMAT_HH
#define LCC_FORMAT_HH

namespace lcc {

// Forward decl; see bottom of file for real one.
namespace detail {
struct Formats;
}


class Format {
public:
    enum _Format {
        INVALID = 0,

        // LLVM's Textual IR (as of LLVM 16.0.4)
        LLVM_TEXTUAL_IR,

        // GNU's `as` assembler. Emits `.s` files.
        GNU_AS_ATT_ASSEMBLY,
    };

private:
    friend lcc::detail::Formats;
    _Format _format{INVALID};

    constexpr Format() = default;

public:
    enum _Format format() const { return _format; }

    static const Format* const llvm_textual_ir;
    static const Format* const gnu_as_att_assembly;
};

namespace detail {
struct Formats {
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
};
} // namespace detail

constexpr inline const Format* const Format::llvm_textual_ir = &detail::Formats::llvm_textual_ir;
constinit inline const Format* const Format::gnu_as_att_assembly = &detail::Formats::gnu_as_att_assembly;

} // namespace lcc

#endif /* LCC_FORMAT_HH */
