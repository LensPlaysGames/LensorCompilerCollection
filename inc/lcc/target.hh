#ifndef LCC_TARGET_HH
#define LCC_TARGET_HH

#include <lcc/utils.hh>

// FIXME: This is a very stupid way to handle things, imo.
// We should be able to switch on a target enum, or similar.
// This nonsense is ridiculous. Whoever wrote it should be ashamed.

namespace lcc {
class Context;

namespace detail {
struct Targets;
}

/// Information about a target.
///
/// There are instances of \c Target for each supported target; there
/// should be no more than one instance of \c Target for each target.
///
/// All type sizes and alignments are in *bits*.
class Target {
    friend lcc::detail::Targets;
    constexpr Target() = default;

public:
    /// Create a copy of a target but change a few things.
    template <typename Callable>
    [[nodiscard]]
    consteval auto with(Callable c) const -> Target { return c(Target{*this}); }

    /// Available targets.
    static const Target* const x86_64_linux;
    static const Target* const x86_64_windows;

    struct {
        usz size_of_bool;
        usz size_of_byte;
        usz size_of_int;

        usz align_of_bool;
        usz align_of_byte;
        usz align_of_int;
    } glint;

    struct {
        usz size_of_bool;
        usz size_of_char;
        usz size_of_short;
        usz size_of_int;
        usz size_of_long;
        usz size_of_long_long;

        usz align_of_bool;
        usz align_of_char;
        usz align_of_short;
        usz align_of_int;
        usz align_of_long;
        usz align_of_long_long;

        bool char_is_signed;
    } ffi;

    usz size_of_pointer;
    usz align_of_pointer;

    [[nodiscard]]
    auto is_platform_linux() const -> bool {
        return this == x86_64_linux;
    }

    [[nodiscard]]
    auto is_platform_windows() const -> bool {
        return this == x86_64_windows;
    }

    [[nodiscard]]
    auto is_arch_x86_64() const -> bool {
        return this == x86_64_windows
            or this == x86_64_linux;
    }

    [[nodiscard]]
    auto is_cconv_sysv() const -> bool {
        return this == x86_64_linux;
    }

    [[nodiscard]]
    auto is_cconv_ms() const -> bool {
        return this == x86_64_windows;
    }
};

namespace detail {
/// Hack to make sure that both all targets are initialised at compile
/// time and that no-one can create targets at runtime.
struct Targets {
    static constexpr Target x86_64_linux = [] {
        Target t{};
        t.glint = {
            .size_of_bool = 8,
            .size_of_byte = 8,
            .size_of_int = 64,

            .align_of_bool = 8,
            .align_of_byte = 8,
            .align_of_int = 64,
        };

        t.ffi = {
            .size_of_bool = 8,
            .size_of_char = 8,
            .size_of_short = 16,
            .size_of_int = 32,
            .size_of_long = 64,
            .size_of_long_long = 64,

            .align_of_bool = 8,
            .align_of_char = 8,
            .align_of_short = 16,
            .align_of_int = 32,
            .align_of_long = 64,
            .align_of_long_long = 64,

            .char_is_signed = true,
        };

        t.size_of_pointer = 64;
        t.align_of_pointer = 64;
        return t;
    }();

    static constexpr Target x86_64_windows = x86_64_linux.with([](Target t) {
        t.ffi.size_of_long = 32;
        t.ffi.align_of_long = 32;
        return t;
    });
};
} // namespace detail

constexpr inline const Target* const Target::x86_64_linux = &detail::Targets::x86_64_linux;
constinit inline const Target* const Target::x86_64_windows = &detail::Targets::x86_64_windows;

} // namespace lcc

#endif // LCC_TARGET_HH
