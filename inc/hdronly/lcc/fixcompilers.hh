#ifndef LCC_FIXCOMPILERS_HH
#define LCC_FIXCOMPILERS_HH

#include <ranges>

/** LLVM, Emscripten **/
#ifndef __cpp_lib_ranges_enumerate
// #    warning "LCC providing implementation of std::ranges::views::enumerate()"
#    define __cpp_lib__cpp_lib_ranges_enumerate
namespace std::ranges::views {
template <typename T>
auto enumerate(T&& v) {
    return std::views::zip(std::views::iota(0), v);
};
}
#endif

/** Emscripten **/
#ifdef __EMSCRIPTEN__
#    ifndef isatty
#        define isatty(...) false
// #        warning "LCC providing definition of isatty for Emscripten toolchain"
#    endif
#endif // __EMSCRIPTEN__

#endif /* LCC_FIXCOMPILERS_HH */
