#ifndef LCC_DETAIL_DEFER_HH
#define LCC_DETAIL_DEFER_HH

#include <concepts>
#include <utility>

#define LCC_STR_(X) #X
#define LCC_STR(X)  LCC_STR_(X)

#define LCC_CAT_(X, Y) X##Y
#define LCC_CAT(X, Y)  LCC_CAT_(X, Y)

namespace lcc::detail {
template <typename Callable>
struct DeferStage2 {
    Callable cb;
    ~DeferStage2() { cb(); }

    explicit DeferStage2(Callable&& _cb)
        : cb(std::forward<Callable>(_cb)) {}
};

struct DeferStage1 {
    template <typename Callable>
    DeferStage2<Callable> operator->*(Callable&& cb) {
        return DeferStage2<Callable>{std::forward<Callable>(cb)};
    }
};

template <typename Type>
struct TempsetStage3 {
    Type& ref;
    Type t;
    Type oldval;

    explicit TempsetStage3(Type& var, std::convertible_to<Type> auto&& cv)
        : ref(var), t(std::forward<decltype(cv)>(cv)) {
        oldval = std::move(ref);
        ref = std::move(t);
    }

    ~TempsetStage3() { ref = std::move(oldval); }
};

template <typename Type>
struct TempsetStage2 {
    Type& ref;

    TempsetStage2(Type& var) : ref(var) {}
    TempsetStage3<Type> operator=(std::convertible_to<Type> auto&& value) {
        return TempsetStage3<Type>{ref, std::forward<decltype(value)>(value)};
    }
};

struct TempsetStage1 {
    template <typename Type>
    TempsetStage2<Type> operator->*(Type& var) {
        return TempsetStage2<Type>{var};
    }
};

} // namespace lcc::detail

// __COUNTER__ is not standard until C29, that is late 2029
// https://isocpp.org/files/papers/P3384R0.html#rationale-for-standardization
#if defined(__clang__)
#pragma clang diagnostic ignored "-Wc2y-extensions"
#endif

// https://github.com/google/benchmark/blob/c19cfee61e136effb05a7fc8a037b0db3b13bd4c/include/benchmark/benchmark.h#L1531-L1538
// Check that __COUNTER__ is defined and that __COUNTER__ increases by 1
// every time it is expanded. X + 1 == X + 0 is used in case X is defined to be
// empty. If X is empty the expression becomes (+1 == +0).
#if defined(__COUNTER__) && (__COUNTER__ + 1 == __COUNTER__ + 0)
#define LCC_PRIVATE_UNIQUE_ID __COUNTER__
#else
#define LCC_PRIVATE_UNIQUE_ID __LINE__
#endif

/// \brief Defer execution of a lambda until the end of the scope.
///
/// Example:
/// \code{.cpp}
///     auto file = std::fopen(...);
///     defer { if (file) std::fclose(file); };
/// \endcode
#define defer auto LCC_CAT(_lcc_defer_, LCC_PRIVATE_UNIQUE_ID) = ::lcc::detail::DeferStage1{}->*[&]

/// \brief Temporarily set a variable to a value.
///
/// Example:
/// \code{.cpp}
///     static int x = 0;
///     tempset x = 1;
///     /// x is reset to `0` at end of scope.
/// \endcode
#define tempset auto LCC_CAT(_lcc_tempset_, LCC_PRIVATE_UNIQUE_ID) = ::lcc::detail::TempsetStage1{}->*

#endif // LCC_DETAIL_DEFER_HH
