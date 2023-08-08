#ifndef LCC_DETAIL_DEFER_HH
#define LCC_DETAIL_DEFER_HH

#include <lcc/utils.hh>

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

/// \brief Defer execution of a lambda until the end of the scope.
///
/// Example:
/// \code{.cpp}
///     auto file = std::fopen(...);
///     defer { if (file) std::fclose(file); };
/// \endcode
#define defer auto LCC_CAT(_lcc_defer_, __COUNTER__) = ::lcc::detail::DeferStage1{}->*[&]

/// \brief Temporarily set a variable to a value.
///
/// Example:
/// \code{.cpp}
///     static int x = 0;
///     tempset x = 1;
///     /// x is reset to `0` at end of scope.
/// \endcode
#define tempset auto LCC_CAT(_lcc_tempset_, __COUNTER__) = ::lcc::detail::TempsetStage1{}->*

#endif // LCC_DETAIL_DEFER_HH
