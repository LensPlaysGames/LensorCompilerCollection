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
} // namespace lcc::detail

#define defer auto LCC_CAT(_lcc_defer_, __COUNTER__) = ::lcc::detail::DeferStage1{}->*[&]

#endif // LCC_DETAIL_DEFER_HH
