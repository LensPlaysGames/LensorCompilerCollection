#ifndef LCC_ALWAYS_FALSE_HH
#define LCC_ALWAYS_FALSE_HH

// For use in static_assert, such that the given type gets included in the
// message...
template <class... T>
constexpr bool always_false = false;

#endif /* LCC_ALWAYS_FALSE_HH */
