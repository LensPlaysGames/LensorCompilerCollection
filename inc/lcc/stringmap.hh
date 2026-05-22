#ifndef LCC_STRINGMAP_HH
#define LCC_STRINGMAP_HH

#include <string>
#include <string_view>
#include <unordered_map>

namespace lcc {

namespace detail {

/// Hash for string maps.
struct StringHash {
    using is_transparent = void;

    [[nodiscard]]
    std::size_t operator()(std::string_view txt) const {
        return std::hash<std::string_view>{}(txt);
    }
    [[nodiscard]]
    std::size_t operator()(const std::string& txt) const {
        return std::hash<std::string>{}(txt);
    }

    template <typename Type>
    requires requires (const Type& t) {
        { t.size() } -> std::convertible_to<std::size_t>;
        { t.data() } -> std::convertible_to<const char*>;
    }
    [[nodiscard]]
    std::size_t operator()(const Type& txt) const {
        return std::hash<std::string_view>{}(
            std::string_view{txt.data(), txt.size()}
        );
    }
};

} // namespace detail

/// Map with heterogeneous lookup.
///
/// Use this whenever you need a \c std::unordered_map from strings
/// to some other type. The reason for this is that, by default,
/// \c std::unordered_map will require you to create a \c std::string
/// to look up a value; this is annoying if you only have a
/// \c std::string_view, because you have to create a copy just to
/// search for something.
///
/// There is an API to fix that, but it’s not exactly obvious, so this
/// takes care of that. This map type allows you to use pretty much
/// anything that supports \c data() and \c size() accessors as a key.
template <typename Type>
using StringMap = std::unordered_map<
    std::string,
    Type,
    detail::StringHash,
    std::equal_to<>>;

} // namespace lcc

#endif // LCC_STRINGMAP_HH
