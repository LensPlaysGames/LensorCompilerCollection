#ifndef CLINK_LINK_UARCHIVE_HH
#define CLINK_LINK_UARCHIVE_HH

#include <object/generic.hh>

#include <span>
#include <string>
#include <string_view>
#include <vector>

namespace clink {

// Collect defined symbols and relocations from blob, treating it as a
// Unix archive file (i.e. created by ar and ranlib).
// @param  blob[in]  The binary bytes of the archive.
// @param  undefined[in]  The symbols we are currently looking to resolve.
// @param  visited[inout]  A list of object files we have already
//   processed, such as to avoid loading duplicates.
[[nodiscard]]
auto collect_uarchive(
    std::span<char> blob,
    std::vector<std::string_view> undefined,
    std::vector<std::string>& visited
) -> std::vector<std::vector<char>>;

}

#endif /* CLINK_LINK_UARCHIVE_HH */
