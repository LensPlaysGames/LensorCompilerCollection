#ifndef CLINK_HH
#define CLINK_HH

#include <filesystem>
#include <string>
#include <vector>

namespace clink {

// Read object files from given input paths, and attempt to merge them,
// resolve symbols, and write relocations. Like `ld` or `mold`, but by me.
bool link(
    std::vector<std::filesystem::path> objects,
    std::filesystem::path executable
);
bool link(
    std::vector<std::string> objects_strings,
    std::string_view executable_string
);

} // namespace clink

#endif /* CLINK_HH */
