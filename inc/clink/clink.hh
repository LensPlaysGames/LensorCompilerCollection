#ifndef CLINK_HH
#define CLINK_HH

#include <filesystem>
#include <vector>

namespace clink {

bool link(
    std::vector<std::filesystem::path> objects,
    std::filesystem::path executable
);

}

#endif /* CLINK_HH */
