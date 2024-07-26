#ifndef LCC_DRIVER_CLI_HH
#define LCC_DRIVER_CLI_HH

#include <string>
#include <string_view>
#include <vector>

namespace cli {

struct Options {
    bool verbose{false};
    bool ast{false};
    bool ir{false};
    bool mir{false};
    bool stopat_syntax{false};
    bool stopat_sema{false};
    bool stopat_ir{false};
    bool stopat_mir{false};
    bool aluminium{false};

    std::vector<std::string> input_files{};
    std::vector<std::string> include_directories{};
    std::string output_filepath{};
    int optimisation{0};
    std::string optimisation_passes{};
    std::string color{"auto"};
    std::string language{"default"};
    std::string format{"default"};
};

auto parse(int argc, const char** argv) -> Options;

} // namespace cli

#endif /* LCC_DRIVER_CLI_HH */
