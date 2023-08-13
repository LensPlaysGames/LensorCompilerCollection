#include <lcc/utils.hh>
#include <lcc/diags.hh>
#include <clopts.hh>

namespace detail {
void aluminium_handler() {
#if defined(WIN32) || defined(_WIN32) || defined(__WIN32__) || defined(__NT__)
    // Windows
    system("start https://www.youtube.com/watch?v=dQw4w9WgXcQ");
#elif __APPLE__
    // Apple (iOS, OS X, watchOS...)
    system("open https://www.youtube.com/watch?v=dQw4w9WgXcQ");
#elif __linux__ || __unix__
    // Linux or unix-based
    system("xdg-open https://www.youtube.com/watch?v=dQw4w9WgXcQ");
#endif
}

using namespace command_line_options;
using options = clopts< // clang-format off
    help<>,
    option<"-o", "Path to the output filepath where target code will be stored">,
    option<"--output", "Path to the output filepath where target code will be stored">,
    func<"--aluminium", "That special something to spice up your compilation", aluminium_handler>,
    multiple<positional<"filepath", "Path to files that should be compiled">>
>; // clang-format on
}
using detail::options;

int main(int argc, char** argv) {
    options::parse(argc, argv);

    // Get input files
    std::vector<std::string> input_files = *detail::options::get<"filepath">();

    // Print out input files
    fmt::print("Input files:\n");
    for (const auto& input_filepath : input_files)
        fmt::print("- {}\n", input_filepath);

    lcc::Diag::Fatal("Driver not fully implemented");
}
