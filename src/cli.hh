#ifndef LCC_DRIVER_CLI_HH
#define LCC_DRIVER_CLI_HH

#include <string>
#include <string_view>
#include <vector>

#include <lcc/context.hh>

namespace cli {

struct Options {
    bool verbose{false};
    bool aluminium{false};
    bool ir{false};
    bool stopat_ir{false};
    lcc::Context::OptionPrintAST ast{false};
    lcc::Context::OptionPrintMIR mir{false};
    lcc::Context::OptionStopatSyntax stopat_syntax{false};
    lcc::Context::OptionStopatSema stopat_sema{false};
    lcc::Context::OptionStopatMIR stopat_mir{false};

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
