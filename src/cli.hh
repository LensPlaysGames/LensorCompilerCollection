#ifndef LCC_DRIVER_CLI_HH
#define LCC_DRIVER_CLI_HH

#include <string>
#include <vector>

#include <lcc/context.hh>

namespace cli {

struct Options {
    bool verbose{false};
    bool aluminium{false};
    bool ir{false};
    bool stopat_ir{false};
    bool emit_sarif{false};
    bool link{false};
    bool run{false};

    lcc::Context::OptionPrintStats print_stats{false};
    lcc::Context::OptionPrintAST ast{false};
    lcc::Context::OptionPrintMIR mir{false};
    lcc::Context::OptionStopatLex stopat_lex{false};
    lcc::Context::OptionStopatSyntax stopat_syntax{false};
    lcc::Context::OptionStopatSema stopat_sema{false};
    lcc::Context::OptionStopatMIR stopat_mir{false};
    lcc::Context::OptionDiagBacktrace diag_backtrace{false};

    std::vector<std::string> input_files{};
    std::vector<std::string> include_directories{};
    std::vector<std::string> frontend_options{};
    std::string output_filepath{};
    int optimisation{0};
    std::string optimisation_passes{};
    std::string color{"auto"};
    std::string language{"default"};
    std::string format{"default"};
    std::string target{"default"};

    bool use_colour() const;
};

auto parse(int argc, const char** argv) -> Options;

} // namespace cli

#endif /* LCC_DRIVER_CLI_HH */
