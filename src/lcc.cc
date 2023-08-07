#include <lcc/utils.hh>
#include <lcc/diags.hh>
#include <clopts.hh>

namespace detail {
using namespace command_line_options;
using options = clopts< // clang-format off
    help<>
>; // clang-format on
}
using detail::options;

int main(int argc, char** argv) {
    options::parse(argc, argv);
    lcc::Diag::Fatal("Driver not implemented");
}