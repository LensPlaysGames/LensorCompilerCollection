#include <fmt/format.h>
#include <lcc/file.hh>

int main(int argc, const char** argv) {
    // Get SARIF files from command line arguments
    for (auto i = 1; i < argc; ++i) {
        auto contents = lcc::File::Read(argv[i]);
        // TODO: Parse contents as JSON/SARIF.
    }

    // TODO: Display results of SARIF in a reasonable and uniform way.

    // TODO: Emit website (HTML) that may be viewed in a browser.

    return 0;
}
