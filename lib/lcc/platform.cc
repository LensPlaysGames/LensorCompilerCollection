#include <lcc/utils.hh>
#include <lcc/utils/platform.hh>

#ifdef _WIN32
#    define NOMINMAX
#    include <io.h>
#    include <Windows.h>
#    define isatty _isatty
#endif

#ifdef __linux__
#    include <execinfo.h>
#endif

namespace {

}

void lcc::platform::PrintBacktrace() {
#ifdef __linux__
    /// Get the backtrace.
    static void* trace[128];
    int n = backtrace(trace, 128);

    /// Convert to strings.
    std::vector<std::string> trace_strs;
    trace_strs.reserve(lcc::usz(n));
    for (int i = 0; i < n; i++) trace_strs.emplace_back(fmt::format("{:p}", trace[i]));

    /// Symboliser path.
    std::string sym = std::getenv("SYMBOLIZER_PATH") ?: "";
    if (sym.empty()) sym = "llvm-symbolizer";

    /// Use llvm-symbolizer to print the backtrace.
    auto cmd = fmt::format(
        "{} {} -e {} -s -p -C -i --color --output-style=GNU | awk '{{ print \"#\" NR, $0 }}'",
        sym,
        fmt::join(trace_strs, " "),
        lcc::fs::canonical("/proc/self/exe").native()
    );
    std::system(cmd.c_str());
#endif
}

bool lcc::platform::StdoutIsTerminal() {
    return isatty(fileno(stdout));
}

bool lcc::platform::StderrIsTerminal() {
    return isatty(fileno(stderr));
}
