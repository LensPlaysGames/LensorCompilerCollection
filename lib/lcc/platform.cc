#include <lcc/utils.hh>
#include <lcc/utils/platform.hh>

#include <cstdio>
#include <filesystem>
#include <iostream>
#include <span>
#include <string_view>

#ifdef _WIN32
#    define NOMINMAX
#    include <io.h>
#    include <Windows.h>
#    define isatty _isatty
#endif

#ifdef __linux__
#    include <execinfo.h>
#    include <unistd.h>
#endif

namespace {

}

static constexpr bool backtrace_addr2line = true;
// -p  pretty print
// -C  do demangling
// -f  function name
static constexpr std::string_view backtrace_addr2line_options = "-p -C -f";

static constexpr bool backtrace_llvm_symbolizer = false;
static constexpr std::string_view backtrace_llvm_symbolizer_options = "-s -p -C -i --color --output-style=GNU";

void lcc::platform::PrintBacktrace() {
#ifdef __linux__
    /// Get the backtrace.
    static constexpr usz size = 128;
    static void* trace[size]{};
    int n = backtrace(trace, size);

    // Skip PrintBacktrace() entry
    int skip_value = 1;

    // Use addr2line like a sane person
    if constexpr (backtrace_addr2line) {
        // Sounds crazy, but since program counter points to one past the
        // executing instruction, we need to subtract one from every address...
        for (int i = skip_value; i < n; ++i) trace[i] = (void*) ((std::uintptr_t) trace[i] - 1);

        std::span<void*> trace_view{&trace[skip_value], &trace[n]};

        std::string command = fmt::format(
            "addr2line {} -e {} {}",
            backtrace_addr2line_options,
            lcc::fs::canonical("/proc/self/exe").native(),
            fmt::join(trace_view, " ")
        );
        std::system(command.data());
    }
    // GROSS LLVM USAGE
    else if constexpr (backtrace_llvm_symbolizer) {
        /// Symboliser path.
        std::string sym = std::getenv("SYMBOLIZER_PATH") ?: "";
        if (sym.empty()) sym = "llvm-symbolizer";

        std::span<void*> trace_view{&trace[skip_value], &trace[n]};
        /// Use llvm-symbolizer to print the backtrace.
        auto cmd = fmt::format(
            "{} {} -e {} {} | awk '{{ print \"#\" NR, $0 }}'",
            sym,
            fmt::join(trace_view, " "),
            lcc::fs::canonical("/proc/self/exe").native(),
            backtrace_llvm_symbolizer_options
        );
        std::system(cmd.c_str());
    }
    // JUST LINUX <execinfo.h> BACKTRACE
    else {
        char** trace_symbols = backtrace_symbols(trace, size);
        if (not trace_symbols) {
            fmt::print("Could not pretty print backtrace: backtrace_symbols() from execinfo.h returned NULL");
            for (int i = 0; i < n; ++i)
                fmt::print("{}: {:p}\n", i, trace[i]);
        }

        // Try to figure out if this is an ICE and, if it is, skip to where that
        // was called from.
        if (n > 7 and fmt::format("{}", trace_symbols[7]).contains("ICE"))
            skip_value = 7;

        std::vector<std::string> trace_strings;
        trace_strings.reserve(lcc::usz(n));
        for (int i = skip_value; i < n; i++)
            trace_strings.emplace_back(trace_symbols[i]);

        free(trace_symbols);

        std::string_view long_path_marker{"/lcc"};
        if (trace_strings.size() and trace_strings.at(0).contains(long_path_marker)) {
            for (auto& s : trace_strings) {
                auto skip = s.find(long_path_marker);
                if (skip != std::string::npos)
                    s = s.substr(skip + long_path_marker.size());
            }
        }

        for (auto [index, s] : vws::enumerate(trace_strings))
            fmt::print("{}: {}\n", index, s);
    }
#endif
}

bool lcc::platform::StdoutIsTerminal() {
    return isatty(fileno(stdout));
}

bool lcc::platform::StderrIsTerminal() {
    return isatty(fileno(stderr));
}
