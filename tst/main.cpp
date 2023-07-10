#include <algorithm>
#include <array>
#include <chrono>
#include <clopts.hh>
#include <cstdio>
#include <cstring>
#include <filesystem>
#include <fmt/format.h>
#include <fmt/std.h>
#include <fstream>
#include <iterator>
#include <random>
#include <string>
#include <thread>
#include <regex>

#ifdef _WIN32
#    define PLATFORM_EXE_SUFFIX "exe"
#    define CALLING_CONVENTION  "MSWIN"
#else
#    include <sys/wait.h>
#    define PLATFORM_EXE_SUFFIX ""
#    define CALLING_CONVENTION  "LINUX"
#endif

namespace fs = std::filesystem;
namespace chr = std::chrono;

fs::path temppath(std::string_view extension) {
    std::mt19937 rd(std::random_device{}());

    /// Get the temporary directory.
    auto tmp_dir = fs::temp_directory_path();

    /// Get the current time, pid, and tid.
    auto now = chr::system_clock::now().time_since_epoch().count();
    auto pid = std::to_string(rd());
    auto tid = std::to_string((uint32_t) std::hash<std::thread::id>{}(std::this_thread::get_id()));

    /// And some random letters too.
    /// Do NOT use `char` for this because it’s signed on some systems (including mine),
    /// which completely breaks the modulo operation below... Thanks a lot, C.
    std::array<uint8_t, 8> rand{};
    std::generate(rand.begin(), rand.end(), [&]() { return rd() % 26 + 'a'; });

    /// Create a unique path.
    tmp_dir /= pid;
    tmp_dir += fmt::format(".{}.{}.{}", tid, now, std::string_view{(char *) rand.data(), rand.size()});
    if (not extension.empty()) {
        if (not extension.starts_with('.')) tmp_dir += ".";
        tmp_dir += extension;
    }

    return tmp_dir;
}

#define CAT_(x, y) x##y
#define CAT(x, y)  CAT_(x, y)

#define defer auto CAT($$defer_instance_, __COUNTER__) = $$defer{} % [&]()

template <typename callable>
struct $$defer_type {
    callable cb;
    explicit $$defer_type(callable&& _cb)
        : cb(std::forward<callable>(_cb)) {}
    ~$$defer_type() { cb(); }
};

struct $$defer {
    template <typename callable>
    $$defer_type<callable> operator%(callable&& cb) {
        return $$defer_type<callable>{std::forward<callable>(cb)};
    }
};

/// The code returned by system() is *very* platform-specific. On Linux,
/// you have to use a bunch of macros to get the actual exit code.
struct exit_status {
    std::uint8_t code{};
    bool success{};
    bool exited{};
    int raw_code{};
};

/// Use this instead of system().
auto run_command(std::string_view command) -> exit_status {
    auto code = std::system(command.data());
    exit_status st;
    st.raw_code = code;

#ifndef _WIN32
    /// Process creation failed.
    if (code == -1) {
        st.success = false;
        st.exited = false;
        st.code = -1;
        return st;
    }

    st.exited = WIFEXITED(code);
    st.success = not WIFSIGNALED(code) and WEXITSTATUS(code) == 0;
    st.code = WEXITSTATUS(code);
#else
    st.exited = true;
    st.success = code == 0;
    st.code = std::uint8_t(code);
#endif

    return st;
}

/// Disallow using system directly for portability reasons.
#if __GNUG__ || __clang__
#    pragma GCC poison system
#endif

namespace detail {
using namespace command_line_options;
using options = clopts< // clang-format off
    option<"--target", "The target to pass to the Intercept compiler", std::string, true>,
    option<"--test", "The path to the test file", std::string, true>,
    option<"--intc", "The path to the Intercept compiler", std::string, true>,
    option<"--cc", "The C compiler to use", std::string, true>,
    flag<"-O", "Whether to enable optimisations">,
    help<>
>; // clang-format on

}

#define ASSERT(cond, ...)                     \
    do {                                      \
        if (not(cond)) fatal("" __VA_ARGS__); \
    } while (0)

/// Check a condition and exit if it isn’t true.
template <typename... arguments>
[[noreturn]] void fatal(fmt::format_string<arguments...> fmt, arguments&&...args) {
    fmt::print(stderr, fmt, std::forward<arguments>(args)...);
    fmt::print(stderr, "\n");
    std::exit(127);
}

/// Get the contents of a file.
///
/// This uses an internal function, but I wrote it so I
/// know how it’s supposed to be used.
auto map_file(const fs::path& path) -> std::string {
    namespace cmd = command_line_options;
    return cmd::detail::map_file<cmd::file_data>(path.string(), [&](std::string&& msg) {
               fatal("Failed to open output file '{}': {}", path, msg);
           })
        .contents;
}

/// Delete a file if it exists.
auto delete_file(const fs::path& path) {
    if (fs::exists(path)) fs::remove(path);
}

int main(int argc_, char **argv_) {
    using detail::options;
    options::parse(argc_, argv_);

    fs::path testpath{*options::get<"--test">()};
    fs::path intcpath{*options::get<"--intc">()};
    fs::path ccpath{*options::get<"--cc">()};

    ASSERT(fs::exists(testpath), "Sorry, but the test specified at \"{}\" does not exist", testpath);
    ASSERT(fs::exists(intcpath), "Sorry, but the intc compiler specified at \"{}\" does not exist", intcpath);

    // Parse expected test results
    std::ifstream testfile(testpath);
    std::string line{};
    ASSERT(std::getline(testfile, line), "Sorry, but the test file at \"{}\" appears to be empty", testpath);
    while (line.starts_with(";; LABELS")) {
        ASSERT(
            std::getline(testfile, line),
            "Sorry, but the test file at \"{}\" appears to be malformed (LABELS nonsense)",
            testpath
        );
    }

    ASSERT(
        line.starts_with(";; "),
        "Sorry, but the test file at \"{}\" appears to be a malformed test.\n"
        "There must be a \";; \" at the beginning of the first line followed by either\n"
        "\"ERROR\", \"SKIP\", or an integer status code that is the expected return value.",
        testpath
    );

    bool expected_error{false};
    int expected_status{0};
    if (line == ";; SKIP") return 0;
    if (line == ";; ERROR") expected_error = true;
    else {
        char *end;
        errno = 0;
        expected_status = (int) std::strtoull(line.c_str() + 3, &end, 10);
        ASSERT(
            errno == 0,
            "Expected exit code must be an integer, but was \"{}\"",
            std::string_view{line}.substr(3)
        );
    }

    std::string expected_output{};
    while (std::getline(testfile, line)) {
        if (not line.starts_with(";; ")) break;
        expected_output += std::string_view{line}.substr(3);
        expected_output += "\n";
    }

    testfile.close();

    /// Construct Intercept compiler invocation.
    fs::path intc_outpath{}, intc_logpath = temppath("log");
    if (options::get<"--target">()->starts_with("asm")) intc_outpath = temppath("s");
    else intc_outpath = temppath(*options::get<"--target">() == "llvm" ? "ll" : "o");
    auto intc_invocation = fmt::format(
        "{} -cc {} -t {} -o {} {}{} > {} 2>&1",
        intcpath,
        CALLING_CONVENTION,
        *options::get<"--target">(),
        intc_outpath,
        testpath,
        options::get<"-O">() ? " -O" : "",
        intc_logpath
    );

    auto status = run_command(intc_invocation);
    defer {
        delete_file(intc_outpath);
        delete_file(intc_logpath);
    };

    /// Check for ICEs.
    std::string log = map_file(intc_logpath);
    ASSERT(
        log.find("Internal Compiler Error") == std::string::npos,
        "Intercept compiler suffered Internal Compiler Error: {}",
        log
    );

    /// If we were expecting an error, check for that.
    if (expected_error) {
        ASSERT(
            !status.success,
            "FAILURE: Intercept compiler returned successful exit code but an error was expected\n"
            "  intc_invocation: \"{}\"",
            intc_invocation
        );

        /// If unsuccessful and error was expected, we good.
        return 0;
    }

    /// Otherwise, the compiler invocation should have succeeded.
    ASSERT(
        status.success,
        "FAILURE: intc did not exit successfully\n"
        "  intc_invocation: \"{}\"\n"
        "  return status:     {}\n"
        "  output:\n{}",
        intc_invocation,
        status.raw_code,
        log
    );

    /// Construct C compiler invocation.
    auto cc_outpath = temppath(PLATFORM_EXE_SUFFIX);
    auto cc_invocation = fmt::format(
        "{} -o {} {}",
        ccpath,
        cc_outpath,
        intc_outpath
    );

    /// Run C compiler.
    status = run_command(cc_invocation);
    defer { delete_file(cc_outpath); };
    ASSERT(
        status.success,
        "FAILURE: C compiler errored\n"
        "  intc_invocation: \"{}\"\n"
        "  cc_invocation:   \"{}\n"
        "  return status:     {}",
        intc_invocation,
        cc_invocation,
        status.raw_code
    );

    /// Output file for test.
    fs::path outpath = temppath("txt");
    auto test_invocation = fmt::format(
        "{} > {} 2>&1", /// At least this works the same on both Linux and Windows.
        cc_outpath.is_absolute() ? cc_outpath.string() : (fs::current_path() / cc_outpath).string(),
        outpath
    );

    /// Run the test.
    status = run_command(test_invocation);
    defer { delete_file(outpath); };

#ifdef __linux__
    /// Check for signals on Linux.
    ASSERT(
        status.exited,
        "FAILURE: Test was terminated by signal\n"
        "  intc_invocation: \"{}\"\n"
        "  cc_invocation:   \"{}\"\n"
        "  test_invocation: \"{}\"\n"
        "  signal:            {}",
        intc_invocation,
        cc_invocation,
        test_invocation,
        WTERMSIG(status.code)
    );
#endif

    /// Check status code.
    ASSERT(
        status.code == expected_status,
        "FAILURE: Test returned unexpected exit code\n"
        "  intc_invocation: \"{}\"\n"
        "  cc_invocation:   \"{}\"\n"
        "  test_invocation: \"{}\"\n"
        "  return code:       {}\n"
        "  expected:          {}\n",
        intc_invocation,
        cc_invocation,
        test_invocation,
        status.code,
        expected_status
    );

    /// Get test output.
    std::string output = map_file(outpath);

    /// Convert \r\n to \n.
    output = std::regex_replace(output, std::regex{"\r\n"}, "\n");

    /// Remove trailing newlines from output and expected output, for consistency.
    while (output.ends_with("\n")) output.pop_back();
    while (expected_output.ends_with("\n")) expected_output.pop_back();

    /// Check output.
    ASSERT(
        output == expected_output,
        "FAILURE: Test generated unexpected output\n"
        "  Output:          {:?}\n"
        "  Expected:        {:?}\n"
        "  intc_invocation: \"{}\"\n"
        "  cc_invocation:   \"{}\"\n"
        "  test_invocation: \"{}\"",
        output,
        expected_output,
        intc_invocation,
        cc_invocation,
        test_invocation
    );
}
