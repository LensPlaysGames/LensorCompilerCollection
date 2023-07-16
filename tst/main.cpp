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
#include <regex>
#include <string>
#include <thread>

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

#define ARG_NO_ESCAPE "\x01"
#define ARG_NONE ARG_NO_ESCAPE

#define ASSERT(cond, ...)                     \
    do {                                      \
        if (not(cond)) fatal("" __VA_ARGS__); \
    } while (0)

#define VERBOSE(...)                 \
    do {                             \
        if (options::get<"-v">()) {  \
            fmt::print(__VA_ARGS__); \
            fmt::print("\n");        \
            std::fflush(stdout);     \
        }                            \
    } while (0)

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
    std::string escaped_command_line;
    std::uint8_t code{};
    bool success{};
    bool exited{};
    int raw_code{};
};

/// Check a condition and exit if it isn’t true.
template <typename... arguments>
[[noreturn]] void fatal(fmt::format_string<arguments...> fmt, arguments&&...args) {
    fmt::print(stderr, fmt, std::forward<arguments>(args)...);
    fmt::print(stderr, "\n");
    std::exit(127);
}

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

/// Append a command-line argument to a string.
///
/// This wraps the argument in double quotes and escapes any double quotes
/// and backslashes in the argument.
void write_command_line_arg(std::string& to, std::string_view arg) {
    /// Append unescaped arguments.
    if (arg.starts_with(ARG_NO_ESCAPE)) {
        /// Skip empty arguments.
        if (arg.empty()) return;

        /// Append the argument.
        arg.remove_prefix(sizeof(ARG_NO_ESCAPE) - 1);
        to += arg;
        return;
    }

    /// If the argument contains no spaces or double quotes,
    /// we can just append it as-is.
    if (arg.find_first_of(" \"") == std::string_view::npos) {
        to += arg;
        return;
    }

    /// Append escaped arguments.
    to += '\"';
    for (;;) {
        /// If there are no more escape characters, we’re done.
        auto escape = arg.find_first_of("\"");
        if (escape == std::string_view::npos) {
            to += arg;
            break;
        }

        /// Write string up to the escape character.
        to += arg.substr(0, escape);

        /// Write the escape character.
        to += '\\';
        to += arg[escape];

        /// Skip the escape character.
        arg = arg.substr(escape + 1);
    }
    to += '\"';
}

/// Use this instead of system().
///
/// \param executable The path of the executable to run.
/// \param args The command line arguments. These will be escaped and quoted,
///        unless they start with ARG_NO_ESCAPE, in which case ARG_NO_ESCAPE
///        is removed from the argument and it is added unquoted and unescaped.
///        Any arguments that are equal to ARG_NONE are ignored.
/// \return The exit status of the process.
auto run_command(const fs::path& executable, auto&&...args) -> exit_status {
    ASSERT(not executable.empty(), "run_command: executable cannot be empty");
    std::string command_line;

#ifndef _WIN32
    /// On POSIX systems, escape the executable like a regular argument.
    write_command_line_arg(command_line, executable.string());
#else
    /// On Windows, the executable name must be a valid path, which means
    /// we need to escape the path segments. First, canonicalise the path.
    std::error_code ec;
    auto canonical = fs::canonical(executable, ec);
    ASSERT(not ec);

    /// Convert to a string.
    auto path_str = canonical.string();
    std::string_view remaining = path_str;
    for (;;) {
        /// Get the next path segment.
        auto sep = remaining.find_first_of("/\\");
        if (sep == std::string::npos) {
            if (command_line.empty()) command_line += remaining;
            else write_command_line_arg(command_line, remaining);
            break;
        }

        /// Write the path segment up to the separator. Take care
        /// not to escape the first part of the path since that
        /// is invalid on Windows.
        if (command_line.empty()) command_line += remaining.substr(0, sep);
        else write_command_line_arg(command_line, remaining.substr(0, sep));

        /// Remove the segment we just appended and skip the 
        /// separator. Since this is a path, it cannot contain
        /// slashes or backslashes, so no need to check for
        /// that.
        remaining = remaining.substr(sep + 1);

        /// Done.
        if (remaining.empty()) break;

        /// Add a path separator.
        command_line += '\\';
    }

    /// Remove trailing path separators, if any.
    if (command_line.ends_with('\\')) command_line.pop_back();
    else if (command_line.ends_with("\\\"")) {
        command_line.erase(command_line.size() - 2);
        command_line += '\"';
    }
#endif

    /// Append the command line arguments.
    ((command_line += ' ', write_command_line_arg(command_line, args)), ...);

    /// Run the command.
    auto code = std::system(command_line.data());
    exit_status st;
    st.raw_code = code;
    st.escaped_command_line = std::move(command_line);

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
    option<"--ld", "The linker to use", std::string, true>,
    flag<"-O", "Whether to enable optimisations">,
    flag<"-v", "Enable verbose output">,
    help<>
>; // clang-format on
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
    /// Disable abort popup on Windows.
#ifdef _WIN32
    _set_abort_behavior(0, _WRITE_ABORT_MSG | _CALL_REPORTFAULT);
#endif

    using detail::options;
    options::parse(argc_, argv_);

    fs::path testpath{*options::get<"--test">()};
    fs::path intcpath{*options::get<"--intc">()};
    fs::path ldpath{*options::get<"--ld">()};

    ASSERT(fs::exists(testpath), "Sorry, but the test specified at \"{}\" does not exist", testpath);
    VERBOSE("Found test file at {}", testpath);
    ASSERT(fs::exists(intcpath), "Sorry, but the intc compiler specified at \"{}\" does not exist", intcpath);
    VERBOSE("Using Intercept compiler at {}", intcpath);
    ASSERT(fs::exists(ldpath), "Sorry, but the linker specified at \"{}\" does not exist", ldpath);
    VERBOSE("Using linker at {}", ldpath);

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
    if (line == ";; SKIP") {
        VERBOSE("Skipping test {}", testpath);
        return 0;
    }

    if (line == ";; ERROR") {
        VERBOSE("Test should error");
        expected_error = true;
    } else {
        char *end;
        errno = 0;
        expected_status = (int) std::strtoull(line.c_str() + 3, &end, 10);
        ASSERT(
            errno == 0,
            "Expected exit code must be an integer, but was \"{}\"",
            std::string_view{line}.substr(3)
        );
        VERBOSE("Expected exit code: {}", expected_status);
    }

    std::string expected_output{};
    while (std::getline(testfile, line)) {
        if (not line.starts_with(";; ")) break;
        expected_output += std::string_view{line}.substr(3);
        expected_output += "\n";
    }

    testfile.close();
    VERBOSE("Expected output: {:?}", expected_output);

    /// Construct Intercept compiler invocation.
    fs::path intc_outpath{}, intc_logpath = temppath("log");
    if (options::get<"--target">()->starts_with("asm")) intc_outpath = temppath("s");
    else intc_outpath = temppath(*options::get<"--target">() == "llvm" ? "ll" : "o");
    auto intc_status = run_command(
        intcpath,
        "-cc",
        CALLING_CONVENTION,
        "-t",
        *options::get<"--target">(),
        "-o",
        intc_outpath.string(),
        testpath.string(),
        options::get<"-O">() ? "-O" : ARG_NONE,
        ARG_NO_ESCAPE ">",
        intc_logpath.string(),
        ARG_NO_ESCAPE "2>&1"
    );

    VERBOSE("Intercept compiler invocation: {}", intc_status.escaped_command_line);
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
            !intc_status.success,
            "FAILURE: Intercept compiler returned successful exit code but an error was expected\n"
            "  intc_invocation: {}",
            intc_status.escaped_command_line
        );

        /// If unsuccessful and error was expected, we good.
        return 0;
    }

    /// Otherwise, the compiler invocation should have succeeded.
    ASSERT(
        intc_status.success,
        "FAILURE: intc did not exit successfully\n"
        "  intc_invocation:   {}\n"
        "  return status:     {}\n"
        "  output:\n{}",
        intc_status.escaped_command_line,
        intc_status.raw_code,
        log
    );

    /// Run linker.
    auto ld_outpath = temppath(PLATFORM_EXE_SUFFIX);
    auto ld_status = run_command(
        ldpath.string(),
        "-o",
        ld_outpath.string(),
        intc_outpath.string()
    );
    defer { delete_file(ld_outpath); };
    VERBOSE("Linker command line is: {}", ld_status.escaped_command_line);
    ASSERT(
        ld_status.success,
        "FAILURE: Linker errored\n"
        "  intc_invocation:   {}\n"
        "  ld_invocation:     {}\n"
        "  return status:     {}",
        intc_status.escaped_command_line,
        ld_status.escaped_command_line,
        ld_status.raw_code
    );

    /// Run the test.
    fs::path outpath = temppath("txt");
    auto test_status = run_command(
        ld_outpath.is_absolute() ? ld_outpath : fs::current_path() / ld_outpath,
        ARG_NO_ESCAPE ">",
        outpath.string(),
        ARG_NO_ESCAPE "2>&1"
    );
    VERBOSE("Running test executable: {}", test_status.escaped_command_line);
    defer { delete_file(outpath); };

#ifdef __linux__
    /// Check for signals on Linux.
    ASSERT(
        test_status.exited,
        "FAILURE: Test was terminated by signal\n"
        "  intc_invocation:   {}\n"
        "  ld_invocation:     {}\n"
        "  test_invocation:   {}\n"
        "  signal:            {}",
        intc_status.escaped_command_line,
        ld_status.escaped_command_line,
        test_status.escaped_command_line,
        WTERMSIG(test_status.code)
    );
#endif

    /// Check status code.
    ASSERT(
        test_status.code == expected_status,
        "FAILURE: Test returned unexpected exit code\n"
        "  intc_invocation:   {}\n"
        "  ld_invocation:     {}\n"
        "  test_invocation:   {}\n"
        "  return code:       {}\n"
        "  expected:          {}\n",
        intc_status.escaped_command_line,
        ld_status.escaped_command_line,
        test_status.escaped_command_line,
        test_status.code,
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
        "  intc_invocation: {}\n"
        "  ld_invocation:   {}\n"
        "  test_invocation: {}",
        output,
        expected_output,
        intc_status.escaped_command_line,
        ld_status.escaped_command_line,
        test_status.escaped_command_line
    );
}
