#include <array>
#include <algorithm>
#include <chrono>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <random>
#include <string>
#include <thread>

#include <cstring>
#include <cstdio>

#ifdef _WIN32
# define PLATFORM_EXE_SUFFIX "exe"
# define CALLING_CONVENTION "MSWIN"
#else
# include <sys/wait.h>
# define PLATFORM_EXE_SUFFIX ""
# define CALLING_CONVENTION "LINUX"
#endif

std::filesystem::path temppath(std::string_view extension) {
    std::mt19937 rd(std::random_device{}());

    /// Get the temporary directory.
    auto tmp_dir = std::filesystem::temp_directory_path();

    /// Get the current time, pid, and tid.
    auto now = std::chrono::system_clock::now().time_since_epoch().count();
    auto pid = std::to_string(rd());
    auto tid = std::to_string((uint32_t) std::hash<std::thread::id>{}(std::this_thread::get_id()));

    /// And some random letters too.
    /// Do NOT use `char` for this because it’s signed on some systems (including mine),
    /// which completely breaks the modulo operation below... Thanks a lot, C.
    std::array<uint8_t, 8> rand{};
    std::generate(rand.begin(), rand.end(), [&] () { return rd() % 26 + 'a'; });

    /// Create a unique path.
    tmp_dir /= pid; tmp_dir += ".";
    tmp_dir += tid; tmp_dir += ".";
    tmp_dir += std::to_string(now); tmp_dir += ".";
    tmp_dir += std::string_view{(char*) rand.data(), rand.size()};
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
    explicit $$defer_type(callable&& _cb) : cb(std::forward<callable>(_cb)) {}
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
#pragma GCC poison system
#endif

int main(int argc, char **argv) {
    // Expecting signature:
    // <invocation> `--target` <target> `--test` <test-path> `--intc` <intc-path> `--cc` <c-compiler-path> [ `-O` ]
    // <target> is passed directly to intc after `-t`
    if (argc < 9 || argc > 10) return 127;
    if (strcmp(argv[1], "--target") != 0) return 127;
    if (strcmp(argv[3], "--test") != 0) return 127;
    if (strcmp(argv[5], "--intc") != 0) return 127;
    if (strcmp(argv[7], "--cc") != 0) return 127;
    if (argc == 10 && strcmp(argv[9], "-O") != 0) return 127;

    bool optimise = false;
    if (argc == 8) optimise = true;

    std::string intc_target{argv[2]};
    std::filesystem::path testpath{argv[4]};
    std::filesystem::path intcpath{argv[6]};
    std::filesystem::path ccpath{argv[8]};

    if (!std::filesystem::exists(testpath)) {
        fprintf(stderr, "Sorry, but the test specified at \"%s\" does not exist\n", testpath.string().c_str());
        return 127;
    }
    if (!std::filesystem::exists(intcpath)) {
        fprintf(stderr, "Sorry, but the intc compiler specified at \"%s\" does not exist\n", intcpath.string().c_str());
        return 127;
    }

    // Parse expected test results
    std::ifstream testfile(testpath);
    std::string line{};
    if (!std::getline(testfile, line)) {
        fprintf(stderr, "Sorry, but the test file at \"%s\" appears to be empty\n", testpath.string().c_str());
        return 127;
    }
    while (line.starts_with(";; LABELS")) {
        if (!std::getline(testfile, line)) {
            fprintf(stderr, "Sorry, but the test file at \"%s\" appears to be malformed (LABELS nonsense)\n", testpath.string().c_str());
            return 127;
        }
    }
    if (!line.starts_with(";; ")) {
        fprintf(stderr,
                "Sorry, but the test file at \"%s\" appears to be a malformed test.\n"
                "There must be a \";; \" at the beginning of the first line followed by either\n"
                "\"ERROR\", \"SKIP\", or an integer status code that is the expected return value.\n",
                testpath.string().c_str());
        return 127;
    }
    bool expected_error{false};
    int expected_status{0};
    if (line.substr(3, 4) == "SKIP") {
        return 0;
    } else if (line.substr(3, 5) == "ERROR") {
        expected_error = true;
    } else {
        // I fucking hate exceptions.
        try {
            expected_status = std::stoi(line.substr(3));
        }
        catch (std::exception e) {
            fprintf(stderr,
                    "Sorry, an exception occured while parsing expected return status from first line\n"
                    "  first line: \"%s\"\n"
                    "  exception: \"%s\"\n",
                    line.c_str(),
                    e.what());
            return 127;
        }
    }
    std::string expected_output{};
    for (; std::getline(testfile, line);) {
        if (!line.starts_with(";; ")) break;
        expected_output += line.substr(3) + "\n";
    }

    testfile.close();


    std::filesystem::path intc_outpath{};
    if (intc_target.starts_with("asm")) intc_outpath = temppath("s");
    else intc_outpath = temppath(intc_target == "llvm" ? "ll" : "o");

    std::string intc_invocation{};
    intc_invocation += intcpath.string();
    // Set calling convention
    intc_invocation += " -cc ";
    intc_invocation += CALLING_CONVENTION;
    // Set target
    intc_invocation += " -t ";
    intc_invocation += intc_target;
    // Set output file
    intc_invocation += " -o ";
    intc_invocation += intc_outpath.string();
    intc_invocation += " ";
    if (optimise) intc_invocation += " -O ";
    // Path to file to compile
    intc_invocation += testpath.string();

    std::filesystem::path cc_outpath = temppath(PLATFORM_EXE_SUFFIX);
    std::string cc_invocation{};
    cc_invocation += ccpath.string();
    cc_invocation += " -o ";
    cc_invocation += cc_outpath.string();
    cc_invocation += " ";
    cc_invocation += intc_outpath.string();

    std::filesystem::path outpath = temppath("txt");
    std::string test_invocation = cc_outpath.is_absolute() ? cc_outpath : std::filesystem::current_path() / cc_outpath;
    test_invocation += " > ";
    test_invocation += outpath.string();

    auto status = run_command(intc_invocation);
    // Delete generated output file at end of scope.
    defer {
        if (std::filesystem::exists(intc_outpath))
            std::filesystem::remove(intc_outpath);
    };
    // TODO: Error on ICE no matter what. Check output for "Internal Compiler Error".
    // This means we'd have to redirect and capture intc_invocation output as well.
    if (expected_error) {
        if (status.success) {
            fprintf(stderr,
                    "\nFAILURE: Test returned successful exit code but an error was expected\n"
                    "  intc_invocation: \"%s\"\n"
                    "  cc_invocation:   \"%s\"\n"
                    "  test_invocation: \"%s\"\n",
                    intc_invocation.c_str(),
                    cc_invocation.c_str(),
                    test_invocation.c_str());
            return 127;
        }
        // If status is non-zero (unsucessful) and error was expected, we good.
        return 0;
    }
    if (not status.success) {
        fprintf(stderr,
                "\nFAILURE: intc did not exit successfully\n"
                "  intc_invocation: \"%s\"\n"
                "  return status:   %d\n",
                intc_invocation.c_str(),
                status.raw_code);
        return 127;
    }

    status = run_command(cc_invocation);
    defer {
        if (std::filesystem::exists(cc_outpath))
            std::filesystem::remove(cc_outpath);
    };

    if (not status.success) {
        fprintf(stderr,
                "\nFAILURE: C compiler errored\n"
                "  intc_invocation: \"%s\"\n"
                "  cc_invocation:   \"%s\"\n"
                "  return status:   %d\n",
                intc_invocation.c_str(),
                cc_invocation.c_str(),
                status.raw_code);
        return 1;
    }

    status = run_command(test_invocation);
    defer {
        if (std::filesystem::exists(outpath))
            std::filesystem::remove(outpath);
    };

#ifdef __linux__
    if (not status.exited) {
        fprintf(stderr,
                "\nFAILURE: Test was terminated by signal\n"
                "  intc_invocation: \"%s\"\n"
                "  cc_invocation:   \"%s\"\n"
                "  test_invocation: \"%s\"\n"
                "  signal:          %d\n",
                intc_invocation.c_str(),
                cc_invocation.c_str(),
                test_invocation.c_str(),
                WTERMSIG(status.code));
        return 1;
    }
#endif

    if (status.code != expected_status) {
        fprintf(stderr,
                "\nFAILURE: Test returned unexpected exit code\n"
                "  intc_invocation: \"%s\"\n"
                "  cc_invocation:   \"%s\"\n"
                "  test_invocation: \"%s\"\n"
                "  return code:     %d\n"
                "  expected:        %d\n",
                intc_invocation.c_str(),
                cc_invocation.c_str(),
                test_invocation.c_str(),
                status.code,
                expected_status);
        return 1;
    }

    std::fstream test_output(outpath);
    std::string output {
        std::istreambuf_iterator<char>(test_output),
        std::istreambuf_iterator<char>()
    };
    // Delete test output file
    test_output.close();

    // NOTE: there is some nonsense here having to do with newlines
    // being automatically added/translated into \r\n from \n, etc.
    // Basically, expected_output *always* has a newline added, so this should too.
    if (!output.empty() && !output.ends_with("\n")) output += "\n";

    // Remove all '\r' from both strings, just in case...
    output.erase(std::remove_if(output.begin(),
                                output.end(),
                                [](const char c){ return c == '\r'; }),
                 output.end());
    expected_output.erase(std::remove_if(expected_output.begin(),
                                         expected_output.end(),
                                         [](const char c){ return c == '\r'; }),
                          expected_output.end());

    printf("\noutput:\n");
    for (const char c : output) {
        printf("%d\n", c);
    }
    printf("expected:\n");
    for (const char c : expected_output) {
        printf("%d\n", c);
    }

    if (output != expected_output) {
        fprintf(stderr,
                "\nFAILURE: Test generated unexpected output\n"
                "  intc_invocation: \"%s\"\n"
                "  cc_invocation:   \"%s\"\n"
                "  test_invocation: \"%s\"\n"
                "  output: (until ---)\n"
                "%s---\n"
                "  expected:\n"
                "%s---\n",
                intc_invocation.c_str(),
                cc_invocation.c_str(),
                test_invocation.c_str(),
                output.c_str(),
                expected_output.c_str());
        return 127;
    }

    return 0;
}
