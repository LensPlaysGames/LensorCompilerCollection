#include <algorithm>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <string>

#include <cstring>
#include <cstdio>

#ifdef _WIN32
# define PLATFORM_EXE_PREFIX ""
# define CALLING_CONVENTION "MSWIN"
#else
# define PLATFORM_EXE_PREFIX "./"
# define CALLING_CONVENTION "LINUX"
#endif

int main(int argc, char **argv) {
    // Expecting signature:
    // <invocation> `--test` <test-path> `--intc` <intc-path> `--cc` <c-compiler-path>
    if (argc != 7) return 127;
    if (strcmp(argv[1], "--test") != 0) return 127;
    if (strcmp(argv[3], "--intc") != 0) return 127;
    if (strcmp(argv[5], "--cc") != 0) return 127;

    std::filesystem::path testpath{argv[2]};
    std::filesystem::path intcpath{argv[4]};
    std::filesystem::path ccpath{argv[6]};

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
    while (line.starts_with("; LABELS")) {
        if (!std::getline(testfile, line)) {
            fprintf(stderr, "Sorry, but the test file at \"%s\" appears to be malformed (LABELS nonsense)\n", testpath.string().c_str());
            return 127;
        }
    }
    if (!line.starts_with("; ")) {
        fprintf(stderr,
                "Sorry, but the test file at \"%s\" appears to be a malformed test.\n"
                "There must be a \"; \" at the beginning of the first line followed by either\n"
                "\"ERROR\", \"SKIP\", or an integer status code that is the expected return value.\n",
                testpath.string().c_str());
        return 127;
    }
    bool expected_error{false};
    int expected_status{0};
    if (line.substr(2, 4) == "SKIP") {
        return 0;
    } else if (line.substr(2, 5) == "ERROR") {
        expected_error = true;
    } else {
        // I fucking hate exceptions.
        try {
            expected_status = std::stoi(line.substr(2));
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
        if (!line.starts_with("; ")) break;
        expected_output += line.substr(2) + "\n";
    }

    testfile.close();


    std::filesystem::path intc_outpath{"test"};
    intc_outpath += testpath.filename().string();
    intc_outpath += ".S";
    std::string intc_invocation{};
    intc_invocation += intcpath.string();
    // Set calling convention
    intc_invocation += " -cc ";
    intc_invocation += CALLING_CONVENTION;
    // Set output file
    intc_invocation += " -o ";
    intc_invocation += intc_outpath.string();
    intc_invocation += " ";
    // Path to file to compile
    intc_invocation += testpath.string();

    std::filesystem::path cc_outpath{"test"};
    cc_outpath += testpath.filename().string();
    std::string cc_invocation{};
    cc_invocation += ccpath.string();
    cc_invocation += " -o ";
    cc_invocation += cc_outpath.string();
    cc_invocation += " ";
    cc_invocation += intc_outpath.string();

    std::filesystem::path outpath{"out"};
    outpath += testpath.filename().string();
    outpath += ".txt";
    std::string test_invocation{};
    test_invocation += PLATFORM_EXE_PREFIX;
    test_invocation += cc_outpath.string();
    test_invocation += " > ";
    test_invocation += outpath.string();

    int status = 0;

    status = system(intc_invocation.c_str());
    // TODO: Error on ICE no matter what. Check output for "Internal Compiler Error".
    // This means we'd have to redirect and capture intc_invocation output as well.
    if (expected_error) {
        if (!status) {
            // Delete generated output file (presumably created since compiler returned success code)
            std::filesystem::remove(intc_outpath);

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
    } else if (status) {
        // Delete generated output file (possibly created even though the compiler returned failure code)
        std::filesystem::remove(intc_outpath);

        fprintf(stderr,
                "\nFAILURE: intc returned non-zero exit code\n"
                "  intc_invocation: \"%s\"\n"
                "  return status:   %d\n",
                intc_invocation.c_str(),
                status);
        return 127;
    }

    status = system(cc_invocation.c_str());

    // As the C compiler has already used it, we can delete the intc-
    // generated assembly now.
    std::filesystem::remove(intc_outpath);

    if (status) {
        fprintf(stderr,
                "\nFAILURE: C compiler returned non-zero exit code\n"
                "  intc_invocation: \"%s\"\n"
                "  cc_invocation:   \"%s\"\n"
                "  return status:   %d\n",
                intc_invocation.c_str(),
                cc_invocation.c_str(),
                status);
        return status;
    }

    status = system(test_invocation.c_str());

    // The test has been run; we can delete the executable now.
    std::filesystem::remove(cc_outpath);

    if (status != expected_status) {
        // The test's status didn't match; it's output isn't ever needed.
        std::filesystem::remove(outpath);

        fprintf(stderr,
                "\nFAILURE: Test returned unexpected exit code\n"
                "  intc_invocation: \"%s\"\n"
                "  cc_invocation:   \"%s\"\n"
                "  test_invocation: \"%s\"\n"
                "  return status:   %d\n"
                "  expected:        %d\n",
                intc_invocation.c_str(),
                cc_invocation.c_str(),
                test_invocation.c_str(),
                status,
                expected_status);
        return status;
    }

    std::fstream test_output(outpath);
    std::string output {
        std::istreambuf_iterator<char>(test_output),
        std::istreambuf_iterator<char>()
    };
    // Delete test output file
    test_output.close();
    std::filesystem::remove(outpath);

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
