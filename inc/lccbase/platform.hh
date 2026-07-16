#ifndef LCC_PLATFORM_HH
#define LCC_PLATFORM_HH

/// *DO NOT* include any system headers (e.g. unistd.h, windows.h, ...)
/// here!! The API below is platform-agnostic.

#if defined(__linux__) && defined(__GLIBC__) && ! defined(__UCLIBC__)
#    define LCC_LINUX_GLIBC
#endif

#if defined(_WIN32)
#    define LCC_WINDOWS
#endif

namespace lcc::platform {

/// Print a stack trace.
void PrintBacktrace();

/// Check whether stdout is a terminal.
bool StdoutIsTerminal();

/// Check whether stderr is a terminal.
bool StderrIsTerminal();

} // namespace lcc::platform

#endif // LCC_PLATFORM_HH
