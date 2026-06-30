#ifndef LCC_PLATFORM_HH
#define LCC_PLATFORM_HH

/// *DO NOT* include any system headers (e.g. unistd.h, windows.h, ...)
/// here!! The API below is platform-agnostic.

namespace lcc::platform {

/// Print a stack trace.
void PrintBacktrace();

/// Check whether stdout is a terminal.
bool StdoutIsTerminal();

/// Check whether stderr is a terminal.
bool StderrIsTerminal();

} // namespace lcc::platform

#endif // LCC_PLATFORM_HH
