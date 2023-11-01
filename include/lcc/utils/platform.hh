#ifndef LCC_PLATFORM_HH
#define LCC_PLATFORM_HH

/// Prefer *not* to include any system headers
/// (e.g. unistd.h, windows.h, ...) here to keep
/// the APIs below platform-agnostic.

namespace lcc::platform {
/// Print a stack trace to a FILE*.
void PrintBacktrace();

/// Check whether stdout is a terminal.
bool StdoutIsTerminal();

/// Check whether stderr is a terminal.
bool StderrIsTerminal();
} // namespace lcc::platform

#endif // LCC_PLATFORM_HH
