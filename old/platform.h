#ifndef FUNCOMPILER_PLATFORM_H
#define FUNCOMPILER_PLATFORM_H

#include <stdbool.h>
#include <utils.h>

#ifndef _WIN32
#  define PLATFORM_PATH_SEPARATOR "/"
#else
#  define PLATFORM_PATH_SEPARATOR "\\"
#endif

#ifdef ENABLE_ASAN
# include <sanitizer/asan_interface.h>
# define ASAN_POISON(ptr, size) __asan_poison_memory_region(ptr, size)
# define ASAN_UNPOISON(ptr, size) __asan_unpoison_memory_region(ptr, size)
#else
# define ASAN_POISON(ptr, size) ((void) 0)
# define ASAN_UNPOISON(ptr, size) ((void) 0)
#endif

/// Initialise the program for the host platform.
void platform_init(void);

/// Check if a file descriptor is a tty.
/// \param fd The file descriptor to check.
bool platform_isatty(int fd);

/// Print the current backtrace.
/// \param ignore The number of stack frames to ignore.
void platform_print_backtrace(int ignore);

/// Read the contents of a file into a string.
///
/// \param path The path to the file to read.
/// \param success Whether the read was successful.
/// \return The contents of the file, or an error message on failure.
string platform_read_file(const char *path, bool *success);

#endif // FUNCOMPILER_PLATFORM_H
