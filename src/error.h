#ifndef COMPILER_ERROR_H
#define COMPILER_ERROR_H

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <utils.h>

/// ===========================================================================
///  Enums and structures
/// ===========================================================================
enum diagnostic_level {
  DIAG_NOTE,
  DIAG_WARN,
  DIAG_ERR,
  DIAG_ICE,
  DIAG_SORRY,

  DIAG_COUNT,
};

/// Source location.
typedef struct {
  u32 start;
  u32 end;
} loc;

/// ===========================================================================
///  Error handling macros.
/// ===========================================================================
/// Raise an internal compiler error.
#define ICE(...) (raise_fatal_error_impl(__FILE__, PRETTY_FUNCTION, __LINE__, false, false, NULL, __VA_ARGS__), BUILTIN_UNREACHABLE())

/// Call this if you’re in a signal/exception handler.
#define ICE_EXCEPTION(...) (raise_fatal_error_impl(__FILE__, PRETTY_FUNCTION, __LINE__, true, false, NULL, __VA_ARGS__), BUILTIN_UNREACHABLE())

/// Usage:
///   ASSERT(condition);
///   ASSERT(condition, "format string", ...);
#define ASSERT(cond, ...) \
  ((cond) ? (void)(0) : (raise_fatal_error_impl(__FILE__, PRETTY_FUNCTION, __LINE__, false, false, #cond, "" __VA_ARGS__), BUILTIN_UNREACHABLE()))

#if __STDC_VERSION__ >= 201112L
#  define STATIC_ASSERT(...) _Static_assert(__VA_ARGS__)
#else
#  define STATIC_ASSERT(...) ASSERT(__VA_ARGS__)
#endif

#define TODO(...)     (raise_fatal_error_impl(__FILE__, PRETTY_FUNCTION, __LINE__, false, true, NULL, "" __VA_ARGS__), BUILTIN_UNREACHABLE())
#define UNREACHABLE() ICE("Unreachable")

/// ===========================================================================
///  Error handling functions.
/// ===========================================================================
EXT_FORMAT(5, 6)
void issue_diagnostic
(/// Error level and source location.
 enum diagnostic_level level,
 const char *filename,
 span source,
 loc location,

 /// The actual error message.
 const char *fmt,
 ...);

void vissue_diagnostic
(/// Error level and source location.
 enum diagnostic_level level,
 const char *filename,
 span source,
 loc location,

 /// The actual error message.
 const char *fmt,
 va_list ap);


/// Used by ASSERT()/ICE()/TODO().
/// You probably don't want to use this directly.
NORETURN
void raise_fatal_error_impl (
    const char *file,
    const char *func,
    int line,
    bool is_signal_or_exception,
    bool is_sorry,
    const char *assert_condition,
    const char *fmt,
    ...
);

#endif /* COMPILER_ERROR_H */
