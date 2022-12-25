#ifndef COMPILER_ERROR_H
#define COMPILER_ERROR_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string_utils.h>

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
///  Attributes and platform-specific macros.
/// ===========================================================================
#ifndef _MSC_VER
#  define NORETURN __attribute__((noreturn))
#  define FALLTHROUGH __attribute__((fallthrough))
#  define FORMAT(...) __attribute__((format(__VA_ARGS__)))
#  define FORCEINLINE __attribute__((always_inline)) inline
#  define PRETTY_FUNCTION __PRETTY_FUNCTION__
#  define NODISCARD __attribute__((warn_unused_result))
#  define BUILTIN_UNREACHABLE() __builtin_unreachable()
#else
#  define NORETURN __declspec(noreturn)
#  define FALLTHROUGH
#  define FORMAT(...)
#  define FORCEINLINE __forceinline inline
#  define PRETTY_FUNCTION __FUNCSIG__
#  define NODISCARD
#  define BUILTIN_UNREACHABLE() __assume(0)
#endif

/// ===========================================================================
///  Error handling macros.
/// ===========================================================================
/// Raise an internal compiler error.
#define ICE(...) (raise_fatal_error_impl(__FILE__, PRETTY_FUNCTION, __LINE__, false, false, NULL, __VA_ARGS__), BUILTIN_UNREACHABLE())

/// Call this if you’re in a signal handler.
#ifndef _WIN32
#  define ICE_SIGNAL(...) (raise_fatal_error_impl(__FILE__, PRETTY_FUNCTION, __LINE__, true, false, NULL, __VA_ARGS__), BUILTIN_UNREACHABLE())
#endif

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
///  Miscellaneous macros.
/// ===========================================================================
#define CAT_(a, b) a ## b
#define CAT(a, b) CAT_(a, b)

#define STR_(s) #s
#define STR(s) STR_(s)

#define VA_FIRST(X, ...) X

/// ===========================================================================
///  Error handling functions.
/// ===========================================================================
FORMAT(printf, 5, 6)
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
FORMAT(printf, 7, 8)
void raise_fatal_error_impl (
    const char *file,
    const char *func,
    int line,
    bool in_signal_handler,
    bool is_sorry,
    const char *assert_condition,
    const char *fmt,
    ...
);

#endif /* COMPILER_ERROR_H */
