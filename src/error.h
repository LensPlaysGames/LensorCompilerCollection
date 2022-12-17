#ifndef COMPILER_ERROR_H
#define COMPILER_ERROR_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

typedef uint32_t u32;
typedef uint64_t u64;
typedef int32_t i32;
typedef int64_t i64;
typedef size_t usz;

/// String span.
typedef struct {
  const char *data;
  usz size;
} span;

/// Owning string.
typedef struct {
  char *data;
  usz size;
} string;

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

#ifndef _MSC_VER
#  define NORETURN __attribute__((noreturn))
#  define FALLTHROUGH __attribute__((fallthrough))
#  define FORMAT(...) __attribute__((format(__VA_ARGS__)))
#  define FORCEINLINE __attribute__((always_inline)) inline
#  define PRETTY_FUNCTION __PRETTY_FUNCTION__
#  define NODISCARD __attribute__((warn_unused_result))
#  define builtin_unreachable() __builtin_unreachable()
#else
#  define NORETURN __declspec(noreturn)
#  define FALLTHROUGH
#  define FORMAT(...)
#  define FORCEINLINE __forceinline inline
#  define PRETTY_FUNCTION __FUNCSIG__
#  define NODISCARD
#  define builtin_unreachable() __assume(0)
#endif

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

/// Used by ASSERT(). You probably don't want to use this directly.
NORETURN
FORMAT(printf, 5, 6)
void assert_impl (
    const char *file,
    const char *func,
    int line,
    const char *condition,
    const char *fmt,
    ...
);

/// Used by ICE(). You probably don't want to use this directly.
NORETURN
FORMAT(printf, 2, 3)
void ice_impl (
    int ignore_frames_n,
    const char *fmt,
    ...
);

#define ICE(...)        (ice_impl(2, __VA_ARGS__), builtin_unreachable())

/// Call this if you’re in a signal handler.
#ifndef _WIN32
#  define ICE_SIGNAL(...) (ice_impl(4, __VA_ARGS__), builtin_unreachable())
#endif

/// Usage:
///   ASSERT(condition);
///   ASSERT(condition, "format string", ...);
///
/// We use `||` instead of `if` here so that this is an expression.
#define ASSERT(cond, ...) \
  ((cond) ? (void)(0) : (assert_impl(__FILE__, PRETTY_FUNCTION, __LINE__, #cond, "" __VA_ARGS__), builtin_unreachable()))

#if __STDC_VERSION__ >= 201112L
#  define STATIC_ASSERT(...) _Static_assert(__VA_ARGS__)
#else
#  define STATIC_ASSERT(...) ASSERT(__VA_ARGS__)
#endif

#define TODO(...)     (ASSERT(false, "TODO: "__VA_ARGS__), builtin_unreachable())
#define PANIC(...)    (ASSERT(false, "PANIC: "__VA_ARGS__), builtin_unreachable())
#define UNREACHABLE() (ASSERT(false, "Unreachable"), builtin_unreachable())

#define CAT_(a, b) a ## b
#define CAT(a, b) CAT_(a, b)

#define STR_(s) #s
#define STR(s) STR_(s)

#endif /* COMPILER_ERROR_H */
