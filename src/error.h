#ifndef COMPILER_ERROR_H
#define COMPILER_ERROR_H

#include <stddef.h>
#include <stdint.h>

// TODO: Add file path, byte offset, etc.
typedef struct Error {
  enum ErrorType {
    ERROR_NONE = 0,
    ERROR_ARGUMENTS,
    ERROR_TYPE,
    ERROR_GENERIC,
    ERROR_SYNTAX,
    ERROR_TODO,
    ERROR_MAX,
  } type;
  char *msg;
} Error;

void print_error(Error err);

extern Error ok;

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

#define ERROR_CREATE(n, t, msg)                 \
  Error (n) = { (t), (msg) }

#define ERROR_PREP(n, t, message)               \
  (n).type = (t);                               \
  (n).msg = (message);

#ifndef _MSC_VER
#  define NORETURN __attribute__((noreturn))
#  define FALLTHROUGH __attribute__((fallthrough))
#  define FORMAT(...) __attribute__((format(__VA_ARGS__)))
#  define FORCEINLINE __attribute__((always_inline)) inline
#  define PRETTY_FUNCTION __PRETTY_FUNCTION__
#else
#  define NORETURN __declspec(noreturn)
#  define FALLTHROUGH
#  define FORMAT(...)
#  define FORCEINLINE __forceinline inline
#  define PRETTY_FUNCTION __FUNCSIG__
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

NORETURN
FORMAT(printf, 1, 2)
void panic(const char *fmt, ...);

NORETURN
FORMAT(printf, 2, 3)
void panic_with_code(int code, const char *fmt, ...);

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

/// Usage:
///   ASSERT(condition);
///   ASSERT(condition, "message", ...);
#define ASSERT(cond, ...)  \
  do {                     \
    if (!(cond)) {         \
      assert_impl          \
        (__FILE__,         \
         PRETTY_FUNCTION,  \
         __LINE__ ,        \
         #cond,            \
         "!" __VA_ARGS__   \
         );                \
    }                      \
  } while (0)

#if __STDC_VERSION__ >= 201112L
#  define STATIC_ASSERT(cond, ...) _Static_assert(cond, "" __VA_ARGS__)
#else
#  define STATIC_ASSERT(cond, ...) ASSERT(cond, "" __VA_ARGS__)
#endif

#define TODO(...)     ASSERT(0, "TODO: "__VA_ARGS__)
#define PANIC(...)    ASSERT(0, "PANIC: "__VA_ARGS__)
#define UNREACHABLE() ASSERT(0, "Unreachable")

#define CAT_(a, b) a ## b
#define CAT(a, b) CAT_(a, b)

#define STR_(s) #s
#define STR(s) STR_(s)

#endif /* COMPILER_ERROR_H */
