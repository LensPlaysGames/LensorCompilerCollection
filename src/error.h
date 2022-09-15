#ifndef COMPILER_ERROR_H
#define COMPILER_ERROR_H

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

#define ERROR_CREATE(n, t, msg)                 \
  Error (n) = { (t), (msg) }

#define ERROR_PREP(n, t, message)               \
  (n).type = (t);                               \
  (n).msg = (message);

#ifndef _MSC_VER
#  define NORETURN __attribute__((noreturn))
#  define FORMAT(...) __attribute__((format(__VA_ARGS__)))
#  define PRETTY_FUNCTION __PRETTY_FUNCTION__
#else
#  define NORETURN
#  define FORMAT(...)
#  define PRETTY_FUNCTION __FUNCSIG__
#endif

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
         "" __VA_ARGS__    \
         );                \
    }                      \
  } while (0)

#endif /* COMPILER_ERROR_H */