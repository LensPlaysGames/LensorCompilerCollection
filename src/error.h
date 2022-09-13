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
#define FUNC_NORETURN __attribute__((noreturn))
#define FUNC_FORMAT(...) __attribute__((format(__VA_ARGS__)))
#define FUNC_PRETTY_FUNCTION __PRETTY_FUNCTION__
#define FUNC_LIKELY(x) __builtin_expect((x), 1)
#define FUNC_UNLIKELY(x) __builtin_expect((x), 0)
#define FUNC_PATH_SEPARATOR "/"
#else
#define FUNC_NORETURN
#define FUNC_FORMAT(...)
#define FUNC_PRETTY_FUNCTION __FUNCSIG__
#define FUNC_LIKELY(x) (x)
#define FUNC_UNLIKELY(x) (x)
#define FUNC_PATH_SEPARATOR "\\"
#endif

#define FUNC_VA_FIRST(x, ...) x
#define FUNC_VA_REST(x, ...) __VA_ARGS__
#define FUNC_VA_REST_COMMA(x, ...) __VA_OPT__(,) __VA_ARGS__

FUNC_NORETURN
FUNC_FORMAT(printf, 1, 2)
void panic(const char *fmt, ...);

FUNC_NORETURN
FUNC_FORMAT(printf, 2, 3)
void panic_with_code(int code, const char *fmt, ...);

/// Used by FUNC_ASSERT(). You probably don't want to use this directly.
FUNC_NORETURN
FUNC_FORMAT(printf, 5, 6)
void func_assert_impl(
    const char *file,
    const char *func,
    int line,
    const char *condition,
    const char *fmt,
    ...
);

/// Usage:
///   FUNC_ASSERT(condition);
///   FUNC_ASSERT(condition, "message", ...);
#define FUNC_ASSERT(cond, ...)                        \
  do {                                                \
    if (FUNC_UNLIKELY(!(cond))) {                     \
      func_assert_impl(                               \
        __FILE__,                                     \
        FUNC_PRETTY_FUNCTION,                         \
        __LINE__ ,                                    \
        #cond,                                        \
        /* First element of __VA_ARGS__ or NULL */    \
        FUNC_VA_FIRST(__VA_ARGS__ __VA_OPT__(,) NULL) \
        /* Comma + rest of __VA_ARGS__ */             \
        FUNC_VA_REST_COMMA(__VA_ARGS__)               \
      );                                              \
    }                                                 \
  } while (0)

#endif /* COMPILER_ERROR_H */
