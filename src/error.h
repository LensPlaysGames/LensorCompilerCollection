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
#  define LIKELY(x) __builtin_expect((x), 1)
#  define UNLIKELY(x) __builtin_expect((x), 0)
#  define PATH_SEPARATOR "/"
#else
#  define NORETURN
#  define FORMAT(...)
#  define PRETTY_FUNCTION __FUNCSIG__
#  define LIKELY(x) (x)
#  define UNLIKELY(x) (x)
#  define PATH_SEPARATOR "\\"
#endif

#define VA_FIRST(x, ...) x
#define VA_REST(x, ...) __VA_ARGS__
#define VA_REST_COMMA(x, ...) __VA_OPT__(,) __VA_ARGS__

NORETURN
FORMAT(printf, 1, 2)
void panic(const char *fmt, ...);

NORETURN
FORMAT(printf, 2, 3)
void panic_with_code(int code, const char *fmt, ...);

/// Used by ASSERT(). You probably don't want to use this directly.
NORETURN
FORMAT(printf, 5, 6)
void assert_impl
(const char *file,
 const char *func,
 int line,
 const char *condition,
 const char *fmt,
 ...
 );

/// Usage:
///   ASSERT(condition);
///   ASSERT(condition, "message", ...);
#define ASSERT(cond, ...)                             \
  do {                                                \
    if (UNLIKELY(!(cond))) {                          \
      assert_impl                                     \
        (__FILE__,                                    \
         PRETTY_FUNCTION,                             \
         __LINE__ ,                                   \
         #cond,                                       \
         /* First element of __VA_ARGS__ or NULL */   \
         VA_FIRST(__VA_ARGS__ __VA_OPT__(,) NULL)     \
         /* Comma + rest of __VA_ARGS__ */            \
         VA_REST_COMMA(__VA_ARGS__)                   \
         );                                           \
    }                                                 \
  } while (0)

#endif /* COMPILER_ERROR_H */
