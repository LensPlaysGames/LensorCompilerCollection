#ifndef FUNCOMPILER_UTILS_H
#define FUNCOMPILER_UTILS_H

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

typedef uint32_t u32;
typedef uint64_t u64;
typedef int32_t i32;
typedef int64_t i64;
typedef size_t usz;
typedef ptrdiff_t isz;

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
#  define THREAD_LOCAL __thread
#else
#  define NORETURN __declspec(noreturn)
#  define FALLTHROUGH
#  define FORMAT(...)
#  define FORCEINLINE __forceinline inline
#  define PRETTY_FUNCTION __FUNCSIG__
#  define NODISCARD
#  define BUILTIN_UNREACHABLE() __assume(0)
#  define THREAD_LOCAL __declspec(thread)
#endif

#ifdef __EXT_FORMAT__
#  define EXT_FORMAT(fmt, arg) __attribute__((ext_format(fmt, arg)))
#else
#  define EXT_FORMAT(fmt, arg)
#endif

/// ===========================================================================
///  Miscellaneous macros.
/// ===========================================================================
#define CAT_(a, b) a ## b
#define CAT(a, b) CAT_(a, b)

#define STR_(s) #s
#define STR(s) STR_(s)

#define VA_FIRST(X, ...) X

/// ===========================================================================
///  Global settings.
/// ===========================================================================
/// This is a command-line setting.
extern bool prefer_using_diagnostics_colours;

/// Don’t question this.
extern bool colours_blink;

/// Whether to keep colours when formatting.
extern THREAD_LOCAL bool thread_use_colours;

/// Enable colours in diagnostics.
static inline void enable_colours() { thread_use_colours = true; }

/// Disable colours in diagnostics.
static inline void disable_colours() { thread_use_colours = false; }

/// ===========================================================================
///  Strings.
/// ===========================================================================

/// String span.
typedef struct span {
  const char *data;
  usz size;
} span;

/// Owning string.
typedef struct string {
  char *data;
  usz size;
} string;

/// Resizeable string buffer
typedef struct string_buffer {
  char *data;
  usz size;
  usz capacity;
} string_buffer;

/// Copy a string.
NODISCARD
string string_dup_impl(const char *src, usz size);
#define string_dup(src) string_dup_impl((src).data, (src).size)

/// Format a string.
NODISCARD string vformat(const char *fmt, va_list args);

/// Format a string.
EXT_FORMAT(1, 2)
NODISCARD string format(const char *fmt, ...);

/// Format a string to a buffer.
void vformat_to(string_buffer *buf, const char *fmt, va_list args);

/// Format a string to a buffer.
EXT_FORMAT(2, 3)
void format_to(string_buffer *buf, const char *fmt, ...);

/// Print a string to a file.
void vfprint(FILE *file, const char *fmt, va_list args);

/// Print a string to a file.
EXT_FORMAT(2, 3)
void fprint(FILE *f, const char *fmt, ...);

/// Print a string to stdout.
EXT_FORMAT(1, 2)
void print(const char *fmt, ...);

/// Print a string to stderr.
EXT_FORMAT(1, 2)
void eprint(const char *fmt, ...);

/// Create a string from a const char*
#define string_create(src) string_dup_impl(src, strlen(src))

/// Check if two strings are equal.
#define string_eq(a, b) ((a).size == (b).size && memcmp((a).data, (b).data, (a).size) == 0)

/// Convert a string to a span.
#define as_span(str) ((span){(str).data, (str).size})

/// Convert a string literal to a span.
#define literal_span_raw(lit) {(lit), sizeof(lit) - 1}
#define literal_span(lit) ((span)literal_span_raw((lit)))

/// ===========================================================================
///  Other.
/// ===========================================================================
/// Determine the width of a number.
NODISCARD usz number_width(u64 n);

#endif // FUNCOMPILER_UTILS_H
