#ifndef FUNCOMPILER_UTILS_H
#define FUNCOMPILER_UTILS_H

#include <stddef.h>
#include <stdint.h>
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
///  Miscellaneous macros.
/// ===========================================================================
#define CAT_(a, b) a ## b
#define CAT(a, b) CAT_(a, b)

#define STR_(s) #s
#define STR(s) STR_(s)

#define VA_FIRST(X, ...) X

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

/// Copy a string.
NODISCARD
string string_dup_impl(const char *src, usz size);
#define string_dup(src) string_dup_impl((src).data, (src).size)

/// Format a string.
NODISCARD
FORMAT(printf, 1, 2)
string format(const char *fmt, ...);

/// Create a string from a const char*
#define string_create(src) string_dup_impl(src, strlen(src))

/// Check if two strings are equal.
#define string_eq(a, b) ((a).size == (b).size && memcmp((a).data, (b).data, (a).size) == 0)

/// Convert a string to a span.
#define as_span(str) ((span){(str).data, (str).size})

/// Convert a string literal to a span.
#define literal_span(lit) ((span){(lit), sizeof(lit) - 1})

#endif // FUNCOMPILER_UTILS_H
