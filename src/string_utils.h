#ifndef FUNCOMPILER_STRING_UTILS_H
#define FUNCOMPILER_STRING_UTILS_H

#include <stddef.h>
#include <stdint.h>
#include <string.h>

typedef uint32_t u32;
typedef uint64_t u64;
typedef int32_t i32;
typedef int64_t i64;
typedef size_t usz;
typedef ptrdiff_t isz;

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
string string_dup_impl(const char *src, usz size);
#define string_dup(src) string_dup_impl((src).data, (src).size)

/// Create a string from a const char*
#define string_create(src) string_dup_impl(src, strlen(src))

/// Check if two strings are equal.
#define string_eq(a, b) ((a).size == (b).size && memcmp((a).data, (b).data, (a).size) == 0)

/// Convert a string to a span.
#define as_span(str) ((span){(str).data, (str).size})

/// Convert a string literal to a span.
#define literal_span(lit) ((span){(lit), sizeof(lit) - 1})

#endif // FUNCOMPILER_STRING_UTILS_H
