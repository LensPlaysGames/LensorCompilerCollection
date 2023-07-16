#ifndef FUNCOMPILER_UTILS_H
#define FUNCOMPILER_UTILS_H

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;
typedef size_t usz;
typedef ptrdiff_t isz;

/// ===========================================================================
///  Attributes and platform-specific macros.
/// ===========================================================================
#define NORETURN __attribute__((noreturn))
#define FALLTHROUGH __attribute__((fallthrough))
#define FORMAT(...) __attribute__((format(__VA_ARGS__)))
#define PRAGMA_STR(_Str) _Pragma(#_Str)
#define PUSH_IGNORE_WARNING(W)      \
    _Pragma("GCC diagnostic push")    \
    PRAGMA_STR(GCC diagnostic ignored W)
#define POP_WARNINGS() _Pragma("GCC diagnostic pop")
#ifndef FORCEINLINE
#  define FORCEINLINE __attribute__((always_inline)) inline
#endif
#define PRETTY_FUNCTION __PRETTY_FUNCTION__
#define NODISCARD __attribute__((warn_unused_result))
#define BUILTIN_UNREACHABLE() __builtin_unreachable()
#define THREAD_LOCAL __thread

/// PACKED **must** go inbetween the struct keyword and the struct
/// name, or else it won't work.
#define PACKED __attribute__((packed))
#define PACKED_DEFAULT

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

/// return_type == return type of function that is pointed to
/// name == name of function pointer type
/// ... == arg types (pass `void` if none)
#define FUNCTION_POINTER(return_type, name, ...) \
  typedef return_type (*name)(__VA_ARGS__)

/// ===========================================================================
///  Global settings.
/// ===========================================================================
/// This is a command-line setting.
extern bool prefer_using_diagnostics_colours;

/// Don’t question this.
extern bool colours_blink;

/// Whether to keep colours when formatting.
extern THREAD_LOCAL bool thread_use_colours;
extern THREAD_LOCAL bool thread_disable_type_colours;

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
/// This is techincally a Vector(char), but circular dependencies go brrr.
typedef struct string_buffer {
  char *data;
  usz size;
  usz capacity;
} string_buffer;

/// Copy a string.
NODISCARD
string string_dup_impl(const char *src, usz size);
#define string_dup(src) string_dup_impl((src).data, (src).size)

/// Zero-terminate a string buffer. This is harder than it sounds.
void string_buf_zterm(string_buffer *buf);

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

/// Check if string A starts with string B.
#define string_starts_with(a, b) ((a).size >= (b).size && memcmp((a).data, (b).data, (b).size) == 0)

/// Check if two strings are equal.
#define string_eq(a, b) ((a).size == (b).size && memcmp((a).data, (b).data, (a).size) == 0)

/// Convert a string to a span.
#define as_span(str) ((span){(str).data, (str).size})

/// Convert a string literal to a span.
#define literal_span_raw(lit) {(lit), sizeof(lit) - 1}
#define literal_span(lit) ((span)literal_span_raw((lit)))

/// Create a string from a const char*
static inline string_buffer as_string_buffer(string s) {
  return (string_buffer){
    .data = s.data,
    .size = s.size,
    .capacity = s.size
  };
}

/// ===========================================================================
///  Other.
/// ===========================================================================
/// Determine the width of a number.
NODISCARD usz number_width(u64 n);

#define ALIGN_TO(value, alignment) ({                                                 \
  isz _value = (isz) value;                                                           \
  isz _alignment = (isz) alignment;                                                   \
  (__typeof__(value)) (_value + ((_alignment - (_value % _alignment)) % _alignment)); \
})

/// Find *last* occurence of entire string TOKEN within INPUT string.
char *strrstr(char *input, const char *token);

#endif // FUNCOMPILER_UTILS_H
