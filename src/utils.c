#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <utils.h>
#include <error.h>
#include <math.h>
#include <inttypes.h>
#include <ast.h>
#include <vector.h>

#ifdef _WIN32
/// Basically strndup(), but for Windows.
static char *strndup(const char *str, usz sz) {
  char *dup = malloc(sz + 1);
  memcpy(dup, str, sz);
  dup[sz] = 0;
  return dup;
}
#endif

THREAD_LOCAL bool thread_use_colours = false;

/// Copy a string to the heap.
string string_dup_impl(const char *src, usz size) {
  string dest;
  dest.data = strndup(src, size);
  dest.size = size;
  return dest;
}

/// ===========================================================================
///  String formatting.
/// ===========================================================================
/// This function handles the actual formatting.
///
/// If colours are disabled, this function will strip all terminal escape codes.
///
/// Supported format specifiers:
///   - %c: char
///   - %s: const char *, char *
///   - %S: span, string
///   - %C: A colour string. Only printed if colours are enabled.
///   - %d: int
///   - %i: int
///   - %u: unsigned
///   - %D: int64_t
///   - %I: int64_t
///   - %U: uint64_t
///   - %Z: size_t
///   - %zu: size_t
///   - %x: hexadecimal (32-bit)
///   - %X: hexadecimal (64-bit)
///   - %p: void *
///   - %b: bool
///   - %T: Type *
///   - %F: Another format string + va_list. This format spec therefore takes *two* arguments.
///   - %%: A '%' character.
///   - %m: Reset colours if colours are enabled.
///   - %X, where X is ESCAPE: A literal escape character if you need that for some ungodly reason.
///   - %XX, where X is in [0-9]: "\033[XXm" if colours are enabled, and "" otherwise.
///   - %BXX, where X is in [0-9]: "\033[1;XXm" if colours are enabled, and "" otherwise.
static inline void vformat_to_impl(
  const char* fmt,
  va_list args,
  void write_string(const char* str, usz size, void *to),
  void *to
) {
  /// Sanity check.
  ASSERT(fmt, "Format string may not be null");

  /// Main formatting loop.
  for (;;) {
    /// Find the start of the next format specifier or ANSI escape code.
    const char *start = fmt;
    fmt += strcspn(fmt, "%\033");

    /// No more format specifiers or escape codes.
    if (*fmt == 0) {
      write_string(start, strlen(start), to);
      return;
    }

    /// Write the string up to the format specifier.
    write_string(start, (usz) (fmt - start), to);

    /// Handle ANSI escape codes.
    if (*fmt == '\033') {
      /// Find the end of the escape code.
      const char *end = fmt + strcspn(fmt, "m") + 1;

      /// Handle unterminated escape codes.
      if (*end == 0) ICE("Unterminated ANSI escape code in format string");

      /// Write the escape code.
      if (thread_use_colours) write_string(fmt, (usz) (end - fmt), to);

      /// Move on to the next format specifier.
      fmt = end;
      continue;
    }

    /// Yeet '%'.
    fmt++;

    /// Formatting state.
    bool bold = false;

    /// Handle the format specifier.
    switch (*fmt++) {
      case 'c': {
        /// Variadic arguments smaller than char are promoted to int.
        char c = (char) va_arg(args, int);
        write_string(&c, 1, to);
      } break;

      case 's': {
        const char *str = va_arg(args, const char *);
        write_string(str, strlen(str), to);
      } break;

      case 'S': {
        span str = va_arg(args, span);
        write_string(str.data, str.size, to);
      } break;

      case 'C': {
        const char *str = va_arg(args, const char *);
        if (thread_use_colours) write_string(str, strlen(str), to);
      } break;

      case 'd':
      case 'i': {
        int n = va_arg(args, int);
        char buf[32];
        sprintf(buf, "%d", n);
        write_string(buf, strlen(buf), to);
      } break;

      case 'u': {
        unsigned n = va_arg(args, unsigned);
        char buf[32];
        sprintf(buf, "%u", n);
        write_string(buf, strlen(buf), to);
      } break;

      case 'D':
      case 'I': {
        int64_t n = va_arg(args, int64_t);
        char buf[32];
        sprintf(buf, "%" PRId64, n);
        write_string(buf, strlen(buf), to);
      } break;

      case 'U': {
        uint64_t n = va_arg(args, uint64_t);
        char buf[32];
        sprintf(buf, "%" PRIu64, n);
        write_string(buf, strlen(buf), to);
      } break;

      case 'z': {
        /// Backwards compatibility with '%zu' because we were using that all over the place.
        if (*fmt != 'u') ICE("Invalid format specifier: '%%z%c'", *fmt);
        fmt++;
      } FALLTHROUGH;

      case 'Z': {
        size_t n = va_arg(args, size_t);
        char buf[32];
        sprintf(buf, "%zu", n);
        write_string(buf, strlen(buf), to);
      } break;

      case 'x': {
        unsigned n = va_arg(args, unsigned);
        char buf[32];
        sprintf(buf, "%x", n);
        write_string(buf, strlen(buf), to);
      } break;

      case 'X': {
        uint64_t n = va_arg(args, uint64_t);
        char buf[32];
        sprintf(buf, "%" PRIx64, n);
        write_string(buf, strlen(buf), to);
      } break;

      case 'p': {
        void *ptr = va_arg(args, void *);
        char buf[32];
        sprintf(buf, "%p", ptr);
        write_string(buf, strlen(buf), to);
      } break;

      case 'b': {
        bool b = va_arg(args, int);
        write_string(b ? "true" : "false", b ? 4 : 5, to);
      } break;

      case 'T': {
        string s = ast_typename(va_arg(args, Type *), thread_use_colours);
        write_string(s.data, s.size, to);
        free(s.data);
      } break;

      case 'F': {
        const char *fmt2 = va_arg(args, const char *);
        vformat_to_impl(fmt2, va_arg(args, va_list), write_string, to);
      } break;

      case '%': {
        write_string("%", 1, to);
      } break;

      case '\033': {
        write_string("\033", 1, to);
      } break;

      case 'm': {
        if (thread_use_colours) write_string("\033[m", 3, to);
      } break;

      case 'B': {
        bold = true;

        /// Next character must be a digit.
        if (*fmt < '0' || *fmt > '9') ICE("Invalid format specifier: '%%B%c'", *fmt);
        fmt++;
      } FALLTHROUGH;

      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9': {
        /// Colour codes are always 2 digits.
        if (*fmt < '0' || *fmt > '9')
          ICE("Invalid format specifier: '%%%s%c%c'", bold ? "B" : "", fmt[-1], *fmt);

        /// Ignore this if colours are disabled.
        fmt++;
        if (!thread_use_colours) break;

        /// Reset the colour.
        write_string("\033[m", 3, to);

        /// Write the colour code.
        write_string("\033[", 2, to);
        if (bold) write_string("1;", 2, to);
        write_string(fmt - 2, 2, to);
        write_string("m", 1, to);
      } break;

      default: ICE("Invalid format specifier: '%%%c'", fmt[-1]);
    }
  }
}

/// Callback to write to a string.
FORCEINLINE static void write_string_to_string(const char *str, usz size, void *to) {
  string_buffer *buf = to;

  /// Synthesise a buffer from the string we need to print.
  string_buffer v = {
    .data = (char *) str,
    .size = size,
    .capacity = size,
  };

  /// Append the buffer to the string.
  vector_append_all(*buf, v);
}

/// Callback to write to a file.
FORCEINLINE static void write_string_to_file(const char *str, usz size, void *to) {
  FILE *file = to;
  fwrite(str, 1, size, file);
}

/// Format a string to a buffer.
void vformat_to(string_buffer *buf, const char *fmt, va_list args) {
  vformat_to_impl(fmt, args, write_string_to_string, buf);
}

/// Format a string to a buffer.
void format_to(string_buffer *buf, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vformat_to(buf, fmt, ap);
  va_end(ap);
}

/// Format a string.
string vformat(const char *fmt, va_list args) {
  string_buffer buf = {};
  vformat_to(&buf, fmt, args);
  return (string) { .data = buf.data, .size = buf.size };
}

/// Format a string.
string format(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  string result = vformat(fmt, ap);
  va_end(ap);
  return result;
}

/// Print a string to a file.
void vfprint(FILE *file, const char *fmt, va_list args) {
  return vformat_to_impl(fmt, args, write_string_to_file, file);
}

/// Print a string to a file.
void fprint(FILE *f, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprint(f, fmt, ap);
  va_end(ap);
}

/// Print a string to stdout.
void print(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprint(stdout, fmt, ap);
  va_end(ap);
}

/// Print a string to stderr.
void eprint(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprint(stderr, fmt, ap);
  va_end(ap);
}

usz number_width(u64 n) { return n ? (usz) log10((double)n) + 1 : 1; }
