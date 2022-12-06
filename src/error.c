#include <error.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>

/// For isatty().
#ifdef _WIN32
#    include <io.h>
#    define isatty _isatty
#else
#    include <unistd.h>
#endif

Error ok = { ERROR_NONE, NULL };

void print_error(Error err) {
  if (err.type == ERROR_NONE) {
    return;
  }
  printf("ERROR: ");
  ASSERT(ERROR_MAX == 6);
  switch (err.type) {
  default:
    printf("Unkown error type...");
    break;
  case ERROR_TODO:
    printf("TODO (not implemented)");
    break;
  case ERROR_SYNTAX:
    printf("Invalid syntax");
    break;
  case ERROR_TYPE:
    printf("Mismatched types");
    break;
  case ERROR_ARGUMENTS:
    printf("Invalid arguments");
    break;
  case ERROR_GENERIC:
    break;
  case ERROR_NONE:
    break;
  }
  putchar('\n');
  if (err.msg) {
    printf("     : %s\n", err.msg);
  }
}

static const char *diagnostic_level_names[DIAG_COUNT] = {
    "Note",
    "Warning",
    "Error",
    "Internal Compiler Error",
    "Sorry, unimplemented",
};

static const char *diagnostic_level_colours[DIAG_COUNT] = {
    "\033[1;34m",
    "\033[1;36m",
    "\033[1;31m",
    "\033[1;31m",
    "\033[1;31m",
};

/// Issue a compiler diagnostic.
///
/// WARNING: ALTER THIS FUNCTION AT YOUR OWN PERIL.
///
/// This function is the culmination of many, *many* hours of battling
/// off-by-one errors and banging my head against the nearest wall. It
/// was copied from a previous project because I tried reproducing it
/// at first, but failed horribly. – Sirraide.
void issue_diagnostic(
    enum diagnostic_level level,
    const char *filename,
    span source,
    loc location,
    const char *fmt,
    ...) {
  ASSERT(level >= 0 && level < DIAG_COUNT);

  /// Check if stderr is a terminal.
  bool is_terminal = isatty(fileno(stderr));

  if (location.start > source.size) location.start = (u32) (source.size);
  if (location.end > source.size) location.end = (u32) (source.size);

  /// Seek to the start of the line. Keep track of the line number.
  u32 line = 1;
  u32 line_start = 0;
  for (u32 i = location.start; i > 0; --i) {
    if (source.data[i] == '\n') {
    if (!line_start) line_start = i + 1;
    ++line;
    }
  }

  /// Don’t include the newline in the line.
  if (source.data[line_start] == '\n') ++line_start;

  /// Seek to the end of the line.
  u32 line_end = location.end;
  while (line_end < source.size && source.data[line_end] != '\n') line_end++;

  /// Print the filename, line and column, severity and message.
  if (is_terminal) fprintf(stderr, "\033[m\033[1;38m");
  fprintf(stderr, "%s:%u:%u: ", filename, line, location.start - line_start);
  if (is_terminal) fprintf(stderr, "%s", diagnostic_level_colours[level]);
  fprintf(stderr, "%s: ", diagnostic_level_names[level]);
  if (is_terminal) fprintf(stderr, "\033[m\033[1;38m");
  va_list args;
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);
  if (is_terminal) fprintf(stderr, "\033[m");

  /// Print the line.
  fprintf(stderr, "\n %d | ", line);
  for (u32 i = line_start; i < location.start; ++i) {
    if (source.data[i] == '\t') fprintf(stderr, "    ");
    else fputc(source.data[i], stderr);
  }
  if (is_terminal) fprintf(stderr, "%s", diagnostic_level_colours[level]);
  for (u32 i = location.start; i < location.end; ++i) {
    if (source.data[i] == '\t') fprintf(stderr, "    ");
    else fputc(source.data[i], stderr);
  }
  if (is_terminal) fprintf(stderr, "\033[m");
  for (u32 i = location.end; i < line_end; ++i) {
    if (source.data[i] == '\t') fprintf(stderr, "    ");
    else fputc(source.data[i], stderr);
  }
  fprintf(stderr, "\n");

  /// Underline the region with tildes.
  size_t spaces = !line ? 1 : (u32) (log10(line) + 1);
  for (size_t i = 0; i < spaces; i++)
    fprintf(stderr, " ");
  fprintf(stderr, "  | %s", is_terminal ? diagnostic_level_colours[level] : "");
  for (u32 i = line_start; i < location.start; ++i) {
    if (source.data[i] == '\t') fprintf(stderr, "    ");
    else fputc(' ', stderr);
  }
  for (u32 i = location.start; i < location.end; ++i) {
    if (source.data[i] == '\t') fprintf(stderr, "~~~~");
    else fputc('~', stderr);
  }
  if (is_terminal) fprintf(stderr, "\033[m\n");
}

NORETURN
static void vpanic(int code, const char *fmt, va_list args) {
  fprintf(stderr, "Panic: ");
  vfprintf(stderr, fmt, args);
  fputc('\n', stderr);
  _Exit(code);
}

void panic(const char *fmt, ...) {
  va_list va;
  va_start(va, fmt);
  vpanic(1, fmt, va);
  UNREACHABLE();
}

void panic_with_code(int code, const char *fmt, ...) {
  va_list va;
  va_start(va, fmt);
  vpanic(code, fmt, va);
  UNREACHABLE();
}

void assert_impl (
    const char *file,
    const char *func,
    int line,
    const char *condition,
    const char *fmt,
    ...
) {
/// Prettier file name
#ifndef _MSC_VER
  const char path_separator = '/';
#else
  const char path_separator = '\\';
#endif
  const char *basename = strrchr(file, path_separator);
  file = basename ? basename + 1 : file;

  fprintf(stderr, "Assertion failed: %s\n", condition);
  fprintf(stderr, "    In file %s:%d\n", file, line);
  fprintf(stderr, "    In function %s", func);

  if (strcmp(fmt, "!") != 0) {
    fprintf(stderr, "\n    Message: ");

    // Skip '!' placeholder for non-empty format string.
    fmt += 1;

    va_list va;
    va_start(va, fmt);
    vfprintf(stderr, fmt, va);
    va_end(va);
  }

  fputc('\n', stderr);
  _Exit(1);
}
