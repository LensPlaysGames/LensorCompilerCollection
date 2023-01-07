#include <error.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>
#include <platform.h>

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

/// This just calls vissue_diagnostic().
void issue_diagnostic(
    enum diagnostic_level level,
    const char *filename,
    span source,
    loc location,
    const char *fmt,
    ...) {
  va_list ap;
  va_start(ap, fmt);
  vissue_diagnostic(level, filename, source, location, fmt, ap);
  va_end(ap);
}

/// Issue a compiler diagnostic.
///
/// WARNING: ALTER THIS FUNCTION AT YOUR OWN PERIL.
///
/// This function is the culmination of many, *many* hours of battling
/// off-by-one errors and banging my head against the nearest wall. It
/// was copied from a previous project because I tried reproducing it
/// at first, but failed horribly. – Sirraide.
void vissue_diagnostic
(/// Error level and source location.
 enum diagnostic_level level,
 const char *filename,
 span source,
 loc location,

 /// The actual error message.
 const char *fmt,
 va_list ap) {
  ASSERT(level >= 0 && level < DIAG_COUNT);

  /// Check if stderr is a terminal.
  bool use_colour = thread_use_colours;

  /// Print a detailed error message if we have access to the source code.
  if (source.data && source.size) {
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
    if (use_colour) fprintf(stderr, "\033[m\033[1;38m");
    fprintf(stderr, "%s:%u:%u: ", filename, line, location.start - line_start);
    if (use_colour && colours_blink) fprintf(stderr, "\033[5m\a\a\a\a");
    if (use_colour) fprintf(stderr, "%s", diagnostic_level_colours[level]);
    fprintf(stderr, "%s: ", diagnostic_level_names[level]);
    if (use_colour) fprintf(stderr, "\033[m\033[1;38m");
    vfprintf(stderr, fmt, ap);
    if (use_colour) fprintf(stderr, "\033[m");

    /// Print the line.
    fprintf(stderr, "\n %d | ", line);
    for (u32 i = line_start; i < location.start; ++i) {
      if (source.data[i] == '\t') fprintf(stderr, "    ");
      else fputc(source.data[i], stderr);
    }
    if (use_colour) fprintf(stderr, "%s", diagnostic_level_colours[level]);
    for (u32 i = location.start; i < location.end; ++i) {
      if (source.data[i] == '\t') fprintf(stderr, "    ");
      else fputc(source.data[i], stderr);
    }
    if (use_colour) fprintf(stderr, "\033[m");
    for (u32 i = location.end; i < line_end; ++i) {
      if (source.data[i] == '\t') fprintf(stderr, "    ");
      else fputc(source.data[i], stderr);
    }
    fprintf(stderr, "\n");

    /// Underline the region with tildes.
    size_t spaces = !line ? 1 : (u32) (log10(line) + 1);
    for (size_t i = 0; i < spaces; i++)
      fprintf(stderr, " ");
    fprintf(stderr, "  | ");
    if (use_colour && colours_blink) fprintf(stderr, "\033[5m");
    if (use_colour) fprintf(stderr, "%s", diagnostic_level_colours[level]);
    for (u32 i = line_start; i < location.start; ++i) {
      if (source.data[i] == '\t') fprintf(stderr, "    ");
      else fputc(' ', stderr);
    }
    for (u32 i = location.start; i < location.end; ++i) {
      if (source.data[i] == '\t') fprintf(stderr, "~~~~");
      else fputc('~', stderr);
    }
    if (use_colour) fprintf(stderr, "\033[m");
    fprintf(stderr, "\n");
  }

  /// Otherwise, just print a simple error message.
  else {
    if (use_colour) fprintf(stderr, "\033[m\033[1;38m");
    fprintf(stderr, "%s: ", filename);
    if (use_colour && colours_blink) fprintf(stderr, "\033[5m");
    if (use_colour) fprintf(stderr, "%s", diagnostic_level_colours[level]);
    fprintf(stderr, "%s: ", diagnostic_level_names[level]);
    if (use_colour) fprintf(stderr, "\033[m\033[1;38m");
    vfprintf(stderr, fmt, ap);
    if (use_colour) fprintf(stderr, "\033[m");
    fprintf(stderr, "\n");
  }
 }

void raise_fatal_error_impl (
    const char *file,
    const char *func,
    int line,
    bool is_signal_or_exception,
    bool is_sorry,
    const char *assert_condition,
    const char *fmt,
    ...
) {
  /// Print the file and line.
  bool use_colour = thread_use_colours;

  /// Removing everything up to and including the `src` prefix.
  const char *filename = file, *src_prefix;
  while (src_prefix = strstr(filename, "src" PLATFORM_PATH_SEPARATOR), src_prefix) filename = src_prefix + 4;

  /// To make this less atrocious,
  const char *const reset = use_colour ? "\033[m" : "";
  const char *const w = use_colour ? "\033[m\033[1;38m" : "";
  const char *const colour = use_colour && colours_blink
    ? "\033[31;5m\a\a\a\a\a"
    : use_colour
        ? diagnostic_level_colours[is_sorry ? DIAG_SORRY : DIAG_ICE]
        : "";

  /// Print a shorter message if this is a TODO() w/ no message.
  if (is_sorry && strcmp(fmt, "") == 0) {
    fprintf(stderr, "%s%s:%d:%s %s", w, filename, line, reset, colour);
    fprintf(stderr, "Sorry, unimplemented:%s Function ‘%s%s%s’\n", reset, w, func, reset);
  }

  /// Print a longer message.
  else {
    /// File, line, message header; if they make sense, that is.
    if (!is_signal_or_exception) {
      fprintf(stderr, "%s%s:%s In function ‘%s%s%s’\n", w, filename, reset, w, func, reset);
      fprintf(stderr, "%s%s:%d:%s %s", w, filename, line, reset, colour);
    } else {
      fprintf(stderr, "%s", colour);
    }

    /// Assert condition (if any) and message.
    if (assert_condition) {
      fprintf(stderr, "Assertion failed:%s ‘%s%s%s’: ", reset, w, assert_condition, reset);
    } else {
      fprintf(stderr, "%s:%s ", diagnostic_level_names[is_sorry ? DIAG_SORRY : DIAG_ICE], reset);
    }

    /// Message.
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    fprintf(stderr, "\n");
  }

  /// Print the stack trace.
#ifndef _WIN32
  platform_print_backtrace(is_signal_or_exception ? 4 : 2);
#else
  platform_print_backtrace(is_signal_or_exception ? 9 : 2);
#endif

  /// Exit.
  _Exit(1);
}
