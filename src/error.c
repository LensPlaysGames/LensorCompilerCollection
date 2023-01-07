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
    eprint("%B38%s:%u:%u: ", filename, line, location.start - line_start);
    if (colours_blink) eprint("\033[5m\a\a\a\a");
    eprint("%C%s: %B38%F%m", diagnostic_level_colours[level], diagnostic_level_names[level], fmt, ap);

    /// Print the line.
    eprint("\n %u | ", line);
    for (u32 i = line_start; i < location.start; ++i) {
      if (source.data[i] == '\t') eprint("    ");
      else fputc(source.data[i], stderr);
    }
    eprint("%C", diagnostic_level_colours[level]);
    for (u32 i = location.start; i < location.end; ++i) {
      if (source.data[i] == '\t') eprint("    ");
      else fputc(source.data[i], stderr);
    }
    eprint("%m");
    for (u32 i = location.end; i < line_end; ++i) {
      if (source.data[i] == '\t') eprint("    ");
      else fputc(source.data[i], stderr);
    }
    eprint("\n");

    /// Underline the region with tildes.
    size_t spaces = !line ? 1 : (u32) (log10(line) + 1);
    for (size_t i = 0; i < spaces; i++) eprint(" ");
    eprint("  | ");
    if (colours_blink) eprint("\033[5m");
    eprint("%C", diagnostic_level_colours[level]);
    for (u32 i = line_start; i < location.start; ++i) {
      if (source.data[i] == '\t') eprint("    ");
      else fputc(' ', stderr);
    }
    for (u32 i = location.start; i < location.end; ++i) {
      if (source.data[i] == '\t') eprint("~~~~");
      else fputc('~', stderr);
    }
    eprint("%m\n");
  }

  /// Otherwise, just print a simple error message.
  else {
    eprint("%B38%s: %C%C%s: %B38%F%m\n",
      filename,
      colours_blink ? "\033[5m" : "", diagnostic_level_colours[level], diagnostic_level_names[level],
      fmt, ap
    );
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
  /// Removing everything up to and including the `src` prefix.
  const char *filename = file, *src_prefix;
  while (src_prefix = strstr(filename, "src" PLATFORM_PATH_SEPARATOR), src_prefix) filename = src_prefix + 4;
  const char *const colour = colours_blink
    ? "\033[31;5m\a\a\a\a\a"
    : diagnostic_level_colours[is_sorry ? DIAG_SORRY : DIAG_ICE];

  /// Print a shorter message if this is a TODO() w/ no message.
  if (is_sorry && strcmp(fmt, "") == 0) {
    eprint("%B38%s:%d: %C", filename, line, colour);
    eprint("Sorry, unimplemented:%m Function ‘%B38%s%m’\n", func);
  }

  /// Print a longer message.
  else {
    /// File, line, message header; if they make sense, that is.
    if (!is_signal_or_exception) {
      eprint("%B38%s:%m In function ‘%B38%s%m’\n", filename, func);
      eprint("%B38%s:%d: %C", filename, line, colour);
    } else {
      eprint("%C", colour);
    }

    /// Assert condition (if any) and message.
    if (assert_condition) {
      eprint("Assertion failed:%m ‘%B38%s%m’: ", assert_condition);
    } else {
      eprint("%s:%m ", diagnostic_level_names[is_sorry ? DIAG_SORRY : DIAG_ICE]);
    }

    /// Message.
    va_list ap;
    va_start(ap, fmt);
    eprint("%F\n", fmt, ap);
    va_end(ap);
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
