#include <error.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>

#ifndef _WIN32
#  include <execinfo.h>
#  include <linux/limits.h>
#  include <unistd.h>
#  define PATH_SEPARATOR "/"
#  define ADDR2LINE_BUFFER_SIZE 1024
#else
#  include <Windows.h>
#  include <dbghelp.h>
#  include <io.h>
#  define isatty _isatty
#  define PATH_SEPARATOR "\\"
#endif

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
  bool is_terminal = isatty(fileno(stderr));

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
    if (is_terminal) fprintf(stderr, "\033[m\033[1;38m");
    fprintf(stderr, "%s:%u:%u: ", filename, line, location.start - line_start);
    if (is_terminal) fprintf(stderr, "%s", diagnostic_level_colours[level]);
    fprintf(stderr, "%s: ", diagnostic_level_names[level]);
    if (is_terminal) fprintf(stderr, "\033[m\033[1;38m");
    vfprintf(stderr, fmt, ap);
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
    if (is_terminal) fprintf(stderr, "\033[m");
    fprintf(stderr, "\n");
  }

  /// Otherwise, just print a simple error message.
  else {
    if (is_terminal) fprintf(stderr, "\033[m\033[1;38m");
    fprintf(stderr, "%s: ", filename);
    if (is_terminal) fprintf(stderr, "%s", diagnostic_level_colours[level]);
    fprintf(stderr, "%s: ", diagnostic_level_names[level]);
    if (is_terminal) fprintf(stderr, "\033[m\033[1;38m");
    vfprintf(stderr, fmt, ap);
    if (is_terminal) fprintf(stderr, "\033[m");
    fprintf(stderr, "\n");
  }
 }

static void print_backtrace(int ignore) {
  bool term = isatty(fileno(stderr));

#ifndef _WIN32
  void *array[15];
  int size = backtrace(array, 10);
  char **strings = backtrace_symbols(array + ignore, size - ignore);

  /// backtrace_symbols() may fail if we’re out of memory
  if (!strings) {
    write(STDERR_FILENO, "Out of memory: Cannot format stack trace\n", 41);
    backtrace_symbols_fd(array + ignore, size - ignore, STDERR_FILENO);
    return;
  }

  /// Check if we have addr2line, and if so, get the name of this executable.
  bool have_addr2line = system("which addr2line > /dev/null 2>&1") == 0;
  static __thread char execname[PATH_MAX];
  usz execname_len = 0;
  if (have_addr2line) {
    ssize_t len = readlink("/proc/self/exe", execname, sizeof(execname));
    if (len > 0) execname_len = (usz) len;
    else have_addr2line = false;
  }

  /// Format the stack traces. No need to demangle anything since this is C
  for (char** func = strings; func < strings + size - 2; func++) {
    char *name_begin = strchr(*func, '(');
    char *name_end = strchr(*func, '+');
    char *address_end = strchr(*func, ')');

    /// Print the function if it is a valid stack trace entry.
    if (name_begin && name_end && address_end) {
      /// If we have addr2line, attempt to get the source location.
      static __thread char buffer[ADDR2LINE_BUFFER_SIZE];
      static __thread char name_buffer[ADDR2LINE_BUFFER_SIZE];
      const char *line_start = NULL;
      if (have_addr2line) {
        /// Run addr2line.
        snprintf(buffer, sizeof(buffer), "addr2line -f -C -e %.*s %.*s",
          (int) execname_len, execname,
          (int) (address_end - name_begin - 1), name_begin + 1);
        FILE *fp = popen(buffer, "r");
        if (fp) {
          /// Attempt to read the result.
          if (fgets(name_buffer, sizeof(name_buffer), fp) && fgets(buffer, sizeof(buffer), fp)) {
            /// If the first character is '?', then we couldn’t find the symbol.
            if (buffer[0] != '?') {
              /// Skip the filename.
              char *ptr = buffer;
              while (ptr = strchr(ptr, ':'), ptr) line_start = ptr = ptr + 1;

              /// Ignore discriminators.
              if (line_start) {
                ptr = strpbrk(line_start, " \r\n");
                if (ptr) *ptr = 0;
              }

              /// Remove the newline in the function name.
              ptr = strpbrk(name_buffer, "\r\n");
              if (ptr) *ptr = 0;
            }
          }
          pclose(fp);
        }
      }

      *name_end = 0;
      *address_end = 0;

      if (line_start) {
        fprintf(stderr, "  in function %s%s():%s%s\n",
          term ? "\033[m\033[1;38m" : "",
          name_buffer,
          line_start,
          term ? "\033[m" : "");
      } else {
        fprintf(stderr, "  in function %s%s%s%s at offset %s%s%s\n",
          term ? "\033[m\033[1;38m" : "",
          name_begin + 1 == name_end ? "\?\?\?" : name_begin + 1,
          name_begin + 1 == name_end ? "" : "()",
          term ? "\033[m" : "",
          term ? "\033[m\033[1;38m" : "",
          name_end + 1,
          term ? "\033[m" : "");
      }


      /// Don’t go any higher than main().
      if (strcmp(name_begin + 1, "main") == 0) break;
    }
  }

#else
    typedef BOOL SymInitializeFunc(
        _In_ HANDLE hProcess,
        _In_ PCSTR  UserSearchPath,
        _In_ BOOL   fInvadeProcess
    );

    typedef BOOL SymFromAddrFunc(
        _In_    HANDLE       hProcess,
        _In_    DWORD64      Address,
        _Out_   PDWORD64     Displacement,
        _Inout_ PSYMBOL_INFO Symbol
    );

    typedef BOOL SymGetLineFromAddr64Func(
        _In_  HANDLE hProcess,
        _In_  DWORD64 qwAddr,
        _Out_ PDWORD pdwDisplacement,
        _Out_ PIMAGEHLP_LINE64 Line64
    );

    /// Get the stacktrace.
    void* stack[100];
    WORD frames = CaptureStackBackTrace(ignore, 100, stack, NULL);

    /// Load DbgHelp.dll.
    HMODULE dbghelp = LoadLibrary(TEXT("DbgHelp.dll"));
    if (!dbghelp) {
    /// Loading failed. Print just the addresses.
    print_raw:
        fprintf(stderr, "  Cannot obtain symbols from backtrace: Could not load DbgHelp.dll\n");
        for (WORD i = 0; i < frames; i++)
            fprintf(stderr, "  at address %p", stack[i]);
        return;
    }

    SymInitializeFunc *SymInitialize = (SymInitializeFunc*)GetProcAddress(dbghelp, "SymInitialize");
    if (!SymInitialize) goto print_raw;

    SymFromAddrFunc *SymFromAddr = (SymFromAddrFunc*)GetProcAddress(dbghelp, "SymFromAddr");
    if (!SymFromAddr) goto print_raw;

    SymGetLineFromAddr64Func *SymGetLineFromAddr64 = (SymGetLineFromAddr64Func*)GetProcAddress(dbghelp, "SymGetLineFromAddr64");

    HANDLE process = GetCurrentProcess();
    SymInitialize(process, NULL, TRUE);


    char* symbol_info_data[sizeof(SYMBOL_INFO) + 256 * sizeof(char)];
    SYMBOL_INFO* symbol = (SYMBOL_INFO*)symbol_info_data;
    symbol->MaxNameLen = 255;
    symbol->SizeOfStruct = sizeof(SYMBOL_INFO);

    for (int i = 0; i < frames; i++) {
        SymFromAddr(process, (DWORD64)(stack[i]), 0, symbol);

        /// Attempt to get the line from the address.
        IMAGEHLP_LINE64 line = { 0 };
        line.SizeOfStruct = sizeof(IMAGEHLP_LINE64);
        DWORD displacement = 0;
        bool have_line = false;
        if (SymGetLineFromAddr64) {
            have_line = SymGetLineFromAddr64(process, (DWORD64)(stack[i]), &displacement, &line);
        }

        if (have_line) {
            fprintf(stderr, "  in function %s%s():%li%s\n",
                term ? "\033[m\033[1;38m" : "", symbol->Name, line.LineNumber, term ? "\033[m" : "");
        } else {
            fprintf(stderr, "  in function %s%s()%s at offset %s%llx%s\n",
                term ? "\033[m\033[1;38m" : "", symbol->Name, term ? "\033[m" : "",
                term ? "\033[m\033[1;38m" : "", symbol->Address, term ? "\033[m" : "");
        }

        if (strcmp(symbol->Name, "main") == 0) break;
    }

    FreeLibrary(dbghelp);
#endif
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
  bool is_terminal = isatty(fileno(stderr));

  /// Removing everything up to and including the `src` prefix.
  const char *filename = file, *src_prefix;
  while (src_prefix = strstr(filename, "src" PATH_SEPARATOR), src_prefix) filename = src_prefix + 4;

  /// To make this less atrocious,
  const char *const reset = is_terminal ? "\033[m" : "";
  const char *const w = is_terminal ? "\033[m\033[1;38m" : "";
  const char *const colour = is_terminal ? diagnostic_level_colours[is_sorry ? DIAG_SORRY : DIAG_ICE] : "";

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
  print_backtrace(is_signal_or_exception ? 4 : 2);
#else
  print_backtrace(is_signal_or_exception ? 9 : 2);
#endif

  /// Exit.
  _Exit(1);
}
