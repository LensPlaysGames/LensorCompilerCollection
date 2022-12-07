#include <error.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>

/// For isatty().
#ifndef _WIN32
#    include <unistd.h>
#    include <execinfo.h>
#else
#    include <Windows.h>
#    include <dbghelp.h>
#    include <io.h>
#    define isatty _isatty
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

static void print_backtrace() {
  bool term = isatty(fileno(stderr));
  
#ifndef _WIN32
  void *array[15];
  int size = backtrace(array, 10);
  char **strings = backtrace_symbols(array + 2, size - 2);

  /// backtrace_symbols() may fail if we’re out of memory
  if (!strings) {
    write(STDERR_FILENO, "Out of memory: Cannot format stack trace\n", 41);
    backtrace_symbols_fd(array + 2, size - 2, STDERR_FILENO);
    return;
  }

  /// Format the stack traces. No need to demangle anything since this is C
  for (char** func = strings; func < strings + size - 2; func++) {
    char *name_begin = strchr(*func, '(');
    char *name_end = strchr(*func, '+');
    char *address_end = strchr(*func, ')');
    if (name_begin && name_end && address_end) {
      *name_end = 0;
      *address_end = 0;
      fprintf(stderr, "  in function %s%s()%s at offset %s%s%s\n",
        term ? "\033[m\033[1;38m" : "", name_begin + 1, term ? "\033[m" : "",
        term ? "\033[m\033[1;38m" : "", name_end + 1, term ? "\033[m" : "");

      /// Don’t go any higher than main().
      if (strcmp(name_begin + 1, "main") == 0) break;
    }
  }

#else
    typedef BOOL IMAGEAPI SymInitializeFunc(
        _In_ HANDLE hProcess,
        _In_ PCSTR  UserSearchPath,
        _In_ BOOL   fInvadeProcess
    );

    typedef BOOL IMAGEAPI SymFromAddrFunc(
        _In_    HANDLE       hProcess,
        _In_    DWORD64      Address,
        _Out_   PDWORD64     Displacement,
        _Inout_ PSYMBOL_INFO Symbol
    );

    /// Get the stacktrace.
    void* stack[100];
    WORD frames = CaptureStackBackTrace(2, 100, stack, NULL);

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

    HANDLE process = GetCurrentProcess();
    SymInitialize(process, NULL, TRUE);


    char* symbol_info_data[sizeof(SYMBOL_INFO) + 256 * sizeof(char)];
    SYMBOL_INFO* symbol = (SYMBOL_INFO*)symbol_info_data;
    symbol->MaxNameLen = 255;
    symbol->SizeOfStruct = sizeof(SYMBOL_INFO);

    for (int i = 0; i < frames; i++) {
        SymFromAddr(process, (DWORD64)(stack[i]), 0, symbol);
        fprintf(stderr, "  in function %s%s()%s at offset %s%llx%s\n",
            term ? "\033[m\033[1;38m" : "", symbol->Name, term ? "\033[m" : "",
            term ? "\033[m\033[1;38m" : "", symbol->Address, term ? "\033[m" : "");
        if (strcmp(symbol->Name, "main") == 0) break;
    }

    FreeLibrary(dbghelp);
#endif
}

NORETURN
FORMAT(printf, 4, 5)
void ice_impl (
    const char *file,
    const char *func,
    int line,
    const char *fmt,
    ...
) {
  /// Check if stderr is a terminal.
  bool is_terminal = isatty(fileno(stderr));
  if (is_terminal) fprintf(stderr, "\033[m\033[1;38m");
  fprintf(stderr, "%s: ", file);
  if (is_terminal) fprintf(stderr, "\033[m");
  fprintf(stderr, "In function '");
  if (is_terminal) fprintf(stderr, "\033[1;38m");
  fprintf(stderr, "%s", func);
  if (is_terminal) fprintf(stderr, "\033[m");
  fprintf(stderr, "':\n");

  if (is_terminal) fprintf(stderr, "\033[1;38m");
  fprintf(stderr, "%s:%d: ", file, line);
  if (is_terminal) fprintf(stderr, "%s", diagnostic_level_colours[DIAG_ICE]);
  fprintf(stderr, "%s: ", diagnostic_level_names[DIAG_ICE]);
  if (is_terminal) fprintf(stderr, "\033[m");

  va_list args;
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);

  if (is_terminal) fprintf(stderr, "\033[m");
  fprintf(stderr, "\n");

  print_backtrace();

  _Exit(1);
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

  if (strcmp(fmt, "") != 0) {
    fprintf(stderr, "\n    Message: ");

    // Skip '!' placeholder for non-empty format string.
    fmt += 1;

    va_list va;
    va_start(va, fmt);
    vfprintf(stderr, fmt, va);
    va_end(va);
  }

  fputc('\n', stderr);

  print_backtrace();

  _Exit(1);
}
