#ifndef _WIN32
#  include <execinfo.h>
#  include <linux/limits.h>
#  include <unistd.h>
#  include <signal.h>
#  include <fcntl.h>
#  include <sys/stat.h>
#  include <sys/mman.h>
#  include <errno.h>
#  define ADDR2LINE_BUFFER_SIZE 1024
#else
#  include <Windows.h>
#  include <dbghelp.h>
#  include <io.h>
#  define isatty _isatty
#endif

#include <error.h>
#include <platform.h>

#include <math.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <locale.h>

/// ===========================================================================
///  Initialisation
/// ===========================================================================
#ifndef _WIN32
/// SIGSEGV/SIGILL/SIGABRT handler.
static void ice_signal_handler(int signal, siginfo_t *info, void* unused) {
  (void) unused;
  switch (signal) {
    case SIGSEGV: ICE_EXCEPTION("Segmentation fault at 0x%X", (u64)info->si_addr);
    case SIGILL: ICE_EXCEPTION("Illegal instruction");
    case SIGABRT: ICE_EXCEPTION("Aborted");
    default: ICE_EXCEPTION("UNREACHABLE");
  }
}
#else
/// Unhandled SEH exception handler.
static LONG WINAPI unhandled_exception_handler(EXCEPTION_POINTERS* info) {
  switch (info->ExceptionRecord->ExceptionCode) {
    case EXCEPTION_ACCESS_VIOLATION: ICE_EXCEPTION("Segmentation Fault at 0x%X", (u64)info->ExceptionRecord->ExceptionInformation[1]);
    case EXCEPTION_ILLEGAL_INSTRUCTION: ICE_EXCEPTION("Illegal instruction");
    case EXCEPTION_STACK_OVERFLOW: ICE_EXCEPTION("Stack Overflow");
    case EXCEPTION_INT_DIVIDE_BY_ZERO: ICE_EXCEPTION("Division by Zero");
    default: ICE_EXCEPTION("Unhandled exception 0x%X", (u64)info->ExceptionRecord->ExceptionCode);
  }
}
#endif

void platform_init(void) {
#ifndef _WIN32
  /// Install signal handlers.
  struct sigaction sa = {0};
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = SA_SIGINFO;
  sa.sa_sigaction = ice_signal_handler;
  if (sigaction(SIGSEGV, &sa, NULL)) ICE("Failed to install SIGSEGV handler");
  if (sigaction(SIGABRT, &sa, NULL)) ICE("Failed to install SIGABRT handler");
  if (sigaction(SIGILL, &sa, NULL)) ICE("Failed to install SIGILL handler");
#else
# if defined (ENABLE_VIRTUAL_TERMINAL_PROCESSING)
  /// Enable console colours explicitly in case the user is using CMD for some reason.
  DWORD omode, emode;
  HANDLE out = GetStdHandle(STD_OUTPUT_HANDLE);
  HANDLE err = GetStdHandle(STD_ERROR_HANDLE);
  GetConsoleMode(out, &omode);
  GetConsoleMode(err, &emode);
  SetConsoleMode(out, omode | ENABLE_VIRTUAL_TERMINAL_PROCESSING);
  SetConsoleMode(err, emode | ENABLE_VIRTUAL_TERMINAL_PROCESSING);
# endif

  /// Set the unhandled exception handler.
  SetUnhandledExceptionFilter(unhandled_exception_handler);

  /// Make sure that the system knows that we're using UTF-8.
  SetConsoleOutputCP(CP_UTF8);
  SetConsoleCP(CP_UTF8);
#endif
}

/// ===========================================================================
///  Miscellanea
/// ===========================================================================
bool platform_isatty(int fd) { return isatty(fd); }

void platform_print_backtrace(int ignore) {
  bool term = platform_isatty(fileno(stderr));

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
  for (char** func = strings; func < strings + size - ignore; func++) {
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
              while (ptr = strchr(ptr, ':'), ptr) {
                ptr += 1;
                line_start = ptr;
              }

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
        eprint("  in function %s%s():%s%s\n",
                term ? "\033[m\033[1;38m" : "",
                name_buffer,
                line_start,
                term ? "\033[m" : "");
      } else {
        eprint("  in function %s%s%s%s at offset %s%s%s\n",
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
    eprint("  Cannot obtain symbols from backtrace: Could not load DbgHelp.dll\n");
    for (WORD i = 0; i < frames; i++)
      eprint("  at address %p", stack[i]);
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
      eprint("  in function %s%s():%D%s\n",
              term ? "\033[m\033[1;38m" : "", symbol->Name, line.LineNumber, term ? "\033[m" : "");
    } else {
      eprint("  in function %s%s()%s at offset %s%X%s\n",
              term ? "\033[m\033[1;38m" : "", symbol->Name, term ? "\033[m" : "",
              term ? "\033[m\033[1;38m" : "", (u64)symbol->Address, term ? "\033[m" : "");
    }

    if (strcmp(symbol->Name, "main") == 0) break;
  }

  FreeLibrary(dbghelp);
#endif
}

static string standard_read_file_contents(const char *path, bool *success) {
  /// Open the file.
  if (success) *success = false;
  FILE *file = fopen(path, "rb");
  if (!file) return format("Could not open file \"%s\" for reading: %s", path, strerror(errno));

  /// Determine its size by seeking to the end.
  fseek(file, 0, SEEK_END);
  long tell = ftell(file);
  if (tell < 0) return format("Could not determine size of file \"%s\": %s", path, strerror(errno));
  usz size = (usz)tell;

  /// Read the contents into a string.
  string str = {0};
  str.size = size + 1;
  str.data = malloc(str.size);
  rewind(file);
  if (fread(str.data, 1, size, file) != size) {
    free(str.data);
    return format("Could not read contents of file \"%s\": %s", path, strerror(errno));
  }
  str.data[size] = 0;

  /// Close the file.
  fclose(file);

  /// Return the string.
  if (success) *success = true;
  return str;
}

string platform_read_file(const char *path, bool *success) {
#ifndef _WIN32
  /// Try to open the file.
  if (success) *success = false;
  int fd = open(path, O_RDONLY);
  if (fd < 0) return format("Could not open file \"%s\" for reading: %s", path, strerror(errno));

  /// Stat it to determine its size.
  struct stat st;
  if (fstat(fd, &st) < 0) return format("Could not stat file \"%s\": %s", path, strerror(errno));

  /// Make sure it’s a regular file.
  if (!S_ISREG(st.st_mode)) return format("Path \"%s\" is not a regular file", path);

  /// If the size is 0, mmap() is both unnecessary and would fail,
  /// so just return an empty string.
  if (st.st_size == 0) {
    if (success) *success = true;
    return (string){0};
  }

  /// Map the file into memory.
  void *data = mmap(NULL, (usz)st.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
  if (data == MAP_FAILED) return standard_read_file_contents(path, success);

  /// Close the file descriptor.
  (void) close(fd);

  /// Copy the data into a string.
  string str = {0};
  str.size = (usz)st.st_size + 1;
  str.data = malloc(str.size);
  memcpy(str.data, data, str.size - 1);
  str.data[str.size - 1] = 0;

  /// Unmap the file.
  (void) munmap(data, (usz)st.st_size);

  /// Return the string.
  if (success) *success = true;
  return str;
#else
  return standard_read_file_contents(path, success);
#endif
}
