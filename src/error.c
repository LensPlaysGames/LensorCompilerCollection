#include <assert.h>
#include <error.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

Error ok = { ERROR_NONE, NULL };

void print_error(Error err) {
  if (err.type == ERROR_NONE) {
    return;
  }
  printf("ERROR: ");
  assert(ERROR_MAX == 6);
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

FUNC_NORETURN
static void vpanic(int code, const char *fmt, va_list args) {
  fprintf(stderr, "Panic: ");
  vfprintf(stderr, fmt, args);
  fputc('\n', stderr);
  exit(code);
}

void panic(const char *fmt, ...) {
  va_list va;
  va_start(va, fmt);
  vpanic(1, fmt, va);
  exit(1); // unreachable
}

void panic_with_code(int code, const char *fmt, ...) {
  va_list va;
  va_start(va, fmt);
  vpanic(code, fmt, va);
  exit(1); // unreachable
}

void func_assert_impl(
    const char *file,
    const char *func,
    int line,
    const char *condition,
    const char *fmt,
    ...
) {
  /// Prettier file name
  const char* basename = strrchr(file, FUNC_PATH_SEPARATOR[0]);
  file = basename ? basename + 1 : file;

  fprintf(stderr, "Assertion failed: %s\n", condition);
  fprintf(stderr, "    In file %s:%d\n", file, line);
  fprintf(stderr, "    In function %s", func);

  if (fmt) {
    fprintf (stderr, "\n    Message: ");

    va_list va;
    va_start(va, fmt);
    fprintf(stderr, fmt, va);
    va_end(va);
  }

  fputc('\n', stderr);
  exit(1);
}