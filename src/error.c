#include <error.h>

#include <assert.h>
#include <stdio.h>
#include <stddef.h>
#include <stdarg.h>
#include <stdlib.h>

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