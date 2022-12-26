#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <utils.h>
#include <error.h>

#ifdef _WIN32
/// Basically strndup(), but for Windows.
static char *strndup(const char *str, usz sz) {
  char *dup = malloc(sz + 1);
  memcpy(dup, str, sz);
  dup[sz] = 0;
  return dup;
}
#endif

string string_dup_impl(const char *src, usz size) {
  string dest;
  dest.data = strndup(src, size);
  dest.size = size;
  return dest;
}

string format(const char *fmt, ...) {
  /// Collect the variadic args.
  va_list ap, ap2;
  va_start(ap, fmt);
  va_copy(ap2, ap);

  /// Determine the size of the resulting string.
  int s = vsnprintf(NULL, 0, fmt, ap);
  va_end(ap);
  ASSERT(s >= 0);
  usz size = (usz)s;

  /// Allocate a buffer and format the string.
  string dest = {0};
  dest.data = malloc(size + 1);
  dest.size = size;
  s = vsnprintf(dest.data, size + 1, fmt, ap2);
  ASSERT(s == (int)size);
  va_end(ap2);

  /// Done.
  return dest;
}