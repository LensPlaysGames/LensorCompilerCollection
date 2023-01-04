#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <utils.h>
#include <error.h>
#include <math.h>

#ifdef _WIN32
/// Basically strndup(), but for Windows.
static char *strndup(const char *str, usz sz) {
  char *dup = malloc(sz + 1);
  memcpy(dup, str, sz);
  dup[sz] = 0;
  return dup;
}
#endif

THREAD_LOCAL bool _thread_use_diagnostics_colours_ = false;

string string_dup_impl(const char *src, usz size) {
  string dest;
  dest.data = strndup(src, size);
  dest.size = size;
  return dest;
}

void string_append_impl(string *lhs, string rhs) {
  string dest;
  dest.size = lhs->size + rhs.size;
  dest.data = malloc(dest.size + 1);
  memcpy(dest.data, lhs->data, lhs->size);
  memcpy(dest.data + lhs->size, rhs.data, rhs.size);
  dest.data[dest.size] = '\0';
  *lhs = dest;
}

void string_append_raw_impl(string *lhs, const char *rhs) {
  if (!rhs || rhs[0] == 0) return;
  size_t rhs_size = strlen(rhs);
  string dest;
  dest.size = lhs->size + rhs_size;
  dest.data = malloc(dest.size + 1);
  memcpy(dest.data, lhs->data, lhs->size);
  memcpy(dest.data + lhs->size, rhs, rhs_size);
  dest.data[dest.size] = '\0';
  *lhs = dest;
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

usz number_width(u64 n) { return n ? (usz) log10((double)n) + 1 : 1; }
