#include <string_utils.h>
#include <stdlib.h>
#include <string.h>

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