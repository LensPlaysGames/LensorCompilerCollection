#ifndef COMPILER_ERROR_H
#define COMPILER_ERROR_H

// TODO: Add file path, byte offset, etc.
typedef struct Error {
  enum ErrorType {
    ERROR_NONE = 0,
    ERROR_ARGUMENTS,
    ERROR_TYPE,
    ERROR_GENERIC,
    ERROR_SYNTAX,
    ERROR_TODO,
    ERROR_MAX,
  } type;
  char *msg;
} Error;

void print_error(Error err);

extern Error ok;

#define ERROR_CREATE(n, t, msg)                 \
  Error (n) = { (t), (msg) }

#define ERROR_PREP(n, t, message)               \
  (n).type = (t);                               \
  (n).msg = (message);

#ifndef _MSC_VER
__attribute__((noreturn))
__attribute__((format(printf, 1, 2)))
#endif
void panic(const char *fmt, ...);

#ifndef _MSC_VER
__attribute__((noreturn))
__attribute__((format(printf, 2, 3)))
#endif
void panic_with_code(int code, const char *fmt, ...);


#endif /* COMPILER_ERROR_H */
