#include "fang_rt/error.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

noreturn void fang_error(char const* fmt, ...) {
  va_list args;
  va_start(args, fmt);
  fputs("Fatal error: ", stderr);
  vfprintf(stderr, fmt, args);
  fputc('\n', stderr);
  abort();
}

noreturn void fang_error_out_of_bounds(fang_int index, fang_int size) {
  fang_error("index %" FANG_PRId_INT " is out of bounds for array of size %" FANG_PRId_INT, index, size);
}

noreturn void fang_error_nil_access(void) {
  fang_error("access of [nil] value");
}
