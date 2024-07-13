#include "fang_rt/io.h"

#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "fang_rt/error.h"

static fang_int print_char(char b) {
  int const e = putchar(b);

  if (e == EOF) {
    fang_error("failed to write to stdout: [putchar] %s", strerror(errno));
  }

  return 0;
}

void fang_io_print(fang_String const* s) {
  fang_string_iter(s, &print_char);
}

void fang_io_print_int(fang_int x) {
  int const e = printf("%" FANG_PRId_INT, x);

  if (e < 0) {
    fang_error("failed to write to stdout: [printf] %s", strerror(errno));
  }
}

void fang_io_print_line(void) {
  print_char('\n');
}

fang_String const* fang_io_read_char(void) {
  int const e = getchar();

  if (e == EOF) {
    if (feof(stdin)) {
      return fang_string_empty();
    }

    fang_error("failed to read from stdin: [getchar] %s", strerror(errno));
  }

  return fang_string_char((fang_int)e);
}

void fang_io_flush(void) {
  int const e = fflush(stdout);

  if (e != 0) {
    fang_error("failed to flush stdout: [fflush] %s", strerror(errno));
  }
}

void fang_io_seed_random(fang_int seed) {
  srand((int)seed);
}

fang_int fang_io_random(fang_int low, fang_int high) {
  if (low > high - 1) {
    fang_error("empty random range [%" FANG_PRId_INT ",%" FANG_PRId_INT ")", low, high);
  }

  int const x = rand();
  double const u = (double)x / RAND_MAX;
  return (fang_int)(u * (high - low)) + low;
}

noreturn void fang_io_tiger_error(fang_String const* what) {
  fang_error("the Tiger program failed with \"%s\"", fang_string_convert(what));
}

noreturn void fang_io_exit(fang_int code) {
  exit((int)code);
}
