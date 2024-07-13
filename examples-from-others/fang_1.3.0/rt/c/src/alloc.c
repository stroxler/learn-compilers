#include "fang_rt/alloc.h"

#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "fang_rt/error.h"

static struct {
  char* start;
  char* offset;
  fang_int remaining_byte;
} buffer = {.start = NULL, .offset = NULL, .remaining_byte = 0};

static void finalize(void) {
  free(buffer.start);
}

static void initialize(void) {
  // 32 MiB
  fang_int const size = 32 * 1024 * 1024;
  buffer.start = malloc(size);

  if (buffer.start == NULL) {
    fang_error("failed to allocate memory: [malloc] %s", strerror(errno));
  }

  buffer.offset = buffer.start;
  buffer.remaining_byte = size;
  atexit(&finalize);
}

void* fang_alloc(fang_int size_byte) {
  if (size_byte < 0) {
    fang_error("invalid allocation size: %" FANG_PRId_INT " B", size_byte);
  }

  if (buffer.start == NULL) {
    initialize();
  }

  if (size_byte > buffer.remaining_byte) {
    fang_error("out of memory");
  }

  void* const p = buffer.offset;
  buffer.offset += size_byte;
  buffer.remaining_byte -= size_byte;
  return p;
}

fang_word* fang_alloc_array(fang_int size, fang_word initial) {
  if (size < 0) {
    fang_error("negative array size: %" FANG_PRId_INT, size);
  }

  fang_int const size_byte = (size + 1) * sizeof(fang_word);
  fang_word* ptr = fang_alloc(size_byte);
  ptr[0] = size;

  for (fang_int i = 0; i < size; ++i) {
    ptr[i + 1] = initial;
  }

  return ptr;
}

fang_word* fang_alloc_record(fang_int size) {
  fang_int const size_byte = size * sizeof(fang_word);
  return fang_alloc(size_byte);
}
