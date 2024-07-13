#include "fang_rt/types.h"

#include "fang_rt/error.h"

fang_int fang_not(fang_int x) {
  return (x == 0) ? 1 : 0;
}

void fang_check_array_index(fang_word const* ptr, fang_int index) {
  fang_int const size = ptr[0];

  if (index < 0 || index >= size) {
    fang_error_out_of_bounds(index, size);
  }
}
