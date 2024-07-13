#include "fang_rt/string.h"
#include "fang_rt/types.h"

extern fang_int fang(void);

int main(void) {
  fang_string_initialize();
  fang_int const r = fang();
  return (int)r;
}
