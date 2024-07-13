#pragma once

#include <inttypes.h>
#include <stdint.h>

typedef int64_t fang_word;
typedef fang_word fang_int;

#define FANG_PRId_WORD PRId64
#define FANG_PRId_INT FANG_PRId_WORD

fang_int fang_not(fang_int);

/** The program is aborted if the index is out of bounds of the array. */
void fang_check_array_index(fang_word const*, fang_int index);
