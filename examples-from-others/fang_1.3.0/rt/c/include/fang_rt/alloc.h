#pragma once

#include "fang_rt/types.h"

/** Memory allocation.

    A single large buffer of 32 MiB is allocated on-demand at the point of the first allocation.

    This has the benefit of being very simple and making individual allocations very fast.

    However, once the buffer is full, subsequent allocations will abort the program.
 */

/** Allocate a contiguous region of memory, in byte. */
void* fang_alloc(fang_int);

/**
    The first parameter is the number of elements in the new array.
 */
fang_word* fang_alloc_array(fang_int size, fang_word initial);

/**
    It's only necessary to know the number of fields in the record.
 */
fang_word* fang_alloc_record(fang_int size);
