#pragma once

#include "fang_rt/types.h"

/** Immutable strings.
 */

typedef struct fang_String fang_String;

/**
    Must be invoked on prior to any other functions in this file.
 */
void fang_string_initialize(void);

fang_String const* fang_string_empty(void);

fang_int fang_string_length(fang_String const*);

fang_int fang_string_iter(fang_String const*, fang_int (*)(char));

fang_String const* fang_string_concat(fang_String const*, fang_String const*);

/**
    The program is aborted if the argument does not fit in a single byte.
 */
fang_String const* fang_string_char(fang_int);

/**
    The result is \c -1 if the string is empty.
 */
fang_int fang_string_ord(fang_String const*);

fang_int fang_string_compare(fang_String const*, fang_String const*);

/**
   The program is aborted if the index and span are out of bounds of the string.
 */
fang_String const* fang_string_sub(fang_String const*, fang_int index, fang_int span);

char* fang_string_convert(fang_String const*);
