#pragma once

#include <stdnoreturn.h>

#include "fang_rt/types.h"

noreturn void fang_error(char const* fmt, ...);

noreturn void fang_error_out_of_bounds(fang_int index, fang_int size);

noreturn void fang_error_nil_access(void);
