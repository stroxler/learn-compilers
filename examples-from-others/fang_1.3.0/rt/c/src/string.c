#include "fang_rt/string.h"

#include <assert.h>
#include <stddef.h>
#include <string.h>

#include "fang_rt/alloc.h"
#include "fang_rt/error.h"

typedef struct fang_String {
  fang_int length;
  /** A flexible array member, but storage for at least one character. */
  char body[1];
} __attribute__((packed)) fang_String;

static const fang_String empty = {.length = 0, .body = {0}};

fang_String const* fang_string_empty(void) {
  return &empty;
}

fang_int fang_string_length(fang_String const* t) {
  return t->length;
}

fang_int fang_string_iter(fang_String const* t, fang_int (*f)(char)) {
  for (fang_int i = 0; i < t->length; ++i) {
    fang_int const e = f(t->body[i]);

    if (e != 0) {
      return e;
    }
  }

  return 0;
}

fang_String const* fang_string_concat(fang_String const* t1, fang_String const* t2) {
  if (t1->length == 0) {
    return t2;
  }

  if (t2->length == 0) {
    return t1;
  }

  fang_int const length = t1->length + t2->length;
  fang_String* r = fang_alloc(sizeof(fang_int) + length);
  r->length = length;
  memcpy(r->body, t1->body, t1->length);
  memcpy(r->body + t1->length, t2->body, t2->length);
  return r;
}

static fang_String char_strings[255] = {0};

void fang_string_initialize(void) {
  for (size_t i = 0; i < 255; ++i) {
    fang_String* t = &(char_strings[i]);
    t->length = 1;
    t->body[0] = (char)i;
  }
}

fang_String const* fang_string_char(fang_int c) {
  char const i = (char)c;

  if ((fang_int)i != c) {
    fang_error("[chr] invalid character value: %" FANG_PRId_INT, c);
  }

  return &(char_strings[(size_t)i]);
}

fang_int fang_string_ord(fang_String const* t) {
  if (t->length == 0) {
    return -1;
  }

  return t->body[0];
}

fang_int fang_string_compare(fang_String const* t1, fang_String const* t2) {
  char const* const end1 = t1->body + t1->length;
  char const* const end2 = t2->body + t2->length;
  char const* iter1 = t1->body;
  char const* iter2 = t2->body;

  while ((iter1 != end1) && (iter2 != end2)) {
    fang_int const c = (fang_int)*iter1++ - (fang_int)*iter2++;

    if (c != 0) {
      return c;
    }
  }

  if ((iter1 == end1) && (iter2 == end2)) {
    return 0;
  } else if (iter2 != end2) {
    return -1;
  } else {
    return 1;
  }
}

fang_String const* fang_string_sub(fang_String const* t, fang_int index, fang_int span) {
  if ((index < 0) || (index + span > t->length)) {
    fang_error("sub-string (%" FANG_PRId_INT ", %" FANG_PRId_INT ") on a string of length %" FANG_PRId_INT
               " is out of range",
               index,
               span,
               t->length);
  }

  if (span == 1) {
    return &(char_strings[(size_t)(t->body[index])]);
  }

  fang_String* r = fang_alloc(sizeof(fang_int) + span);
  r->length = span;
  memcpy(r->body, t->body + index, span);
  return r;
}

char* fang_string_convert(fang_String const* t) {
  char* const bytes = fang_alloc((t->length + 1) * sizeof(char));
  memcpy(bytes, t->body, t->length);
  bytes[t->length] = '\0';
  return bytes;
}
