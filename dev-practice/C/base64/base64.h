#ifndef BASE64_H
#define BASE64_H

#include <stddef.h>

char * base64_encode(const char * const, const size_t);
char * base64_decode(const char * const, const size_t);

#endif
