#include <stdlib.h>
#include <stdio.h>
#include "include/util.h"

size_t util_strlen(char* s) {
    size_t l = (size_t) 0u;
    char* i = s;
    while (*i != '\0') {
        l++;
        i++;
        if (l >= UTIL_STR_MAX_LEN) return l;
    }
    return l;
}

