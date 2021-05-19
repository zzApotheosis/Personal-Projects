#include <stdlib.h>
#include <stdio.h>
#include "include/util.h"

#define MAX_LEN 65536

size_t util_strlen(char* s) {
    size_t l = (size_t) 0u;
    char* i = s;
    while (*i != '\0') {
        l++;
        i++;
        if (l >= MAX_LEN) return l;
    }
    return l;
}

