#include <stdlib.h>
#include <stdio.h>
#include "jdlutil.h"

/* Utility function to copy a string into another memory location */
void jdlutil_strcpy(char* dest, char* src) {
    // Define function variables
    int l = 1;
    char* ptr = src;

    // Check for null pointer
    if (src == NULL) {
        return;
    }

    // Determine size of src
    while (*ptr != '\0') {
        l++;
        ptr++;
        if (l > 65536) {
            return;
        }
    }
    fprintf(stdout, "src = %s\n", src);
    fprintf(stdout, "src length = %d\n", l);
}

