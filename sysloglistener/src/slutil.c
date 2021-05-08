#include <stdlib.h>
#include <stdio.h>
#include "slutil.h"

/* Utility function to copy a string into another memory location */
void slutil_strcpy(char** dest, char* src) {
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

    // Perform memory copy
    *dest = (char*) malloc(l * sizeof(char));
    for (int i = 0; i < l; i++) {
        (*dest)[i] = src[i];
    }
}

/* Utility function to get the number of chars between a char pointer and a null terminator */
size_t slutil_strlen(char* cp) {
    size_t l = 1; // Initialize to 1 to account for null terminator
    char* ptr = cp;
    if (cp == NULL) return (size_t) 0;
    while (*ptr != '\0') {
        l++;
        ptr++;
        if (l >= 65536) return l;
    }
    return l;
}

