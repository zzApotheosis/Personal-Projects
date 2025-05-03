#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <time.h>
#include "main.h"

int main(int argc, char** argv) {
    srand(time(NULL));
    int i = 0;
    int limit = 16;
    char* str = malloc(limit * sizeof(char) + 1);
    for (i = 0; i < limit; i++) {
        str[i] = charset_lc[rand() % CHARSET_LC_SIZE];
    }
    str[limit] = '\0';

    fprintf(stdout, "String: %s\n", str);

    free(str);

    return EXIT_SUCCESS;
}
