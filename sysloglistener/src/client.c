#include <stdlib.h>
#include <stdio.h>
#include "client.h"

int main(int argc, char** argv) {
    char* test = NULL;
    slutil_strcpy(&test, "test");
    fprintf(stdout, "test = %s\n", test);
    free(test);
    return EXIT_SUCCESS;
}
