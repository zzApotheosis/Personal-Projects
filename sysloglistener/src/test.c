#include <stdlib.h>
#include <stdio.h>
#include "test.h"

void test_slutil_strcpy() {
    char* test = NULL;
    slutil_strcpy(&test, "Testing from test_slutil_strcpy()");
    fprintf(stdout, "%s\n", test);
    free(test);
}

int main(int argc, char** argv) {
    test_slutil_strcpy();
    return EXIT_SUCCESS;
}

