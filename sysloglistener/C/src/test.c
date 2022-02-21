#include <stdlib.h>
#include <stdio.h>
#include "test.h"

void test_slutil_strcpy() {
    char* test = NULL;
    slutil_strcpy(&test, "Testing from test_slutil_strcpy()");
    fprintf(stdout, "test_slutil_strcpy() %s\n", test);
    free(test);
}

void test_slutil_strlen() {
    char* test = "Hello world";
    int l = slutil_strlen(test);
    fprintf(stdout, "test_slutil_strlen() test = %s\n", test);
    fprintf(stdout, "test_slutil_strlen() l = %d\n", l);
}

int main(int argc, char** argv) {
    test_slutil_strcpy();
    test_slutil_strlen();
    return EXIT_SUCCESS;
}

