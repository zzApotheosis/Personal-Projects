#include <stdio.h>
#include <stdlib.h>
#include "foo.h"

int main(int argc, char** argv) {
    int exitCode = 0;
    fprintf(stdout, "Hello, world!\n");
    testFunction();
    return exitCode;
}

