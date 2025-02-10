#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "foo.h"

int main(const int argc, const char * const * const argv) {
    (void) argc;
    (void) argv;
    fprintf(stdout, "Hello, world!\n");
    library_function(42);
    return EXIT_SUCCESS;
}
