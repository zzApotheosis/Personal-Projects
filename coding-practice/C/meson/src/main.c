#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "foo.h"

#define PROJECT_NAME "meson"

int main(int argc, char **argv) {
    if (argc != 1) {
        printf("%s takes no arguments.\n", argv[0]);
        return 1;
    }
    printf("This is project %s.\n", PROJECT_NAME);

    library_function(42);

    return 0;
}
