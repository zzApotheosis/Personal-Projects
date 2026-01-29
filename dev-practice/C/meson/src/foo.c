#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "foo.h"

void library_function(const int x) {
    fprintf(stderr, "Library function received integer: %d\n", x);
}
