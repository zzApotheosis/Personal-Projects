#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "foo.h"

void library_function(int x) {
    fprintf(stdout, "Library function received integer: %d\n", x);
}
