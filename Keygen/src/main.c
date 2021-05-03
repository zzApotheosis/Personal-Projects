#include <stdlib.h>
#include <stdio.h>
#include "main.h"

int main(int argc, char** argv) {
    fprintf(stdout, "Hello world\n");
    fprintf(stdout, "charset_lc[0] = %c\n", charset_lc[0]);
    fprintf(stdout, "charset_lc size = %d\n", CHARSET_LC_SIZE);
    return EXIT_SUCCESS;
}
