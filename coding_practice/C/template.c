#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

#include "template.h"

int main(int argc, char * argv[]) {
        /* Define function variables preferably near the start of the function */
        int exit_code = EXIT_SUCCESS;

        /* Do cool code stuff */
        fprintf(stdout, "Hello, world!\n");
        fflush(stdout);

        /* Exercising my warning macro */
        warn(stderr, "This is a cool warning");

        /* Try passing in any arguments to this program */
        if (argc > 1)
                die(stderr, strerror(errno));

        return exit_code;
}
