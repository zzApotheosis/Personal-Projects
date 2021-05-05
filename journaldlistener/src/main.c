#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include "main.h"

/* Forward declarations */
void sigint_handler(int);

/* Main */
int main(int argc, char** argv) {
    signal(SIGINT, sigint_handler);
    fprintf(stdout, "Hello world, from C!\n");
    unixsocket_example();
    return EXIT_SUCCESS;
}

/* SIGINT handler function */
void sigint_handler(int sig) {
    fprintf(stdout, "Received SIGINT. Exiting\n");
    unlink("socket");
    exit(EXIT_SUCCESS);
}
