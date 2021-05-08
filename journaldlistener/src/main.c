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
    setup_listener("This is a test");
    // unixsocket_example();
    return EXIT_SUCCESS;
}

/* SIGINT handler function */
void sigint_handler(int sig) {
    unlink("socket");
    exit(EXIT_SUCCESS);
}
