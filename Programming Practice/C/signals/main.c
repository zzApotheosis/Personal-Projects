#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <signal.h>

void sigint_handler(int);

int main(int argc, char** argv) {
    signal(SIGINT, sigint_handler);

    fprintf(stdout, "Press Ctrl-C to exit\n");
    
    while (1) {
        usleep(1000000);
    }

    return EXIT_SUCCESS;
}

void sigint_handler(int signum) {
    fprintf(stdout, "Caught signal %d, exiting program\n", signum);
    exit(EXIT_SUCCESS);
}

