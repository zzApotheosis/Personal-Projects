#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include "server.h"

/* Forward declarations */
void sigint_handler(int);

/* Main */
int main(int argc, char** argv) {
    signal(SIGINT, sigint_handler);
    logger_setup("sysloglistener", "\0S.sysloglistener");
    logger_set_socket("\0sysloglistener");
    return EXIT_SUCCESS;
}

/* SIGINT handler function */
void sigint_handler(int sig) {
    unlink("socket");
    exit(EXIT_SUCCESS);
}
