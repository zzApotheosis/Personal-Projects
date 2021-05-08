#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include "server.h"

/* Forward declarations */
void server_init(void);
void sigint_handler(int);

/* Main */
int main(int argc, char** argv) {
    /* Define function variables */
    char* project_name = NULL;
    
    /* Perform initialization */
    server_init();
    
    project_name = project_get_name();
    logger_server_setup(project_name, "S.sysloglistener");
    free(project_name);
    
    /* Begin main loop */
    fprintf(stdout, "Listening for connections...\n");
    logger_listen();
    
    /* Cleanup, shutdown, release memory back to OS, etc. */
    project_cleanup();
    logger_shutdown();
    return EXIT_SUCCESS;
}

/* Server initialization function */
void server_init() {
    project_set_name("sysloglistener");
    signal(SIGINT, sigint_handler);
}

/* SIGINT handler function */
void sigint_handler(int sig) {
    project_cleanup();
    logger_shutdown();
    exit(EXIT_SUCCESS);
}

