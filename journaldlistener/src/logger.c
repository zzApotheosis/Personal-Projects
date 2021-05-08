#include <stdlib.h>
#include <stdio.h>
#include <syslog.h>
#include "logger.h"

/* Global variables */
int syslog_initialized = 0; // Default: False
char* identifier = NULL;

/* Initialize syslog */
void logger_setup(char* new_identifier) {
    jdlutil_strcpy(identifier, new_identifier);
    fprintf(stdout, "%s\n", identifier);
    // Finally, globally declare syslog has been initialized
    syslog_initialized = 1;
}

/* Write message to syslog */
// void logger_send(int p);

/* Cleanup function */
void logger_cleanup() {
    free(identifier);
}
