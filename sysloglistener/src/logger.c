#include <stdlib.h>
#include <stdio.h>
#include <syslog.h>
#include "logger.h"

/* Global variables */
int syslog_initialized = 0; // Default: False
char* identifier = NULL;
char* socket = NULL;

/* Initialize syslog */
void logger_setup(char* new_identifier) {
    jdlutil_strcpy(identifier, new_identifier);
    fprintf(stdout, "%s\n", identifier);
    // Finally, globally declare syslog has been initialized
    syslog_initialized = 1;
}

/* Write message to syslog */
// void logger_send(int p);

/* Set/Get socket location */
void logger_set_socket(char* new_socket) {
    jdlutil_strcpy(socket, new_socket);
}
char* logger_get_socket() {
    char* socket_copy = NULL;
    jdlutil_strcpy(socket_copy, socket);
    return socket_copy;
}

/* Cleanup function */
void logger_cleanup() {
    free(identifier);
}
