#include <stdlib.h>
#include <stdio.h>
#include <syslog.h>
#include "logger.h"

/* Global variables */
char* logger_identifier = NULL;
char* logger_socket = NULL;

/* Initialize syslog */
void logger_setup(char* new_identifier, char* new_socket) {
    logger_set_identifier(new_identifier);
    logger_set_socket(new_socket);
    fprintf(stdout, "%s\n", logger_identifier);
}

/* Write message to syslog */
// void logger_send(int p);

/* Logger Identifier */
void logger_set_identifier(char* new_identifier) {
    if (logger_identifier != NULL) free(logger_identifier);
    slutil_strcpy(&logger_identifier, new_identifier);
}
char* logger_get_identifier() {
    char* logger_identifier_copy = NULL;
    slutil_strcpy(&logger_identifier_copy, logger_identifier);
    return logger_identifier_copy;
}

/* Logger Socket */
void logger_set_socket(char* new_socket) {
    slutil_strcpy(&logger_socket, new_socket);
}
char* logger_get_socket() {
    char* logger_socket_copy = NULL;
    slutil_strcpy(&logger_socket_copy, logger_socket);
    return logger_socket_copy;
}

/* Cleanup function */
void logger_cleanup() {
    free(logger_identifier);
    free(logger_socket);
}
