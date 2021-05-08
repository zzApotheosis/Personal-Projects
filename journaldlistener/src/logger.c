#include <stdlib.h>
#include <stdio.h>
#include <syslog.h>
#include "logger.h"

/* Global variables */
int syslog_initialized = 0; // Default: False
char* identifier = NULL;

/* Initialize syslog */
void logger_setup(const char* new_identifier) {
    int l = 0; // Count the length of given argument (minus one because the null character isn't counted)
    char* ptr = new_identifier;
    while (*ptr != '\0') {
        l++;
        ptr++;
    }
    identifier = (char*) malloc((l + 1) * sizeof(char));

    // Copy memory into identifier
    for (int i = 0; i < l + 1; i++) {
        identifier[i] = new_identifier[i];
    }

    // Finally, globally declare syslog has been initialized
    syslog_initialized = 1;
}

/* Write message to syslog */
// void logger_send(int p);

/* Cleanup function */
void logger_cleanup() {
    free(identifier);
}
