#include <stdlib.h>
#include <stdio.h>
#include "project.h"

/* Global variables */
char* project_name = NULL;
char* socket = NULL;

/* Project Name */
void project_set_name(char* new_name) {
    if (project_name != NULL) free(project_name);
    slutil_strcpy(&project_name, new_name);
}
char* project_get_name() {
    char* copy = NULL;
    slutil_strcpy(&copy, project_name);
    return copy;
}

/* Socket Name */
void project_set_socket(char* new_socket) {
    if (socket != NULL) free(socket);
    slutil_strcpy(&socket, new_socket);
}
char* project_get_socket() {
    char* copy = NULL;
    slutil_strcpy(&copy, socket);
    return copy;
}

