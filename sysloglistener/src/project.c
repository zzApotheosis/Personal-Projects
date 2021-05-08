#include <stdlib.h>
#include <stdio.h>
#include "project.h"

/* Global variables */
char* project_name = NULL;

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

void project_cleanup() {
    if (project_name != NULL) free(project_name);
}

