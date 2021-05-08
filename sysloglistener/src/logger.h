#ifndef LOGGER_H
#define LOGGER_H

/* Includes */
#include "jdlutil.h"

/* Functions */
void logger_setup(char*);
void logger_set_socket(char*);
char* logger_get_socket(void);
void logger_cleanup(void);

#endif

