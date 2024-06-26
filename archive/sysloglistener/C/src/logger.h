#ifndef LOGGER_H
#define LOGGER_H

/* Includes */
#include "slutil.h"

/* Functions */
void logger_server_setup(char*, char*);
void logger_listen(void);

void logger_set_identifier(char*);
char* logger_get_identifier(void);
void logger_set_socket(char*);
char* logger_get_socket(void);

void logger_shutdown(void);

#endif

