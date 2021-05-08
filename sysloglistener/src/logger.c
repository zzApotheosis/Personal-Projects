#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <syslog.h>
#include <sys/socket.h>
#include <sys/un.h>
#include "logger.h"

/* Forward declare functions not in header */
void logger_cleanup(void);

/* Global variables */
char* logger_identifier = NULL;
char* logger_socket = NULL;
int logger_socket_fd = 0;

/* Initialize listener and syslog */
void logger_server_setup(char* new_identifier, char* new_socket) {
    struct sockaddr_un addr;

    logger_set_identifier(new_identifier);
    logger_set_socket(new_socket);
    
    if ((logger_socket_fd = socket(AF_UNIX, SOCK_STREAM, 0)) == -1) {
        perror("Socket error");
        exit(EXIT_FAILURE);
    }

    memset(&addr, 0, sizeof(addr));
    addr.sun_family = AF_UNIX;
    if (*logger_socket == '\0') {
        *addr.sun_path = '\0';
        strncpy(addr.sun_path + 1, logger_socket + 1, sizeof(addr.sun_path) - 2);
    } else {
        strncpy(addr.sun_path, logger_socket, sizeof(addr.sun_path) - 1);
        unlink(logger_socket);
    }
    
    if (bind(logger_socket_fd, (struct sockaddr*) &addr, sizeof(addr)) == -1) {
        perror("Bind error");
        exit(EXIT_FAILURE);
    }

    openlog(new_identifier, LOG_PERROR | LOG_NDELAY, LOG_USER);
}

void logger_listen() {
    int c1 = 0;
    int rc = 0;
    char buf[1024];

    if (listen(logger_socket_fd, 5) == -1) {
        perror("Listen error");
        exit(EXIT_FAILURE);
    }

    while (1) {
        if ((c1 = accept(logger_socket_fd, NULL, NULL)) == -1) {
            perror("Accept error");
            continue;
        }

        while ((rc = read(c1, buf, sizeof(buf))) > 0) {
            // fprintf(stdout, "Read %u bytes: %.*s\n", rc, rc, buf);
            // This is where the syslog logic will go
            syslog(LOG_INFO, "%.*s", rc, buf);
        }

        if (rc == -1) {
            perror("Read error");
            continue;
        } else if (rc == 0) {
            close(c1);
        }
    }
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

/* Shutdown function */
void logger_shutdown() {
    char* s = logger_get_socket();
    if (s != NULL) {
        unlink(s);
        free(s);
    }
    logger_cleanup();
}

/* Cleanup function */
void logger_cleanup() {
    if (logger_identifier != NULL) free(logger_identifier);
    if (logger_socket != NULL) free(logger_socket);
    closelog();
}
