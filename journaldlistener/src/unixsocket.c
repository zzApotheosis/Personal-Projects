#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/un.h>

char* socket_path = "socket";
// char* socket_path = "\0hidden";

void unixsocket_example() {
    struct sockaddr_un addr;
    char buf[100];
    int fd, c1, rc;

    // if (argc > 1) socket_path = argv[1];

    if ((fd = socket(AF_UNIX, SOCK_STREAM, 0)) == -1) {
        perror("Socket error");
        return;
    }

    memset(&addr, 0, sizeof(addr));
    addr.sun_family = AF_UNIX;
    if (*socket_path == '\0') {
        *addr.sun_path = '\0';
        strncpy(addr.sun_path + 1, socket_path + 1, sizeof(addr.sun_path) - 2);
    } else {
        strncpy(addr.sun_path, socket_path, sizeof(addr.sun_path) - 1);
        unlink(socket_path);
    }

    if (bind(fd, (struct sockaddr*) &addr, sizeof(addr)) == -1) {
        perror("Bind error");
        return;
    }

    if (listen(fd, 5) == -1) {
        perror("Listen error");
        return;
    }

    while (1) {
        if ((c1 = accept(fd, NULL, NULL)) == -1) {
            perror("Accept error");
            return;
        }

        while ((rc = read(c1, buf, sizeof(buf))) > 0) {
            fprintf(stdout, "Read %u bytes: %.*s\n", rc, rc, buf);
        }
        if (rc == -1) {
            perror("Read error");
            return;
        } else if (rc == 0) {
            fprintf(stdout, "EOF\n");
            close(c1);
        }
    }

    return;
}
