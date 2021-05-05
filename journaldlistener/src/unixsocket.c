#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/un.h>

char* socket_path = "socket";
// char* socket_path = "\0hidden";

void unixsocket_example() {
    struct sockaddr_un addr; // sockaddr_un is a UNIX socket address
    char buf[100];
    int fd = 0; // fd = file descriptor
    int c1 = 0; // c1 = connection
    int rc = 0; // rc = read count

    // if (argc > 1) socket_path = argv[1];

    if ((fd = socket(AF_UNIX, SOCK_STREAM, 0)) == -1) {
        perror("Socket error");
        return;
    }

    memset(&addr, 0, sizeof(addr));
    addr.sun_family = AF_UNIX;
    if (*socket_path == '\0') {
        // For a hidden Unix socket
        *addr.sun_path = '\0';
        strncpy(addr.sun_path + 1, socket_path + 1, sizeof(addr.sun_path) - 2);
    } else {
        // For an actual socket file
        strncpy(addr.sun_path, socket_path, sizeof(addr.sun_path) - 1);
        unlink(socket_path);
    }

    // Here, &addr must be casted as a struct sockaddr* because it is actually a sockaddr_un type. See above
    if (bind(fd, (struct sockaddr*) &addr, sizeof(addr)) == -1) {
        perror("Bind error");
        return;
    }

    // Begin listening on the file descriptor for the Unix socket, with backlog = 5
    if (listen(fd, 5) == -1) {
        perror("Listen error");
        return;
    }

    // Begin main loop
    while (1) {
        // Accept connection
        if ((c1 = accept(fd, NULL, NULL)) == -1) {
            perror("Accept error");
            return;
        }

        // Begin reading from the connection, once read() returns 0 bytes, the connection is closed
        while ((rc = read(c1, buf, sizeof(buf))) > 0) {
            //fprintf(stdout, "Read %u bytes: %.*s\n", rc, rc, buf);

            // Echo the input back to the client
            if (write(c1, buf, rc) == -1) {
                perror("Write error");
                return;
            }
        }

        // Check for errors
        if (rc == -1) {
            perror("Read error");
            return;
        } else if (rc == 0) {
            // If rc is 0, the client has successfully disconnected
            fprintf(stdout, "EOF\n");
            close(c1);
        }
    }

    // This will basically never happen
    return;
}
