#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <stdint.h>
#include <string.h>
#include <netinet/in.h>
#include <sys/socket.h>

#define PORT 8080

int main(int argc, char ** argv) {
    int server_fd, new_socket, valread;
    struct sockaddr_in address;
    int opt = 1;
    int addrlen = sizeof(address);
    char buffer[1024] = {0};
    char * hello = "Hello from server";

    // Create socket file descriptor
    if ((server_fd = socket(AF_INET, SOCK_STREAM, 0)) == 0) {
        fprintf(stderr, "socket failed\n");
        abort();
    }

    // Forcefully attaching socket to the port 8080
    if (setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR | SO_REUSEPORT, &opt, sizeof(opt))) {
        fprintf(stderr, "setsockopt\n");
        abort();
    }
    address.sin_family = AF_INET;
    address.sin_addr.s_addr = INADDR_ANY;
    address.sin_port = htons(PORT);

    // Forcefully attaching socket to the port 8080
    if (bind(server_fd, (struct sockaddr *) &address, sizeof(address)) < 0) {
        fprintf(stderr, "bind failed\n");
        abort();
    }
    if (listen(server_fd, 3) < 0) {
        fprintf(stderr, "listen\n");
        abort();
    }
    if ((new_socket = accept(server_fd, (struct sockaddr *) &address, (socklen_t *) &addrlen)) < 0) {
        fprintf(stderr, "accept\n");
        abort();
    }
    valread = read(new_socket, buffer, 1024);
    fprintf(stdout, "%s\n", buffer);
    send(new_socket, hello, strlen(hello), 0);
    fprintf(stdout, "Hello message sent\n");

    // Close connection
    close(new_socket);
    shutdown(server_fd, SHUT_RDWR);
    
    return(EXIT_SUCCESS);
}
