#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <stdint.h>
#include <string.h>
#include <arpa/inet.h>
#include <sys/socket.h>

#define PORT 8080

int main(int argc, char ** argv) {
    int sock = 0, valread, client_fd;
    struct sockaddr_in serv_addr;
    char * hello = "Hello from client";
    char buffer[1024] = {0};
    if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
        fprintf(stderr, "Socket creation error\n");
        abort();
    }

    serv_addr.sin_family = AF_INET;
    serv_addr.sin_port = htons(PORT);

    // Convert IPv4 and IPv6 addresses from text to binary form
    if (inet_pton(AF_INET, "127.0.0.1", &serv_addr.sin_addr) <= 0) {
        fprintf(stderr, "Invalid address/ Address not supported\n");
        abort();
    }

    if ((client_fd = connect(sock, (struct sockaddr *) & serv_addr, sizeof(serv_addr))) < 0) {
        fprintf(stderr, "Connection failed\n");
        abort();
    }
    send(sock, hello, strlen(hello), 0);
    fprintf(stdout, "Hello message sent\n");
    valread = read(sock, buffer, 1024);
    fprintf(stdout, "%s\n", buffer);

    close(client_fd);
    return(EXIT_SUCCESS);
}
