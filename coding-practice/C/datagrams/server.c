#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <stdint.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <string.h>
#include <sys/wait.h>

#define PORT 41454 // Port randomly selected between 1-65536
#define MAXLINE 1024

const char hello[] = "Hello from server!";

void handle_client(int sockfd, const char msg[], const size_t msg_len, const struct sockaddr_in client) {
    int len = sizeof(client);
    sendto(sockfd, msg, msg_len, MSG_CONFIRM, (const struct sockaddr *) &client, len);
    close(sockfd);
    exit(EXIT_SUCCESS); // CHILD EXIT
}

int main(int argc, char ** argv) {
    int sockfd;
    char buffer[MAXLINE];
    struct sockaddr_in servaddr, cliaddr;
    
    if ((sockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
        fprintf(stdout, "line %d: Error caught\n", __LINE__);
        abort();
    }

    memset(&servaddr, 0, sizeof(servaddr));
    memset(&cliaddr, 0, sizeof(cliaddr));

    servaddr.sin_family = AF_INET; // IPv4
    servaddr.sin_addr.s_addr = INADDR_ANY;
    servaddr.sin_port = htons(PORT);

    if (bind(sockfd, (const struct sockaddr *) &servaddr, sizeof(servaddr)) < 0) {
        fprintf(stdout, "line %d: Error caught\n", __LINE__);
        abort();
    }

    int len, n;
    int pid;
    len = sizeof(cliaddr);
    while (1) {
        memset(buffer, 0, sizeof(buffer));
        n = recvfrom(sockfd, buffer, MAXLINE, MSG_WAITALL, (struct sockaddr *) &cliaddr, &len);
        buffer[sizeof(buffer) - 1] = '\0';
        fprintf(stdout, "Server received message from client: %s\n", buffer);
        pid = fork();
        if (pid == 0) {
            // Let the child handle this client
            handle_client(sockfd, buffer, n, cliaddr);
        }
    }
    
    close(sockfd);
    return(EXIT_SUCCESS);
}
