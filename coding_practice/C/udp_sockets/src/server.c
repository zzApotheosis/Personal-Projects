#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <stdint.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/wait.h>
#include <string.h>

#define PORT 41454 // Port randomly selected between 1-65536
#define MAXLINE 1024

int main(int argc, char ** argv) {
    int sockfd;
    unsigned char buffer[MAXLINE];
    unsigned char * hello = "Hello from server";
    struct sockaddr_in servaddr, cliaddr;
    
    // DO UDP STUFF HERE
    if ((sockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
        perror("socket error");
        abort();
    }

    memset(&servaddr, 0, sizeof(servaddr));
    memset(&cliaddr, 0, sizeof(cliaddr));

    servaddr.sin_family = AF_INET; // IPv4
    servaddr.sin_addr.s_addr = INADDR_ANY;
    servaddr.sin_port = htons(PORT);

    if (bind(sockfd, (const struct sockaddr *) &servaddr, sizeof(servaddr)) < 0) {
        perror("Bind error");
        abort();
    }

    int len, n;
    len = sizeof(cliaddr);
    while (1) {
        memset(buffer, 0, sizeof(buffer));
        n = recvfrom(sockfd, buffer, MAXLINE, MSG_WAITALL, (struct sockaddr *) &cliaddr, &len);
        sendto(sockfd, hello, strlen(hello), MSG_CONFIRM, (const struct sockaddr *) &cliaddr, sizeof(struct sockaddr));
    }
    
    close(sockfd);
    return(EXIT_SUCCESS);
}
