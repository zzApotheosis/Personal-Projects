#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>

#define PORT 41454
#define MAXLINE 1024

int main(int argc, char ** argv) {
    int sockfd;
    char buffer[MAXLINE];
    char * hello = "Hello from client";
    struct sockaddr_in servaddr;
    
    if ((sockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
        fprintf(stdout, "line %d: Error caught\n", __LINE__);
        abort();
    }
    
    memset(&servaddr, 0, sizeof(servaddr));
    
    servaddr.sin_family = AF_INET;
    servaddr.sin_port = htons(PORT);
    //servaddr.sin_addr.s_addr = INADDR_ANY;
    servaddr.sin_addr.s_addr = inet_addr("127.0.0.1"); // Change this to the server IP
    
    int n, len;
    sendto(sockfd, (const char *) hello, strlen(hello), MSG_CONFIRM, (const struct sockaddr *) &servaddr, sizeof(servaddr));
    n = recvfrom(sockfd, buffer, MAXLINE, MSG_WAITALL, (struct sockaddr *) &servaddr, &len);
    fprintf(stdout, "Client received message from server: %s\n", buffer);
    
    close(sockfd);
    
    return(EXIT_SUCCESS);
}
