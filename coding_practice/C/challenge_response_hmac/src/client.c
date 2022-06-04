#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <gcrypt.h>

#define NEED_LIBGCRYPT_VERSION "1.10.1"
#define PORT 41454
#define MAXLINE 1024

unsigned long key_len;
unsigned char * key;

unsigned char * read_file(const unsigned char fn[], unsigned long * data_len) {
    if (fn == NULL) {
        fprintf(stderr, "Error on line %d\n", __LINE__);
        return(NULL);
    }
    FILE * fh = fopen(fn, "rb");
    if (fh == NULL) {
        fprintf(stderr, "Error opening file %s\n", fn);
        return(NULL);
    }
    struct stat s;
    if (fstat(fileno(fh), &s) != 0) {
        fprintf(stderr, "Error on line %d\n", __LINE__);
        fclose(fh);
        return(NULL);
    }
    if (data_len != NULL) {
        *data_len = s.st_size;
    }
    unsigned char * file_data = (unsigned char *) malloc(s.st_size * sizeof(unsigned char));
    fread(file_data, sizeof(unsigned char), s.st_size, fh);
    fclose(fh);
    return(file_data);
}

void init() {
    if (!gcry_check_version(NEED_LIBGCRYPT_VERSION)) {
        fprintf(stderr, "libgcrypt is too old (need %s, have %s)\n", NEED_LIBGCRYPT_VERSION, gcry_check_version(NULL));
        exit(2);
    }
    gcry_control(GCRYCTL_SUSPEND_SECMEM_WARN);
    gcry_control(GCRYCTL_INIT_SECMEM, 16384, 0);
    gcry_control(GCRYCTL_RESUME_SECMEM_WARN);
    gcry_control(GCRYCTL_INITIALIZATION_FINISHED);
}

void generate_mac(size_t data_len, unsigned char data[data_len], size_t key_len, unsigned char key[key_len], size_t buffer_len, unsigned char buffer[buffer_len]) {
    int selected_mac_algorithm = GCRY_MAC_HMAC_SHA256;
    unsigned char mac[32];
    size_t mac_len = 32;
    gcry_error_t err;
    gcry_mac_hd_t hd;
    
    err = gcry_mac_open(&hd, selected_mac_algorithm, GCRY_MAC_FLAG_SECURE, NULL);
    if (err) {
        fprintf(stdout, "line %d: Error code %d caught\n", __LINE__, gcry_err_code(err));
        abort();
    }
    err = gcry_mac_setkey(hd, key, key_len);
    if (err) {
        fprintf(stdout, "line %d: Error code %d caught\n", __LINE__, gcry_err_code(err));
        abort();
    }
    gcry_mac_write(hd, data, data_len);
    err = gcry_mac_read(hd, mac, &mac_len);
    if (err) {
        fprintf(stdout, "line %d: Error code %d caught\n", __LINE__, gcry_err_code(err));
        abort();
    }
    memcpy(buffer, mac, buffer_len);
    gcry_mac_close(hd);
}

int main(int argc, char ** argv) {
    int sockfd;
    char buffer[MAXLINE];
    struct sockaddr_in servaddr;
    unsigned char mac[32];
    key = read_file("client.key", &key_len);

    init();

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

    sendto(sockfd, "", 1, MSG_CONFIRM, (const struct sockaddr *) &servaddr, sizeof(servaddr));
    //fprintf(stdout, "Hello message sent.\n");

    n = recvfrom(sockfd, buffer, MAXLINE, MSG_WAITALL, (struct sockaddr *) &servaddr, &len);
    fprintf(stdout, "n = %d\n", n);
    
    generate_mac(MAXLINE, buffer, key_len, key, sizeof(mac), mac);
    //mac[0]++; // Test sending back a bad MAC
    sendto(sockfd, mac, sizeof(mac), MSG_CONFIRM, (const struct sockaddr *) &servaddr, sizeof(servaddr));
    memset(buffer, 0, sizeof(buffer));
    n = recvfrom(sockfd, buffer, MAXLINE, MSG_WAITALL, (struct sockaddr *) &servaddr, &len);
    fprintf(stdout, "%s\n", buffer);

    close(sockfd);

    return(EXIT_SUCCESS);
}
