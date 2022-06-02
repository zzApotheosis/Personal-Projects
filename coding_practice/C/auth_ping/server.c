#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <stdint.h>
#include <gcrypt.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>

#define NEED_LIBGCRYPT_VERSION "1.10.1"
#define PORT 41454
#define MAXLINE 1024

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
    unsigned long key_len;
    unsigned char * key = read_file("test.key", &key_len); // Read 32 bytes of random data; alternatively, you could use a rand() function call for a quick and dirty way to generate 32 pseudo-random bytes
    unsigned char data[1024];
    memset(data, 0, sizeof(data));
    if (key_len != 32lu) {
        fprintf(stderr, "The key must be 32 bytes long\n");
        return(EXIT_FAILURE);
    }
    unsigned char mac[32];
    memset(mac, 0, sizeof(mac));
    int sockfd;
    char buffer[MAXLINE];
    char * hello = "Hello from server";
    struct sockaddr_in servaddr, cliaddr;
    
    // Initialize libgcrypt
    init();
    
    // DO UDP STUFF HERE
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
    len = sizeof(cliaddr);
    while (1) {
        memset(buffer, 0, sizeof(buffer));
        n = recvfrom(sockfd, buffer, MAXLINE, MSG_WAITALL, (struct sockaddr *) &cliaddr, &len);
        gcry_randomize(data, sizeof(data), GCRY_VERY_STRONG_RANDOM);
        generate_mac(sizeof(data), data, key_len, key, sizeof(mac), mac);
        sendto(sockfd, data, sizeof(data), MSG_CONFIRM, (const struct sockaddr *) &cliaddr, len);
        fprintf(stdout, "Challenged the client with random data\n");

        memset(buffer, 0, sizeof(buffer));
        n = recvfrom(sockfd, buffer, MAXLINE, MSG_WAITALL, (struct sockaddr *) &cliaddr, &len);
        fprintf(stdout, "Our MAC vs. Their MAC\n");
        for (int i = 0; i < sizeof(mac); i++) {
            fprintf(stdout, "%02x ", mac[i]);
        }
        fprintf(stdout, "\n");
        for (int i = 0; i < sizeof(mac); i++) {
            fprintf(stdout, "%02x ", buffer[i]);
        }
        fprintf(stdout, "\n");
        sendto(sockfd, "end", 4, MSG_CONFIRM, (const struct sockaddr *) &cliaddr, len);
    }
    
    close(sockfd);
    return(EXIT_SUCCESS);
}
