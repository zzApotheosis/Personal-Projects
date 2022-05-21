#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <stdint.h>
#include <gcrypt.h>
#include <sys/stat.h>

#define NEED_LIBGCRYPT_VERSION "1.10.1"

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

int main(int argc, char ** argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <input file>\n", argv[0]);
        return(EXIT_FAILURE);
    }
    
    uint8_t key[32] = {
        0x00, 0x01, 0x02, 0x03,
        0x04, 0x05, 0x06, 0x07,
        0x08, 0x09, 0x0a, 0x0b,
        0x0c, 0x0d, 0x0e, 0x0f,
        0x10, 0x11, 0x12, 0x13,
        0x14, 0x15, 0x16, 0x17,
        0x18, 0x19, 0x1a, 0x1b,
        0x1c, 0x1d, 0x1e, 0x1f
    }; // Sample key; This should be securely generated and handled with care
    unsigned long file_size = 0lu;
    unsigned char * file_data = read_file(argv[1], &file_size);
    int selected_mac_algorithm = GCRY_MAC_HMAC_SHA256;
    unsigned char mac[32];
    size_t mac_len = 32;
    unsigned char buffer[32]; // Buffer to copy the HMAC into; change this length to accomodate other MAC algorithms
    gcry_error_t err;
    gcry_mac_hd_t hd;
    
    // Check if file was correctly opened for reading
    if (file_data == NULL) {
        fprintf(stderr, "line %d: Unable to open file %s\n", __LINE__, argv[1]);
        abort();
    }
    
    // Initialize libgcrypt
    init();
    
    // Calculate HMAC
    fprintf(stdout, "Input file: %s\n", argv[1]);
    err = gcry_mac_open(&hd, selected_mac_algorithm, GCRY_MAC_FLAG_SECURE, NULL);
    if (err) {
        fprintf(stderr, "line %d: Error code %d caught\n", __LINE__, gcry_err_code(err));
        abort();
    }
    err = gcry_mac_setkey(hd, key, sizeof(key));
    if (err) {
        fprintf(stderr, "line %d: Error code %d caught\n", __LINE__, gcry_err_code(err));
        abort();
    }
    gcry_mac_write(hd, file_data, file_size);
    err = gcry_mac_read(hd, mac, &mac_len);
    if (err) {
        fprintf(stderr, "line %d: Error code %d caught\n", __LINE__, gcry_err_code(err));
        abort();
    }
    memcpy(buffer, mac, sizeof(buffer));
    gcry_mac_close(hd);
    
    // Display hex-encoded HMAC
    fprintf(stdout, "HMAC (%s): ", gcry_mac_algo_name(selected_mac_algorithm));
    for (int i = 0; i < sizeof(buffer); i++) {
        fprintf(stdout, "%02x", buffer[i]);
    }
    fprintf(stdout, "\n");
    
    free(file_data);
    return(EXIT_SUCCESS);
}
