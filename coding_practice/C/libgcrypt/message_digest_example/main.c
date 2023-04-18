#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <stdint.h>
#include <gcrypt.h>
#include <sys/stat.h>

#define NEED_LIBGCRYPT_VERSION "1.10.1"

unsigned char * read_file(const char fn[], unsigned long * data_len) {
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
    
    unsigned long file_size = 0lu;
    unsigned char * file_data = read_file(argv[1], &file_size);
    int selected_hash_algorithm = GCRY_MD_SHA256;
    unsigned char * digest;
    unsigned char buffer[32]; // Buffer to copy the digest into; change this length to accomodate other hash algorithms
    gcry_error_t err;
    gcry_md_hd_t md;
    
    // Check if file was correctly opened for reading
    if (file_data == NULL) {
        fprintf(stderr, "line %d: Unable to open file %s\n", __LINE__, argv[1]);
        abort();
    }
    
    // Initialize libgcrypt
    init();
    
    // Perform SHA256 digest
    fprintf(stdout, "Input file: %s\n", argv[1]);
    err = gcry_md_open(&md, selected_hash_algorithm, 0);
    if (err) {
        fprintf(stderr, "Error %d on line %d\n", gcry_err_code(err), __LINE__);
        abort();
    }
    gcry_md_write(md, file_data, file_size);
    digest = gcry_md_read(md, 0);
    memcpy(buffer, digest, sizeof(buffer));
    gcry_md_close(md);
    
    // Display hex-encoded message digest
    fprintf(stdout, "Message digest (%s): ", gcry_md_algo_name(selected_hash_algorithm));
    for (int i = 0; i < sizeof(buffer); i++) {
        fprintf(stdout, "%02x", buffer[i]);
    }
    fprintf(stdout, "\n");
    
    free(file_data);
    return(EXIT_SUCCESS);
}
