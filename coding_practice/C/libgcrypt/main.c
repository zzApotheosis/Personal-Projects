#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <gcrypt.h>
#include "main.h"

int main(int argc, char **argv) {
    // Define function variables
    int status = 0;
    char passphrase[] = "TESTKEY                  ";
    char *key = NULL;
    int keysize = 0;
    int blklen = 0;
    size_t size_of_crypt = 0;
    gcry_error_t e = 0;
    gcry_cipher_hd_t hd = 0;
    char *out = NULL;
    char *plaintext = NULL;
    char *deout = NULL;

    // Initialize libgcrypt
    e = init_gcry();
    if (e) {
        fprintf(stderr, "Error (%d) initializing libgcrypt\n", e);
        return(e);
    }

    // Initialize variables for real
    keysize = gcry_cipher_get_algo_keylen(GCRY_CIPHER_AES256);
    blklen = gcry_cipher_get_algo_blklen(GCRY_CIPHER_AES256);
    size_of_crypt = sizeof(char) * 2 * blklen;
    out = malloc(size_of_crypt);
    deout = malloc(size_of_crypt);
    plaintext = malloc(blklen);
    memset(plaintext, 0, blklen);
    memset(out, 0, size_of_crypt);
    memset(deout, 0, size_of_crypt);

    // BEGIN SHIT
    fprintf(stdout, "Key size: %d\n", keysize);
    fprintf(stdout, "Block length: %d\n", blklen);
    fprintf(stdout, "size_of_crypt: %d\n", size_of_crypt);
    
    strncpy(plaintext, "Secret Text", blklen);
    plaintext[blklen - 1] = 0;

    e = gcry_cipher_open(&hd, GCRY_CIPHER_AES256, GCRY_CIPHER_MODE_CBC, 0);
    if (e) {
        fprintf(stderr, "Error (%d) opening cipher handle\n", e);
        return(e);
    }

    e = gcry_cipher_setkey(hd, key, keysize);
    if (e) {
        fprintf(stderr, "Error (%d) setting cipher key\n", e);
        return(e);
    }

    e = gcry_cipher_encrypt(hd, (unsigned char *) out, size_of_crypt, (const unsigned char *) plaintext, blklen);
    if (e) {
        fprintf(stderr, "Error (%d) on gcry_cipher_encrypt()\n", e);
        return(e);
    }

    e = gcry_cipher_setkey(hd, key, keysize);
    if (e) {
        fprintf(stderr, "Error (%d) on gcry_cipher_setkey()\n", e);
        return(e);
    }

    e = gcry_cipher_decrypt(hd, (unsigned char *) deout, size_of_crypt, (const unsigned char *) out, blklen);
    if (e) {
        fprintf(stderr, "Error (%d) on gcry_cipher_decrypt()\n", e);
        return(e);
    }

    fprintf(stdout, "Plaintext: (len=%d) '%s'\n", strlen(plaintext), plaintext);
    fprintf(stdout, "Ciphertext: ");
    printhex(out, size_of_crypt);
    fprintf(stdout, "\n");
    fprintf(stdout, "Decrypted text: %s\n", deout);

    free(plaintext);
    free(out);
    free(deout);
    gcry_cipher_close(hd);

    return EXIT_SUCCESS;
}

gcry_error_t init_gcry() {
    gcry_error_t e = 0;
    e = version_check();
    if (e) {
        return(e);
    }
    gcry_control(GCRYCTL_SUSPEND_SECMEM_WARN);
    gcry_control(GCRYCTL_INIT_SECMEM, 16384, 0);
    gcry_control(GCRYCTL_RESUME_SECMEM_WARN);
    gcry_control(GCRYCTL_INITIALIZATION_FINISHED, 0);
    return(e);
}

gcry_error_t version_check() {
    if (!gcry_check_version(NEED_LIBGCRYPT_VERSION)) {
        fprintf(stderr, "libgcrypt is too old (need %s, have %s)\n", NEED_LIBGCRYPT_VERSION, gcry_check_version(NULL));
        return(2);
    } else {
        return(0);
    }
}

int printhex(void* ptr, int l) {
    int aa;
    char* p = (char*) ptr;
    for (aa = 0; aa < l; aa++) {
        fprintf(stdout, "%02x ", p[aa] & 0xff);
    }
}

