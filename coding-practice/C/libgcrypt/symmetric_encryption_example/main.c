#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <gcrypt.h>

#define NEED_LIBGCRYPT_VERSION "1.10.1"

void printhex(int arr_len, unsigned char arr[arr_len], char name[]) {
    fprintf(stdout, "[*] Printing array %s, length=%d\n", name, arr_len);
    fprintf(stdout, "    0x%02x ", arr[0]);
    for (size_t i = 1; i < arr_len - 1; i++) {
        fprintf(stdout, "0x%02x ", arr[i]);
    }
    fprintf(stdout, "0x%02x\n", arr[arr_len - 1]);
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
    // Declare function variables
    gpg_error_t err = 0;
    unsigned char str[] = "lets try a new password!";
    unsigned char salt[8] = {
        0xea, 0x22, 0xb1, 0xc9,
        0x6d, 0xb9, 0x8e, 0xab
    }; // This is a sample Salt. Ideally this should be randomly generated and discarded once the key has been derived/generated
    unsigned char key[32]; memset(key, 0, sizeof(key));
    gcry_cipher_hd_t hd = 0;
    unsigned char iv[16] = {
        0x19, 0x89, 0x24, 0x1c,
        0xbd, 0x79, 0x97, 0x40,
        0xdf, 0x58, 0xef, 0x84,
        0x49, 0x7f, 0x9c, 0x2d
    }; // This is a sample Initialization Vector. Ideally this is supposed to be randomly generated and stored alongside the ciphertext
    unsigned char plaintext[16] = "Hello!";
    unsigned char ciphertext[16]; memset(ciphertext, 0, sizeof(ciphertext));
    unsigned char decrypted[16]; memset(decrypted, 0, sizeof(decrypted));
    
    // Perform initialization
    init();
    
    // Derive a key based on a string (KDF)
    err = gcry_kdf_derive(str, sizeof(str), GCRY_KDF_ITERSALTED_S2K, GCRY_MD_SHA3_512, salt, 8, 10, sizeof(key), key);
    if (err) {
        fprintf(stderr, "GOT ERROR ON gcry_kdf_derive\n");
        exit(err);
    }
    printhex(sizeof(key), key, "derived key");
    
    // Open a new cipher handle
    err = gcry_cipher_open(&hd, GCRY_CIPHER_AES256, GCRY_CIPHER_MODE_GCM, GCRY_CIPHER_SECURE);
    if (err) {
        fprintf(stderr, "GOT ERROR ON gcry_cipher_open\n");
        exit(err);
    }
    
    // Set the symmetric key
    err = gcry_cipher_setkey(hd, key, sizeof(key));
    if (err) {
        fprintf(stderr, "GOT ERROR ON gcry_cipher_setkey\n");
        exit(err);
    }
    
    // Set the Initialization Vector
    err = gcry_cipher_setiv(hd, iv, sizeof(iv));
    if (err) {
        fprintf(stderr, "GOT ERROR ON gcry_cipher_setiv\n");
        exit(err);
    }
    
    // Perform encryption
    //gcry_cipher_final(hd); // This is used only for certain cipher modes
    err = gcry_cipher_encrypt(hd, ciphertext, sizeof(ciphertext), plaintext, sizeof(plaintext));
    if (err) {
        fprintf(stderr, "GOT ERROR ON gcry_cipher_encrypt\n");
        exit(err);
    }
    
    // Reset the cipher handle
    err = gcry_cipher_reset(hd);
    if (err) {
        fprintf(stderr, "GOT ERROR ON gcry_cipher_reset\n");
        exit(err);
    }

    // Set the same Initialization Vector
    err = gcry_cipher_setiv(hd, iv, sizeof(iv));
    if (err) {
        fprintf(stderr, "GOT ERROR ON 2ND gcry_cipher_setiv\n");
        exit(err);
    }
    
    // Perform decryption
    err = gcry_cipher_decrypt(hd, decrypted, sizeof(decrypted), ciphertext, sizeof(ciphertext));
    if (err) {
        fprintf(stderr, "GOT ERROR ON gcry_cipher_decrypt\n");
        exit(err);
    }
    
    // Show results
    fprintf(stdout, "[*] Passphrase string: %s\n", str);
    fprintf(stdout, "[*] Original text: %s\n", plaintext);
    printhex(sizeof(plaintext), plaintext, "plaintext");
    printhex(sizeof(ciphertext), ciphertext, "ciphertext");
    fprintf(stdout, "[*] Decrypted text: %s\n", decrypted);
    printhex(sizeof(decrypted), decrypted, "decrypted");
    
    return(EXIT_SUCCESS);
}

