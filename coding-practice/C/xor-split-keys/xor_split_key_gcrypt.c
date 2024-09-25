/*
 * Compile this program with compiler flags:
 * -lgcrypt -lgpg-error
 */
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <stdint.h>
#include <time.h>
#include <string.h>
#include <gcrypt.h>

#define NEED_LIBGCRYPT_VERSION "1.10.0"

#define KEY_LEN 32
#define N_PARTS 10

void printhex(uint8_t arr[], size_t arr_len, char arr_name[]) {
    fprintf(stdout, "    Printing array %s, length=%d\n", arr_name, (int) arr_len);
    for (size_t i = 0; i < arr_len; i++) {
        if (i == 0) {
            fprintf(stdout, "    0x%02x ", arr[i]);
        } else if (i == arr_len - 1) {
            fprintf(stdout, "0x%02x\n", arr[i]);
        } else if (i > 0 && i < arr_len - 1) {
            fprintf(stdout, "0x%02x ", arr[i]);
        } else {
            fprintf(stderr, "wtf bro\n");
        }
    }
}

void split_key(const size_t key_len, const uint32_t n_parts, const uint8_t original_key[key_len], uint8_t split_keys[n_parts][key_len]) {
    // Perform checks to ensure we have good data to work with
    if (key_len <= 0 || n_parts < 2 || original_key == NULL || split_keys == NULL) {
        fprintf(stderr, "wtf bro\n");
        return;
    }

    // Perform derivation of split keys
    for (size_t i = 0; i < key_len; i++) {
        // Initialize the 1st split key as the original key
        split_keys[0][i] = original_key[i];
    }
    for (uint32_t i = 1; i < n_parts; i++) {
        gcry_randomize((unsigned char *) split_keys[i], key_len, GCRY_VERY_STRONG_RANDOM);
        for (size_t j = 0; j < key_len; j++) {
            split_keys[0][j] ^= split_keys[i][j];
        }
    }
}

void join_keys(const size_t key_len, const uint32_t n_parts, const uint8_t split_keys[n_parts][key_len], uint8_t output_key[key_len]) {
    // Perform checks to ensure we have good data to work with
    if (key_len <= 0 || n_parts < 2 || output_key == NULL || split_keys == NULL) {
        fprintf(stderr, "wtf bro\n");
        return;
    }

    // Perform joining of split keys
    // output_key must start zeroized
    memset(output_key, 0, key_len);
    for (uint32_t i = 0; i < n_parts; i++) {
        for (size_t j = 0; j < key_len; j++) {
            output_key[j] ^= split_keys[i][j];
        }
    }
}

void gcrypt_initialize() {
    if (!gcry_check_version(NEED_LIBGCRYPT_VERSION)) {
        fprintf(stderr, "libgcrypt is too old (need %s, have %s)\n", NEED_LIBGCRYPT_VERSION, gcry_check_version(NULL));
        exit(2);
    }
    gcry_control(GCRYCTL_SUSPEND_SECMEM_WARN);
    gcry_control(GCRYCTL_INIT_SECMEM, 16384, 0);
    gcry_control(GCRYCTL_RESUME_SECMEM_WARN);
    gcry_control(GCRYCTL_INITIALIZATION_FINISHED);
}

void dump_to_file(size_t len, uint8_t data[len], const char filename[]) {
    FILE * fh = fopen(filename, "w+b");
    if (fh == NULL) {
        fprintf(stderr, "CANNOT DUMP DATA TO FILE!\n");
        return;
    } else {
        fwrite(data, sizeof(uint8_t), len, fh);
        fclose(fh);
    }
}

int main(int argc, char ** argv) {
    // Declare a test key, a split key multidimensional array, and a derived key
    uint8_t original_key[KEY_LEN];
    memset(original_key, 0, KEY_LEN);
    uint8_t split_keys[N_PARTS][KEY_LEN];
    memset(split_keys, 0, N_PARTS * KEY_LEN);
    uint8_t derived_key[KEY_LEN];
    memset(derived_key, 0, KEY_LEN);
    char buffer[32]; // Character buffer for strings
    memset(buffer, 0, sizeof(buffer));

    // Initialize gcrypt
    gcrypt_initialize();

    // Generate a secure random key
    gcry_randomize(original_key, KEY_LEN, GCRY_VERY_STRONG_RANDOM);

    // Show that our original key is clearly defined in memory
    fprintf(stdout, "[*] HERE IS OUR ORIGINAL KEY GENERATED BY LIBGCRYPT\n");
    printhex(original_key, KEY_LEN, "original_key");
    fprintf(stdout, "[*] HERE IS OUR ORIGINAL \"DERIVED\" KEY. It should be blank (zeroed)\n");
    printhex(derived_key, KEY_LEN, "derived_key");

    // Pass our original key and split-key array to our custom split_key function to perform split key derivation
    split_key(KEY_LEN, N_PARTS, original_key, split_keys);

    // Use our join_keys function to derive the original key using ONLY the split keys
    join_keys(KEY_LEN, N_PARTS, split_keys, derived_key);

    // Show that our original key HOPEFULLY matches our derived key
    fprintf(stdout, "[*] HERE ARE ALL %d OF OUR SPLIT KEYS\n", N_PARTS);
    for (int i = 0; i < N_PARTS; i++) {
        snprintf(buffer, sizeof(buffer), "split_key_%02d", i);
        printhex(split_keys[i], KEY_LEN, buffer);
    }
    fprintf(stdout, "[*] HERE IS OUR DERIVED KEY. MAKE SURE IT MATCHES THE ORIGINAL KEY!!!\n");
    printhex(derived_key, KEY_LEN, "derived_key");
    
    // Dump all of them to files
    fprintf(stdout, "[*] DUMPING KEYS TO INDIVIDUAL FILES\n");
    dump_to_file(KEY_LEN, original_key, "original.key");
    for (int i = 0; i < N_PARTS; i++) {
        snprintf(buffer, sizeof(buffer), "split%02d.key", i);
        dump_to_file(KEY_LEN, split_keys[i], buffer);
    }
    dump_to_file(KEY_LEN, derived_key, "derived.key");

    // Done
    return(EXIT_SUCCESS);
}
