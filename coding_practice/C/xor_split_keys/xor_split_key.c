#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <stdint.h>
#include <time.h>
#include <string.h>

#define KEY_LEN 32
#define N_PARTS 3

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
    
    // Initialize RNG
    srand((unsigned int) time(NULL));

    // Perform derivation of split keys
    for (size_t i = 0; i < key_len; i++) {
        // Initialize the 1st split key as the original key
        split_keys[0][i] = original_key[i];
    }
    for (uint32_t i = 1; i < n_parts; i++) {
        for (size_t j = 0; j < key_len; j++) {
            split_keys[i][j] = (uint8_t) rand();
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

int main(int argc, char ** argv) {
    // Declare a test key, a split key multidimensional array, and a derived key
    uint8_t original_key[KEY_LEN] = {
        0x33, 0xb8, 0x74, 0x84,
        0x83, 0x88, 0xb4, 0x4a,
        0x27, 0x22, 0x21, 0xcb,
        0x21, 0x18, 0x53, 0x9a,
        0x54, 0x5d, 0x43, 0x5e,
        0x9c, 0x28, 0xec, 0x3e,
        0x55, 0xa8, 0x31, 0x9d,
        0xf7, 0xea, 0xe9, 0x1a
    };
    uint8_t split_keys[N_PARTS][KEY_LEN];
    memset(split_keys, 0, N_PARTS * KEY_LEN);
    uint8_t derived_key[KEY_LEN];
    memset(derived_key, 0, KEY_LEN);

    // Show that our original key is clearly defined in memory
    fprintf(stdout, "[*] HERE IS OUR ORIGINAL KEY AS DEFINED\n");
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
        printhex(split_keys[i], KEY_LEN, "split_key");
    }
    fprintf(stdout, "[*] HERE IS OUR DERIVED KEY. MAKE SURE IT MATCHES THE ORIGINAL KEY!!!\n");
    printhex(derived_key, KEY_LEN, "derived_key");

    // Done
    return(EXIT_SUCCESS);
}
