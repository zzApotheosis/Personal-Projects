#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <openssl/evp.h>
#include <openssl/rand.h>

//int app(int argc, char * argv[])
//{
//  for (int i = 0; i < argc; i++)
//    fprintf(stderr, " [%d]: %s\n", i, argv[i]);
//  return EXIT_SUCCESS;
//}

/**
 * Basic PBKDF2 Example using OpenSSL C API
 * 
 * This example demonstrates how to derive a key from a password
 * using PBKDF2 (Password-Based Key Derivation Function 2).
 * 
 * Compile: gcc -o pbkdf2_example pbkdf2_example.c -lcrypto
 * Run: ./pbkdf2_example
 */

static void print_hex(const unsigned char * const data, const size_t len) {
    for (size_t i = 0; i < len; i++) {
        printf("%02x", data[i]);
    }
    printf("\n");
}

int app(int argc, char * argv[]) {
    // Input parameters
    const char * password = "my_secret_password";
    const char * salt = "random_salt_value";  // In production, use random bytes
    int iterations = 10000;                   // Number of iterations
    int key_length = 32;                      // Desired key length in bytes (256 bits)
    
    // Buffer for the derived key
    unsigned char derived_key[32];
    
    printf("=== PBKDF2 Key Derivation Example ===\n\n");
    
    // Display input parameters
    printf("Password:    %s\n", password);
    printf("Salt:        %s\n", salt);
    printf("Iterations:  %d\n", iterations);
    printf("Key Length:  %d bytes (%d bits)\n\n", key_length, key_length * 8);
    
    // Derive the key using PKCS5_PBKDF2_HMAC
    // Parameters:
    //   - password: the password string
    //   - password_len: length of password (or -1 for null-terminated)
    //   - salt: the salt bytes
    //   - salt_len: length of salt
    //   - iterations: number of iterations
    //   - digest: hash function to use (SHA-256)
    //   - key_length: desired output key length
    //   - derived_key: output buffer for the derived key
    int result = PKCS5_PBKDF2_HMAC(
        password,           // Password
        strlen(password),   // Password length
        (const unsigned char *)salt,  // Salt
        strlen(salt),       // Salt length
        iterations,         // Iteration count
        EVP_sha256(),       // Hash function (SHA-256)
        key_length,         // Derived key length
        derived_key         // Output buffer
    );
    
    if (result != 1) {
        fprintf(stderr, "Error: PBKDF2 key derivation failed\n");
        return 1;
    }
    
    // Print the derived key in hexadecimal
    printf("Derived Key (hex): ");
    print_hex(derived_key, key_length);
    
    printf("\nKey derivation successful!\n");
    
    return 0;
}
