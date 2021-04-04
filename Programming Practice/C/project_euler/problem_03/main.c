#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv) {
    // Define method variables
    int exitCode = 0;
    unsigned long long n = 600851475143ull;
    unsigned long long i;

    // Find prime
    for (i = 2ull; i < n; i++) {
        while (n % i == 0) {
            n /= i;
        }
    }
    
    // Print result
    fprintf(stdout, "%llu\n", n);
    
    // Done
    return exitCode;
}

