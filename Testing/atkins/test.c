#include <stdio.h>
#include <stdlib.h>
#include "sieve.h"

int main(int argc, char** argv) {
    // Define method variables
    int exitCode = 0;
    unsigned char* temp;
    unsigned long int limit = 10000;
    
    // Test
    temp = satkins(limit);
    for (int i = 0; i < limit; i++) {
        if (temp[i]) {
            fprintf(stdout, "%d is prime\n", i);
        }
    }
    
    // Clean up
    free(temp);
    
    // Done
    return exitCode;
}

