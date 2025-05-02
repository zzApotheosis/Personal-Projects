#include <stdio.h>
#include <stdlib.h>
#include "sieve.h"

int main(int argc, char** argv) {
    // Define method variables
    unsigned char* temp = NULL;
    unsigned long int limit = 500000lu;
    unsigned long int counter = 0lu;
    unsigned long int prime = 0lu;
    
    // Test
    temp = satkins(limit);
    for (int i = 0; i < limit; i++) {
        if (temp[i]) {
            counter++;
            if (counter == 10001) {
                prime = i;
                break;
            }
        }
    }

    fprintf(stdout, "%lu\n", prime);
    
    // Clean up
    free(temp);
    
    // Done
    return EXIT_SUCCESS;
}

