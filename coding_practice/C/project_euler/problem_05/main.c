#include <stdlib.h>
#include <stdio.h>
#include "euclid.h"

int main(int argc, char** argv) {
    // Define method variables
    int exitCode = 0;
    int num = 1;
    
    // Solve for answer
    for (unsigned long long i = 1; i <= 20; i++) {
        num = lcm(num, i);
    }
    
    // Print result
    fprintf(stdout, "%d\n", num);
    
    // Done
    return exitCode;
}

