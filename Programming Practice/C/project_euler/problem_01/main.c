#include <stdio.h>
#include <stdlib.h>

int main(int argc, char* argv) {
    // Define method variables
    int exitCode = 1;
    int limit = 1000;
    int sum = 0;
    
    // Do code
    for (int i = 0; i < limit; i++) {
        if (i % 3 == 0 || i % 5 == 0) {
            sum += i;
        }
    }
    
    // Print results
    fprintf(stdout, "%d\n", sum);
    
    // Done
    return exitCode;
}

