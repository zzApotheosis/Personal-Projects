#include <stdio.h>
#include <stdlib.h>

int main(int argc, char* argv) {
    // Define method variables
    int exitCode = 0;
    int limit = 4000000;
    int previous = 0;
    int temp = 0;
    int sum = 0;
    int index = 1;
    
    // Solve
    while (index < limit) {
        // Summation
        if (index % 2 == 0) {
            sum += index;
        }

        // Update terms
        temp = index;
        index += previous;
        previous = temp;
    }
    
    // Print results
    fprintf(stdout, "%d\n", sum);
    
    // Done
    return exitCode;
}

