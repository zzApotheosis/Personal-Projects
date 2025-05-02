#include <stdlib.h>
#include <stdio.h>

// Function declarations
unsigned long long gcd(unsigned long long, unsigned long long);

unsigned long long lcm(unsigned long long a, unsigned long long b) {
    // Define method variables
    unsigned long long p = (unsigned long long) a * b;
    
    // Done
    return p / gcd(a, b);
}

unsigned long long gcd(unsigned long long a, unsigned long long b) {
    // Define method variables
    unsigned long long r = 0ull;
    
    // Flip operators if needed
    if (a > b) {
        unsigned long long temp = a;
        a = b;
        b = temp;
    }
    
    // Calculate GCD
    while (r = a % b) {
        a = b;
        b = r;
    }

    // Done
    return b;
}

