#include <stdlib.h>
#include <stdio.h>
#include "palindrome.h"

int main(int argc, char** argv) {
    // Define method variables
    int exitCode = 0;
    int limit = 999;
    int a = 0;
    int b = 0;
    int biggestPalindrome = 0;
    
    // Loop through all possible products and capture the biggest palindrome
    for (a = limit; a >= 100; a--) {
        for (b = limit; b >= a; b--) {
            if (isPalindrome(a * b) && a * b > biggestPalindrome) {
                biggestPalindrome = a * b;
            }
        }
    }

    // Print results
    fprintf(stdout, "%d\n", biggestPalindrome);
    
    // Done
    return exitCode;
}

