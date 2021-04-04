#include <stdlib.h>
#include <stdio.h>

int isPalindrome(int n) {
    // Define method variables
    int reversed = 0;
    int temp = 0;
    
    // Calculate length of number
    temp = n;
    while (temp) {
        reversed = 10 * reversed + (temp % 10);
        temp /= 10;
    }

    // Done
    return reversed == n;
}

