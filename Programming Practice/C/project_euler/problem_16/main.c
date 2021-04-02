#include <stdio.h>
#include <stdlib.h>
#include <gmp.h>

int main(int argc, char* argv) {
    // Define method variables
    int exitCode = 0;
    mpz_t base;
    unsigned long int power = 1000;
    mpz_t num;
    mpz_t sum;
    mpz_t digit;
    
    // Initialize variables
    mpz_init(base);
    mpz_init(num);
    mpz_init(sum);
    mpz_init(digit);
    mpz_set_ui(base, (unsigned long int) 2); // Assign base = 2
    mpz_pow_ui(num, base, power); // Get big exponent

    // Do code
    while (mpz_cmp_ui(num, (unsigned long int) 0) > 0) {
        mpz_mod_ui(digit, num, (unsigned long int) 10);
        mpz_add(sum, sum, digit);
        mpz_tdiv_q_ui(num, num, (unsigned long int) 10);
    }

    // Print results
    mpz_out_str(stdout, 10, sum);
    fprintf(stdout, "\n");
    
    // Clean up
    mpz_clear(base);
    mpz_clear(num);
    mpz_clear(sum);
    mpz_clear(digit);
    
    // Done
    return exitCode;
}

