#include <stdlib.h>
#include <stdio.h>
#include <gmp.h>

int main(int argc, char** argv) {
    // Define variables
    unsigned long int llim = 1lu;
    unsigned long int ulim = 100lu;
    unsigned long int power = 2lu;
    mpz_t soe; // Sum of exponent
    mpz_t eos; // Exponent of sum
    mpz_t sum;
    mpz_t diff;
    mpz_t temp;

    // Initialize variables
    mpz_inits(soe, eos, sum, diff, temp, NULL);

    // Calculate soe
    for (unsigned long int i = llim; i <= ulim; i++) {
        mpz_set_ui(temp, i);
        mpz_pow_ui(temp, temp, power);
        mpz_add(sum, sum, temp);
    }
    mpz_set(soe, sum);

    // Calculate eos
    mpz_set_ui(sum, (unsigned long int) 0);
    for (unsigned long int i = llim; i <= ulim; i++) {
        mpz_add_ui(sum, sum, i);
    }
    mpz_pow_ui(eos, sum, power);
    
    // Calculate difference
    mpz_sub(diff, eos, soe);
    mpz_abs(diff, diff);

    // Display results
    gmp_fprintf(stdout, "%Zd\n", diff);
    
    // Clean up
    mpz_clears(soe, eos, sum, diff, temp, NULL);

    // Done
    return EXIT_SUCCESS;
}

