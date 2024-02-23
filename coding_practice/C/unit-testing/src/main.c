#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "common-macros.h"
#include "math.h"

int main(void) {
        /* Addition */
        fprintf(stdout, "%d? Nice\n", add(35, 34));

        /* Subtraction */
        fprintf(stdout, "%dspeak\n", sub(1500, 163));

        /* Multiplication */
        fprintf(stdout, "%d blaze it\n", multi(42, 10));

        /* Division */
        fprintf(stdout, "Life, the universe, and everything: %d\n", divide(420, 10));

        /* Modulo */
        fprintf(stdout, "%d is even? %s\n", 69, mod(69, 2) == 1 ? "Hell nah!" : "Heavens yes!");

        /* Exponent */
        fprintf(stdout, "Beeg number: %d\n", power(10, 4));

        /* Factorial */
        fprintf(stdout, "%d! = %ld\n", 5, factorial(5));

        return EXIT_SUCCESS;
}
