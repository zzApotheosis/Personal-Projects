#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <gmp.h>

#include "common-macros.h"

int main(int argc, char * argv[]) {
  (void) argc; /* Compiler, plz stfu */
  (void) argv;

  /* Define variables */
  mpz_t x, y, z;

  /* Initialize variables */
  //mpz_init(x);
  //mpz_init(y);
  //mpz_init(z);
  mpz_inits(x, y, z, NULL); /* Equivalent to the three statements above */
  
  /* Assignment */
  mpz_set_ui(x, 0x1337);
  mpz_set_si(y, -69420);
  mpz_set_str(z, "12345678909876543212345678900069", 0);
  
  /* Use output functions */
  gmp_printf("%s is an mpz: %Zx\n", "x", x);
  gmp_printf("%s is an mpz: %Zd\n", "y", y);
  gmp_printf("%s is an mpz: %Zd\n", "z", z);

  /* Always be cognizant of memory usage */
  //mpz_clear(x);
  //mpz_clear(y);
  //mpz_clear(z);
  mpz_clears(x, y, z, NULL); /* Equivalent to the three statements above */

  /* 
   * I recently learned that the C standard dictates that if the main()
   * function does not explicitly define a return value, then it will default
   * to EXIT_SUCCESS, i.e. 0.
   */
}

