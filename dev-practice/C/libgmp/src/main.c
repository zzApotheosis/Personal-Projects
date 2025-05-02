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
  mpz_set_si(y, 69420);
  mpz_set_str(z,
      "214037531996711403489735576201333780809818562656253"
      "361013084186849597122035276377524870741016199384551"
      "174825312719173832734949617955266120908276482402851"
      "581123099623674169657330358862960922951593534331089"
      "311626270104334735885244965465370133312909238725178"
      "583724625724957385227392687755116967339708230347149"
      "480431980972650736231000178910218531352807334545066"
      "967704363463795702362816279868725722795383379120504"
      "375833195321502341190822242620725216073633790270933"
      "435241526519392700118934494343612062001192396318634", 0);

  
  /* Use output functions */
  gmp_printf("%s is an mpz: %Zx\n", "x", x);
  gmp_printf("%s is an mpz: %Zd\n", "y", y);
  gmp_printf("%s is an mpz: %Zd\n", "z", z);
  fprintf(stdout, "\n");

  /* Show the power of this epic math library */
  mpz_pow_ui(z, x, 42);
  gmp_printf("%s is now: %Zd\n", "z", z);
  fprintf(stdout, "\n");

  mpz_t divisor;
  mpz_init(divisor);
  mpz_set_ui(divisor, 1000);
  for (;;) {
    gmp_printf("%Zd / %Zd = ", z, divisor);
    mpz_cdiv_q(z, z, divisor);
    gmp_printf("%Zd\n", z);
    fprintf(stdout, "\n");

    if (mpz_cmp_ui(z, 1) <= 0) {
      break;
    }
  }

  /* Always be cognizant of memory usage */
  //mpz_clear(x);
  //mpz_clear(y);
  //mpz_clear(z);
  //mpz_clear(divisor);
  mpz_clears(x, y, z, divisor, NULL); /* Equivalent to the three statements above */

  /* 
   * I recently learned that the C standard dictates that if the main()
   * function does not explicitly define a return value, then it will default
   * to EXIT_SUCCESS, i.e. 0. I usually still prefer to define it anyway.
   */
  return EXIT_SUCCESS;
}

