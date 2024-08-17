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
  mpz_set_str(z,
      "1234567890987654321234567890006900000000000000000"
      "0000000000000526532465234623462652652346234623462"
      "4623456234573457354673568456845684568745763456234"
      "6123462134623456257234573457345674536324623462357"
      "2457345634567345734573473457347534096802349560234"
      "5091704597123485918723645017263501782305760342758"
      "0239845702938475091832745089137240587120348502384"
      "7508370587203846702873465023485760923486750918734"
      "6597123645918736245918762345012786501827364598172"
      "2345349857934859823459283592385293534591783264590", 0);
  
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

