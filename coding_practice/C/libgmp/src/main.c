#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <gmp.h>

#include "common-macros.h"

int main(int argc, char * argv[]) {
  (void) argc; /* Compiler, plz stfu */
  (void) argv;

  mpz_t x;
  mpz_init(x);
  //mpz_out_str(stdout, 10, x);
  gmp_printf("%s is an mpz: %Zd\n", "this", x);

  return EXIT_SUCCESS;
}

