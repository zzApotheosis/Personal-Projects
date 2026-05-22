#include <gcrypt.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "foo.h"

#define NEED_LIBGCRYPT_VERSION "1.11.0"

void library_function(const int x) {
  fprintf(stderr, "Library function received integer: %d\n", x);

  if (!gcry_check_version(NEED_LIBGCRYPT_VERSION)) {
    fprintf(stderr, "libgcrypt is too old (need %s, have %s)\n",
            NEED_LIBGCRYPT_VERSION, gcry_check_version(NULL));
    exit(2);
  }
}
