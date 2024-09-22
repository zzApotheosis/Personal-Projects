#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <stdarg.h>

#include "common-macros.h"

int func(int n, ...) {
  va_list arglist;
  va_start(arglist, n);
  for (int i = 0; i < n; i++) {
    fprintf(stdout, "%s", va_arg(arglist, char *));
  }
  va_end(arglist);
  return n;
}

int main(int argc, char * argv[]) {
  (void) argc; /* Compiler, plz stfu */
  (void) argv;

  func(2, "I do not know if I am Tim Pope dreaming I am an Emacs user,\n",
      "or an Emacs user dreaming I am Tim Pope!\n");

  return EXIT_SUCCESS;
}

