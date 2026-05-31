#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "foo.h"

int main(int argc, char *argv[]) {
  fprintf(stderr, "This is project %s.\n", PROJECT_NAME);

  return do_app(argc, argv);
}
