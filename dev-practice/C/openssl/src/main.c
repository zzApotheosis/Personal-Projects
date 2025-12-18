#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "foo.h"

#define ROLE_SERVER 0
#define ROLE_CLIENT 1

int main(int argc, char **argv) {
  printf("This is project %s.\n", PROJECT_NAME);

#if ROLE == ROLE_SERVER
  do_server(argc, argv);
#endif

#if ROLE == ROLE_CLIENT
  do_client(argc, argv);
#endif
}
