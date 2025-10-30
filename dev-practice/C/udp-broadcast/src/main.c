#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "foo.h"

#define CLIENT_ROLE 0
#define SERVER_ROLE 1

int main(int argc, char **argv) {
  fprintf(stderr, "This is project %s.\n", PROJECT_NAME);

#if ROLE == CLIENT_ROLE
  do_udp_broadcast_client(argc, argv);
#endif
  
#if ROLE == SERVER_ROLE
  do_udp_broadcast_server(argc, argv);
#endif
  
  return 0;
}
