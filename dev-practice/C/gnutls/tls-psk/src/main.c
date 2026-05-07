#include <stdio.h>
#include <unistd.h>

#include "foo.h"

int
main (int argc, char * argv[])
{
  int result = 0;
  fprintf (stderr, "This is project %s.\n", PROJECT_NAME);

#ifdef SERVER
  result = echo_server (argc, argv);
#endif

#ifdef CLIENT
  result = echo_client (argc, argv);
#endif

  return result;
}
