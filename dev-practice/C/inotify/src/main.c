#include <stdio.h>
#include <unistd.h>

#include "foo.h"

int main(int argc, char * argv[])
{
  fprintf(stderr, "This is project %s.\n", PROJECT_NAME);
  do_inotify_example(argc, argv);
}
