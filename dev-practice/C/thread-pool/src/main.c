#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "foo.h"

int main(void)
{
  fprintf(stderr, "This is project %s.\n", PROJECT_NAME);

  do_thread_pool_example();
}
