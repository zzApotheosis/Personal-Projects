#include <stdlib.h> // For EXIT_*
#include <stdio.h>  // For fprintf()
#include <unistd.h> // For no reason (good practice on Unix, maybe?)
#include <time.h>   // For time()
#include <string.h> // For strerror()
#include <errno.h>  // For errno

int main(void)
{
  FILE * const stream = stderr;
  const unsigned char * index = NULL;
  time_t t = 0;
  time(&t);
  if (time(&t) == ((time_t) -1))
    {
      fprintf(stream, "%s\n", strerror(errno));
      return EXIT_FAILURE;
    }
  
  index = (const unsigned char *) &t;

  fprintf(stream, "Time in seconds since the Epoch, represented as [%lu] bytes:\n", sizeof(time_t));
  for (unsigned int i = 0; i < sizeof(time_t); i++)
    {
      fprintf(stderr, "[%02x]", index[i]);
      if ((i + 1) % 8 == 0)
        fprintf(stderr, "\n");
    }
}
