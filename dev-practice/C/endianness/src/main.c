#include <stdlib.h> // For EXIT_*
#include <stdio.h>  // For fprintf()
#include <unistd.h> // For no reason (good practice on Unix, maybe?)
#include <time.h>   // For time()
#include <string.h> // For strerror()
#include <errno.h>  // For errno

#define LITTLE 0
#define BIG 1

static FILE * stream = NULL;
static unsigned int endianness = 0;

void show_endianness(int * const exit_code)
{
  const unsigned int x = 0x00000001; // 1 is a good, simple number to determine endianness
  const unsigned char * const index = (const unsigned char * const) &x;

  const unsigned char first_byte = index[0];
  const unsigned char last_byte = index[sizeof(const unsigned int) - 1];
  const unsigned char least_significant_byte = (x & 0xFF);

  if (first_byte == last_byte)
    {
      fprintf(stream, "Cannot determine endianness from sample data (%u). First byte and last byte are equivalent\n", x);
      if (exit_code != NULL)
        *exit_code |= EXIT_FAILURE;
      return;
    }
  
  if (least_significant_byte == first_byte)
    {
      fprintf(stream, "This system is little-endian!\n");
      endianness = LITTLE;
    }
  else if (least_significant_byte == last_byte)
    {
      fprintf(stream, "This system is big-endian!\n");
      endianness = BIG;
    }
  else
    {      
      fprintf(stream, "Cannot determine endianness\n");
      if (exit_code != NULL)
        *exit_code |= EXIT_FAILURE;
    }
}

void show_epoch_time(int * const exit_code)
{
  time_t t = 0;
  const unsigned long int year_2038_index = (endianness == LITTLE) ? 3 : 4; /* The specific byte that represents the year 2038 problem will be in a different index depending on little/big endianness */
  
  if (time(&t) == ((time_t) -1))
    {
      fprintf(stream, "%s\n", strerror(errno));
      if (exit_code != NULL)
        *exit_code |= EXIT_FAILURE;
      return;
    }
  
  const unsigned char * index = (const unsigned char *) &t;

  fprintf(stream, "Time in seconds since the Epoch, represented as [%lu] bytes:\n", sizeof(time_t));
  for (unsigned int i = 0; i < sizeof(time_t); i++)
    {
      fprintf(stream, "[%p] = %08b", (const void * const) &index[i], index[i]);
      if (i == year_2038_index)
        {
          fprintf(stream, " <-- When this byte is > 10000000, the world has experienced the 2038 problem. See https://en.wikipedia.org/wiki/Year_2038_problem");
        }
      fprintf(stream, "\n");
    }
}

int main(void)
{
  int exit_code = EXIT_SUCCESS;
  stream = stderr;
  show_endianness(&exit_code);
  show_epoch_time(&exit_code);
  return exit_code;
}
