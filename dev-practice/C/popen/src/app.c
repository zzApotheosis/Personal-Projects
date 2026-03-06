#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

static int usage(const int argc, char ** const argv)
{
  fprintf(stderr, "This program runs the given arguments in popen()\n");
  fprintf(stderr, "\n");
  fprintf(stderr, "Usage: %s [args...]\n", argv[0]);
  return 0;
}

static int do_commands(const int argc, char ** const argv)
{
  int status = 0;
  const long page_size = sysconf(_SC_PAGESIZE);
  char * const buffer = (char * const) calloc(1, page_size);
  if (buffer == NULL)
    return 1;

  for (int i = 1; i < argc; i++)
    {
      FILE * const cpid = popen(argv[i], "r");
      if (cpid == NULL)
        break;
      for (;!feof(cpid);)
        {
          const size_t n = fread(buffer, 1, page_size, cpid);
          fwrite(buffer, 1, n, stdout);
        }
      pclose(cpid);
    }
  return status;
}

int app(int argc, char * argv[])
{
  if (argc == 1)
    return usage(argc, argv);
  return do_commands(argc, argv);
}
