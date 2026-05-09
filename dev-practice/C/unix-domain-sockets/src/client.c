#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

char *socket_path = "\0hidden";

int
main (int argc, char **argv)
{
  struct sockaddr_un addr;
  char buf[100];
  int fd, rc;

  if (argc > 1)
    socket_path = argv[1];

  if ((fd = socket (AF_UNIX, SOCK_STREAM, 0)) == -1)
    {
      perror ("Socket error");
      return EXIT_FAILURE;
    }

  memset (&addr, 0, sizeof (addr));
  addr.sun_family = AF_UNIX;
  if (*socket_path == '\0')
    {
      *addr.sun_path = '\0';
      strncpy (addr.sun_path + 1, socket_path + 1, sizeof (addr.sun_path) - 2);
    }
  else
    {
      strncpy (addr.sun_path, socket_path, sizeof (addr.sun_path) - 1);
    }

  if (connect (fd, (struct sockaddr *)&addr, sizeof (addr)) == -1)
    {
      perror ("Connect error");
      return EXIT_FAILURE;
    }

  for (;;)
    {
      fprintf(stderr, "> ");
      rc = read (STDIN_FILENO, buf, sizeof (buf));
      if (rc <= 0)
        break;
      if (write (fd, buf, rc) != rc)
        {
          if (rc > 0)
            {
              fprintf (stderr, "partial write\n");
            }
          else
            {
              perror ("write error");
              return EXIT_FAILURE;
            }
        }
    }

  return EXIT_SUCCESS;
}
