#include <stdlib.h>
#include <stdio.h>
#include <sys/socket.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <gnutls/gnutls.h>

#include "foo.h"

/* Buffer size to be used for transfers */
#define BUFSIZE 4096

int server(const char * port_str,
           const char * chain_pem,
           const char * key_pem)
{
  return 0;
}

int client(const char * const server_addr, const char * const port_str)
{
  return 0;
}

void do_server(int argc, char * argv[])
{
  if (argc < 4)
    {
      fprintf(stderr, "Usage: %s <port> <certificate chain> <private key>\n", argv[0]);
      return;
    }

  server(argv[1], argv[2], argv[3]);
}

void do_client(int argc, char * argv[])
{
  if (argc < 3)
    {
      fprintf(stderr, "Usage: %s <server address> <port>\n", argv[0]);
      return;
    }

  client(argv[1], argv[2]);
}
