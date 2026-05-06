#include <arpa/inet.h>
#include <netinet/in.h>
#include <openssl/bio.h>
#include <openssl/crypto.h>
#include <openssl/prov_ssl.h>
#include <openssl/ssl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

#include "foo.h"

/* Buffer size to be used for transfers */
#define BUFSIZE 4096

static int
get_socket(const char * const port_str, BIO ** const bio_out)
{
  /* Create BIO object */
  BIO * const bio = BIO_new_accept(port_str);
  if (bio == NULL)
    {
      fprintf(stderr, "BIO_new_accept failed\n");
      goto bad_bio_new_accept;
    }

  BIO_set_bind_mode(bio, BIO_BIND_REUSEADDR);
  if (BIO_do_accept(bio) <= 0)
    {
      fprintf(stderr, "BIO_do_accept failed\n");
      goto bad_bio_set_bind_mode;
    }

  /* Done */
  *bio_out = bio;
  return 0;

bad_bio_set_bind_mode:
  BIO_free(bio);
bad_bio_new_accept:
  return -1;
}

static int
client_connect(const char * const server_addr_str, const char * const port_str, BIO ** const bio_out)
{
  int sock = -1;
  BIO_ADDRINFO * res = NULL;
  const BIO_ADDRINFO * ai = NULL;
  
  /*
   * Lookup IP address info for the server.
   */
  if (!BIO_lookup_ex(server_addr_str, port_str, BIO_LOOKUP_CLIENT, AF_UNSPEC, SOCK_STREAM, 0, &res))
    {
      fprintf(stderr, "BIO_lookup_ex failed\n");
      return -1;
    }

  /*
   * Loop through all the possible addresses for the server and find one
   * we can connect to.
   */
  for (ai = res; ai != NULL; ai = BIO_ADDRINFO_next(ai))
    {
      /*
       * Create a TCP socket. We could equally use non-OpenSSL calls such
       * as "socket" here for this and the subsequent connect and close
       * functions. But for portability reasons and also so that we get
       * errors on the OpenSSL stack in the event of a failure we use
       * OpenSSL's versions of these functions.
       */
      sock = BIO_socket(BIO_ADDRINFO_family(ai), SOCK_STREAM, 0, 0);
      if (sock == -1)
        continue;

      /* Connect the socket to the server's address */
      if (!BIO_connect(sock, BIO_ADDRINFO_address(ai), BIO_SOCK_NODELAY))
        {
          BIO_closesocket(sock);
          sock = -1;
          continue;
        }

      /* We have a connected socket so break out of the loop */
      break;
    }

  /* Free the address information resources we allocated earlier */
  BIO_ADDRINFO_free(res);

  /* Create a BIO to wrap the socket */
  BIO * const bio = BIO_new(BIO_s_socket());
  if (bio == NULL)
    {
      fprintf(stderr, "BIO_new failed\n");
      BIO_closesocket(sock);
      return -1;
    }

  /*
   * Associate the newly created BIO with the underlying socket. By
   * passing BIO_CLOSE here the socket will be automatically closed when
   * the BIO is freed. Alternatively you can use BIO_NOCLOSE, in which
   * case you must close the socket explicitly when it is no longer
   * needed.
   */
  BIO_set_fd(bio, sock, BIO_CLOSE);

  *bio_out = bio;
  return 0;
}

int
server(const char * port_str)
{
  unsigned char buffer[BUFSIZE];
  memset(buffer, 0, BUFSIZE);
  size_t nread = 0, nwritten = 0, total = 0;

  /* Get a socket which is ready to listen on the server's port number */
  BIO * bio = NULL;
  if (get_socket(port_str, &bio) < 0)
    abort();

  /* Server loop */
  unsigned int client_id = 0;
  fprintf(stderr, "Listening for connections...\n");
  for (;;)
    {
      if (BIO_do_accept(bio) <= 0)
        continue;

      BIO * const client = BIO_pop(bio);
      fprintf(stderr, "[%u] New client connected.\n", client_id);

      /* Fork to serve other clients */
      if (fork() > 0)
        {
          client_id++;
          continue;
        }

      /* Echo client input */
      for (; BIO_read_ex(client, buffer, sizeof(buffer), &nread) > 0;)
        {
          fprintf(stderr, "[%u] Client said: %s", client_id, buffer);
          if (BIO_write_ex(client, buffer, nread, &nwritten) > 0 && nwritten == nread)
            {
              total += nwritten;
              memset(buffer, 0, BUFSIZE);
              continue;
            }
          fprintf(stderr, "[%u] Error echoing client input.\n", client_id);
          break;
        }

      /* Close connection and free memory */
      fprintf(stderr, "[%u] Client connection closed, %zu bytes sent\n", client_id, total);
      exit(0);
    }
  return 0;
}

int
client(const char * const server_addr, const char * const port_str)
{
  int result = EXIT_SUCCESS;
  BIO * bio = NULL;

  if (client_connect(server_addr, port_str, &bio) < 0)
    return -1;

  size_t written = 0;
  char buf[BUFSIZE];
  memset(buf, 0, BUFSIZE);

  size_t readbytes = 0;

  /* Let's do a user input driven echo loop */
  fprintf(stderr, "Enter Ctrl+D to quit.\n");
  for (;;)
    {
      /* Reset the buffer */
      memset(buf, 0, BUFSIZE);
      written = 0;
      readbytes = 0;

      /* Get user input */
      fprintf(stderr, "> ");
      fgets(buf, BUFSIZE, stdin);

      if (feof(stdin))
        {
          fprintf(stderr, "\n");
          break;
        }

      /* Send the data over the wire */
      BIO_write_ex(bio, buf, strnlen(buf, BUFSIZE), &written);

      /* Receive response */
      memset(buf, 0, BUFSIZE);
    read_more:
      BIO_read_ex(bio, &buf[readbytes], sizeof(buf) - readbytes, &readbytes);
      if (readbytes != written)
        goto read_more;

      /* Output response */
      fwrite(buf, 1, strnlen(buf, BUFSIZE), stdout);
    }

  /*
   * If something bad happened then we will dump the contents of the
   * OpenSSL error stack to stderr. There might be some useful diagnostic
   * information there.
   */
  if (result == EXIT_FAILURE)
    fprintf(stderr, "Bad exit result\n");

  return result;
}

void
do_server(int argc, char * argv[])
{
  if (argc < 2)
    {
      fprintf(stderr, "Usage: %s <port>\n", argv[0]);
      return;
    }
  if (argc > 2)
    fprintf(stderr, "WARNING: Too many args provided\n");

  server(argv[1]);
}

void
do_client(int argc, char * argv[])
{
  if (argc < 3)
    {
      fprintf(stderr, "Usage: %s <server address> <port>\n", argv[0]);
      return;
    }
  if (argc > 3)
    fprintf(stderr, "WARNING: Too many args provided\n");

  client(argv[1], argv[2]);
}
