#include <arpa/inet.h>
#include <netinet/in.h>
#include <openssl/bio.h>
#include <openssl/crypto.h>
#include <openssl/err.h>
#include <openssl/prov_ssl.h>
#include <openssl/ssl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

#include "foo.h"

#define ERROR(m)                                                              \
  do                                                                          \
    {                                                                         \
      fprintf (stderr, "[%s:%d] %s\n", __FILE__, __LINE__, m);                \
    }                                                                         \
  while (0)

#define ERROR_GOTO(m, l)                                                      \
  do                                                                          \
    {                                                                         \
      ERROR (m);                                                              \
      goto l;                                                                 \
    }                                                                         \
  while (0)

/* Buffer size to be used for transfers */
#define BUFSIZE 4096

static int
get_socket (const char *const port_str, BIO **const bio_out)
{
  BIO *const bio = BIO_new (BIO_s_datagram ());
  if (bio == NULL)
    ERROR_GOTO ("Bad BIO_new", bad_bio_new);
  BIO_set_bind_mode (bio, BIO_BIND_REUSEADDR);

  struct sockaddr_in addr = { 0 };
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = INADDR_ANY;
  addr.sin_port = htons (atoi (port_str));

  const int socket_fd = socket (AF_INET, SOCK_DGRAM, 0);
  if (socket_fd < 0)
    ERROR_GOTO ("Bad socket", bad_socket);

  if (bind (socket_fd, (const struct sockaddr *)&addr,
            sizeof (struct sockaddr_in))
      < 0)
    ERROR_GOTO ("Bad bind", bad_bind);

  BIO_set_fd (bio, socket_fd, BIO_CLOSE);

  /* Done */
  *bio_out = bio;
  return 0;

bad_bind:
bad_socket:
  BIO_free (bio);
bad_bio_new:
  return -1;
}

static int
client_connect (const char *const server_addr_str, const char *const port_str,
                BIO **const bio_out)
{
  int sock = -1;
  BIO_ADDRINFO *res = NULL;
  const BIO_ADDRINFO *ai = NULL;

  /*
   * Lookup IP address info for the server.
   */
  if (!BIO_lookup_ex (server_addr_str, port_str, BIO_LOOKUP_CLIENT, AF_UNSPEC,
                      SOCK_DGRAM, 0, &res))
    {
      fprintf (stderr, "BIO_lookup_ex failed\n");
      return -1;
    }

  BIO_ADDR *peer_addr = NULL;
  /*
   * Loop through all the possible addresses for the server and find one
   * we can connect to.
   */
  for (ai = res; ai != NULL; ai = BIO_ADDRINFO_next (ai))
    {
      /*
       * Create a TCP socket. We could equally use non-OpenSSL calls such
       * as "socket" here for this and the subsequent connect and close
       * functions. But for portability reasons and also so that we get
       * errors on the OpenSSL stack in the event of a failure we use
       * OpenSSL's versions of these functions.
       */
      sock = BIO_socket (BIO_ADDRINFO_family (ai), SOCK_DGRAM, 0, 0);
      if (sock == -1)
        continue;

      /* Connect the socket to the server's address */
      // if (!BIO_connect(sock, BIO_ADDRINFO_address(ai), BIO_SOCK_NODELAY))
      //   {
      //     ERROR("Can't connect");
      //     BIO_closesocket(sock);
      //     sock = -1;
      //     continue;
      //   }

      peer_addr = BIO_ADDR_dup (BIO_ADDRINFO_address (ai));

      /* We have a connected socket so break out of the loop */
      break;
    }

  /* Free the address information resources we allocated earlier */
  BIO_ADDRINFO_free (res);

  /* Create a BIO to wrap the socket */
  BIO *const bio = BIO_new (BIO_s_datagram ());
  if (bio == NULL)
    {
      fprintf (stderr, "BIO_new failed\n");
      BIO_closesocket (sock);
      return -1;
    }

  /*
   * Associate the newly created BIO with the underlying socket. By
   * passing BIO_CLOSE here the socket will be automatically closed when
   * the BIO is freed. Alternatively you can use BIO_NOCLOSE, in which
   * case you must close the socket explicitly when it is no longer
   * needed.
   */
  BIO_set_fd (bio, sock, BIO_CLOSE);

  BIO_ADDR *peer = BIO_ADDR_new ();
  BIO_ctrl_dgram_connect (bio, peer);
  BIO_ctrl_set_connected (bio, peer_addr);

  *bio_out = bio;
  return 0;
}

int
server (const char *port_str)
{
  unsigned char buffer[BUFSIZE] = { 0 };
  memset (buffer, 0, BUFSIZE);
  size_t nread = 0, nwritten = 0, total = 0;

  /* Get a socket which is ready to listen on the server's port number */
  BIO *bio = NULL;
  if (get_socket (port_str, &bio) < 0)
    abort ();

  /* Server loop */
  unsigned int client_id = 0;
  fprintf (stderr, "Listening for connections...\n");
  for (;;)
    {
      BIO_MSG msg = { 0 };
      size_t msgs_processed = 0;
      BIO_recvmmsg (bio, &msg, sizeof (BIO_MSG), 1, 0, &msgs_processed);
      fprintf (stderr, "Received: %s\n", (const char *const)msg.data);
      continue;

      BIO *const client = BIO_pop (bio);
      fprintf (stderr, "[%u] New client connected.\n", client_id);

      /* Fork to serve other clients */
      if (fork () > 0)
        {
          client_id++;
          continue;
        }

      /* Echo client input */
      for (; BIO_read_ex (client, buffer, sizeof (buffer), &nread) > 0;)
        {
          fprintf (stderr, "[%u] Client said: %s", client_id, buffer);
          if (BIO_write_ex (client, buffer, nread, &nwritten) > 0
              && nwritten == nread)
            {
              total += nwritten;
              memset (buffer, 0, BUFSIZE);
              continue;
            }
          fprintf (stderr, "[%u] Error echoing client input.\n", client_id);
          break;
        }

      /* Close connection and free memory */
      fprintf (stderr, "[%u] Client connection closed, %zu bytes sent\n",
               client_id, total);
      exit (0);
    }
  return 0;
}

int
client (const char *const server_addr, const char *const port_str)
{
  int result = EXIT_SUCCESS;
  BIO *bio = NULL;

  if (client_connect (server_addr, port_str, &bio) < 0)
    ERROR_GOTO ("Bad client_connect", bad_client_connect);

  size_t written = 0;
  char buf[BUFSIZE];
  memset (buf, 0, BUFSIZE);

  size_t readbytes = 0;

  /* Let's do a user input driven echo loop */
  fprintf (stderr, "Enter Ctrl+D to quit.\n");
  for (;;)
    {
      /* Reset the buffer */
      memset (buf, 0, BUFSIZE);
      written = 0;
      readbytes = 0;

      /* Get user input */
      fprintf (stderr, "> ");
      fgets (buf, BUFSIZE, stdin);

      if (feof (stdin))
        {
          fprintf (stderr, "\n");
          break;
        }

      /* Send the data over the wire */
      BIO_MSG msg = { 0 };
      msg.data = buf;
      msg.data_len = strnlen (buf, BUFSIZE);
      struct sockaddr_in peer = { 0 };
      peer.sin_family = AF_INET;
      peer.sin_port = htons (atoi (port_str));
      peer.sin_addr.s_addr = inet_addr (server_addr);
      size_t msgs_processed = 0;
      BIO_sendmmsg (bio, &msg, sizeof (BIO_MSG), 1, 0, &msgs_processed);
      fprintf (stderr, "Client sent: %s\n", buf);

      /* Receive response */
      memset (buf, 0, BUFSIZE);
    read_more:
      BIO_read_ex (bio, &buf[readbytes], sizeof (buf) - readbytes, &readbytes);
      if (readbytes != written)
        goto read_more;

      /* Output response */
      fwrite (buf, 1, strnlen (buf, BUFSIZE), stdout);
    }

  /*
   * If something bad happened then we will dump the contents of the
   * OpenSSL error stack to stderr. There might be some useful diagnostic
   * information there.
   */
  if (result == EXIT_FAILURE)
    fprintf (stderr, "Bad exit result\n");

  return 0;

bad_client_connect:
  return -1;
}

void
do_server (int argc, char *argv[])
{
  if (argc < 2)
    {
      fprintf (stderr, "Usage: %s <port>\n", argv[0]);
      return;
    }
  if (argc > 2)
    fprintf (stderr, "WARNING: Too many args provided\n");

  server (argv[1]);
}

void
do_client (int argc, char *argv[])
{
  if (argc < 3)
    {
      fprintf (stderr, "Usage: %s <server address> <port>\n", argv[0]);
      return;
    }
  if (argc > 3)
    fprintf (stderr, "WARNING: Too many args provided\n");

  client (argv[1], argv[2]);
}
