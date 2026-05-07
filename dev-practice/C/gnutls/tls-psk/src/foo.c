/* This example code is placed in the public domain. */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <arpa/inet.h>
#include <assert.h>
#include <errno.h>
#include <gnutls/gnutls.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#include "foo.h"

#define ERROR_GOTO(m, l)                                                      \
  do                                                                          \
    {                                                                         \
      fprintf (stderr, "[%s:%d] %s\n", __FILE__, __LINE__, m);                \
      goto l;                                                                 \
    }                                                                         \
  while (0)

#define CHECK(x) assert ((x) >= 0)
#define LOOP_CHECK(rval, cmd)                                                 \
  do                                                                          \
    {                                                                         \
      rval = cmd;                                                             \
    }                                                                         \
  while (rval == GNUTLS_E_AGAIN || rval == GNUTLS_E_INTERRUPTED)

#define SOCKET_ERR(err, s)                                                    \
  if (err == -1)                                                              \
    {                                                                         \
      perror (s);                                                             \
      return (1);                                                             \
    }
#define MAX_BUF 4096

static int
pskfunc (gnutls_session_t session, const char *username, gnutls_datum_t *key)
{
  fprintf (stderr, "psk: username %s\n", username);
  key->data = gnutls_malloc (4);
  key->data[0] = 0xDE;
  key->data[1] = 0xAD;
  key->data[2] = 0xBE;
  key->data[3] = 0xEF;
  key->size = 4;
  return 0;
}

void
library_function (const int x)
{
  fprintf (stderr, "Library function received integer: %d\n", x);
}

int
echo_server (const int argc, char *argv[])
{
  int err, listen_sd;
  int sd, ret;
  struct sockaddr_in sa_serv;
  struct sockaddr_in sa_cli;
  socklen_t client_len;
  char topbuf[512] = { 0 };
  gnutls_session_t session;
  // gnutls_certificate_credentials_t x509_cred;
  gnutls_psk_server_credentials_t psk_cred;
  gnutls_priority_t priority_cache;
  char buffer[MAX_BUF + 1] = { 0 };
  int optval = 1;
  int kx;

  if (argc < 2)
    ERROR_GOTO ("Incorrect usage", bad_usage);

  if (gnutls_check_version ("3.1.4") == NULL)
    {
      fprintf (stderr, "GnuTLS 3.1.4 or later is required for this example\n");
      exit (1);
    }

  /* for backwards compatibility with gnutls < 3.3.0 */
  gnutls_global_init ();

  // gnutls_certificate_allocate_credentials (&x509_cred);
  // gnutls_certificate_set_x509_trust_file (x509_cred, CAFILE,
  //                                         GNUTLS_X509_FMT_PEM);

  // gnutls_certificate_set_x509_crl_file (x509_cred, CRLFILE,
  //                                       GNUTLS_X509_FMT_PEM);

  // gnutls_certificate_set_x509_key_file (x509_cred, CERTFILE, KEYFILE,
  //                                       GNUTLS_X509_FMT_PEM);

  gnutls_psk_allocate_server_credentials (&psk_cred);
  gnutls_psk_set_server_credentials_function (psk_cred, pskfunc);

  /* pre-3.6.3 equivalent:
   * gnutls_priority_init(&priority_cache,
   *                      "NORMAL:+PSK:+ECDHE-PSK:+DHE-PSK",
   *                      NULL);
   */
  gnutls_priority_init2 (&priority_cache, "+ECDHE-PSK:+DHE-PSK:+PSK", NULL,
                         GNUTLS_PRIORITY_INIT_DEF_APPEND);

  // gnutls_certificate_set_known_dh_params (x509_cred,
  // GNUTLS_SEC_PARAM_MEDIUM);

  /* Socket operations
   */
  listen_sd = socket (AF_INET, SOCK_STREAM, 0);
  SOCKET_ERR (listen_sd, "socket");

  memset (&sa_serv, '\0', sizeof (sa_serv));
  sa_serv.sin_family = AF_INET;
  sa_serv.sin_addr.s_addr = INADDR_ANY;
  sa_serv.sin_port = htons (atoi (argv[1])); /* Server Port number */

  setsockopt (listen_sd, SOL_SOCKET, SO_REUSEADDR, (void *)&optval,
              sizeof (int));

  err = bind (listen_sd, (struct sockaddr *)&sa_serv, sizeof (sa_serv));
  SOCKET_ERR (err, "bind");
  err = listen (listen_sd, 1024);
  SOCKET_ERR (err, "listen");

  fprintf (stderr, "Server ready. Listening to port '%d'.\n\n",
           atoi (argv[1]));

  client_len = sizeof (sa_cli);
  for (;;)
    {
      gnutls_init (&session, GNUTLS_SERVER);
      gnutls_priority_set (session, priority_cache);
      // gnutls_credentials_set (session, GNUTLS_CRD_CERTIFICATE, x509_cred);
      gnutls_credentials_set (session, GNUTLS_CRD_PSK, psk_cred);

      /* request client certificate if any.
       */
      gnutls_certificate_server_set_request (session, GNUTLS_CERT_REQUEST);

      sd = accept (listen_sd, (struct sockaddr *)&sa_cli, &client_len);

      fprintf (stderr, "- connection from %s, port %d\n",
               inet_ntop (AF_INET, &sa_cli.sin_addr, topbuf, sizeof (topbuf)),
               ntohs (sa_cli.sin_port));

      gnutls_transport_set_int (session, sd);
      LOOP_CHECK (ret, gnutls_handshake (session));
      if (ret < 0)
        {
          close (sd);
          gnutls_deinit (session);
          fprintf (stderr, "*** Handshake has failed (%s)\n\n",
                   gnutls_strerror (ret));
          continue;
        }
      fprintf (stderr, "- Handshake was completed\n");

      kx = gnutls_kx_get (session);
      if (kx == GNUTLS_KX_PSK || kx == GNUTLS_KX_DHE_PSK
          || kx == GNUTLS_KX_ECDHE_PSK)
        {
          fprintf (stderr, "- User %s was connected\n",
                   gnutls_psk_server_get_username (session));
        }

      /* see the Getting peer's information example */
      /* print_info(session); */

      for (;;)
        {
          LOOP_CHECK (ret, gnutls_record_recv (session, buffer, MAX_BUF));

          if (ret == 0)
            {
              fprintf (stderr, "\n- Peer has closed the GnuTLS connection\n");
              break;
            }
          else if (ret < 0 && gnutls_error_is_fatal (ret) == 0)
            {
              fprintf (stderr, "*** Warning: %s\n", gnutls_strerror (ret));
            }
          else if (ret < 0)
            {
              fprintf (stderr,
                       "\n*** Received corrupted "
                       "data(%d). Closing the connection.\n\n",
                       ret);
              break;
            }
          else if (ret > 0)
            {
              /* echo data back to the client
               */
              gnutls_record_send (session, buffer, ret);
            }
        }
      fprintf (stderr, "\n");
      /* do not wait for the peer to close the connection.
       */
      LOOP_CHECK (ret, gnutls_bye (session, GNUTLS_SHUT_WR));

      close (sd);
      gnutls_deinit (session);
    }
  close (listen_sd);

  // gnutls_certificate_free_credentials (x509_cred);
  gnutls_psk_free_server_credentials (psk_cred);

  gnutls_priority_deinit (priority_cache);

  gnutls_global_deinit ();

  return 0;

bad_usage:
  fprintf (stderr, "Usage: %s <port>\n", argv[0]);
  return -1;
}

static int
tcp_connect (const char *const addr_str, const char *const port_str)
{
  int fd = socket (AF_INET, SOCK_STREAM, 0);
  if (fd < 0)
    ERROR_GOTO ("Bad socket", bad_socket);

  struct sockaddr_in server_addr = { 0 };
  server_addr.sin_family = AF_INET;
  server_addr.sin_port = htons (atoi (port_str));
  server_addr.sin_addr.s_addr = inet_addr (addr_str);

  if (connect (fd, (const struct sockaddr *)&server_addr,
               sizeof (struct sockaddr_in))
      < 0)
    ERROR_GOTO ("Bad connect", bad_connect);

  return fd;

bad_connect:
  close (fd);
bad_socket:
  return -1;
}

static void
tcp_close (const int fd)
{
  close (fd);
}

int
echo_client (const int argc, char *argv[])
{
  if (argc < 3)
    ERROR_GOTO ("Incorrect usage", bad_usage);

  int ret, sd, ii;
  gnutls_session_t session;
  char buffer[MAX_BUF + 1];
  const char *err;
  gnutls_psk_client_credentials_t pskcred;
  const gnutls_datum_t key = { (void *)"DEADBEEF", 8 };

  if (gnutls_check_version ("3.6.3") == NULL)
    {
      fprintf (stderr, "GnuTLS 3.6.3 or later is required for this example\n");
      exit (1);
    }

  CHECK (gnutls_global_init ());

  CHECK (gnutls_psk_allocate_client_credentials (&pskcred));
  CHECK (gnutls_psk_set_client_credentials (pskcred, "test", &key,
                                            GNUTLS_PSK_KEY_HEX));

  /* Initialize TLS session
   */
  CHECK (gnutls_init (&session, GNUTLS_CLIENT));

  ret = gnutls_set_default_priority_append (
      session, "-KX-ALL:+ECDHE-PSK:+DHE-PSK:+PSK", &err, 0);

  /* Alternative for pre-3.6.3 versions:
   * gnutls_priority_set_direct(session, "NORMAL:+ECDHE-PSK:+DHE-PSK:+PSK",
   * &err)
   */
  if (ret < 0)
    {
      if (ret == GNUTLS_E_INVALID_REQUEST)
        {
          fprintf (stderr, "Syntax error at: %s\n", err);
        }
      exit (1);
    }

  /* put the x509 credentials to the current session
   */
  CHECK (gnutls_credentials_set (session, GNUTLS_CRD_PSK, pskcred));

  /* connect to the peer
   */
  sd = tcp_connect (argv[1], argv[2]);
  if (sd < 0)
    abort ();

  gnutls_transport_set_int (session, sd);
  gnutls_handshake_set_timeout (session, GNUTLS_DEFAULT_HANDSHAKE_TIMEOUT);

  /* Perform the TLS handshake
   */
  do
    {
      ret = gnutls_handshake (session);
    }
  while (ret < 0 && gnutls_error_is_fatal (ret) == 0);

  if (ret < 0)
    {
      fprintf (stderr, "*** Handshake failed\n");
      gnutls_perror (ret);
      goto end;
    }
  else
    {
      char *desc;

      desc = gnutls_session_get_desc (session);
      fprintf (stderr, "- Session info: %s\n", desc);
      gnutls_free (desc);
    }

  for (;;)
    {
      memset (buffer, 0, sizeof (buffer));
      fprintf(stderr, "> ");
      if (fgets (buffer, sizeof (buffer), stdin) == NULL && feof(stdin))
        break;

      LOOP_CHECK (ret, gnutls_record_send (session, buffer,
                                           strnlen (buffer, MAX_BUF)));

      LOOP_CHECK (ret, gnutls_record_recv (session, buffer, MAX_BUF));
      if (ret == 0)
        {
          fprintf (stderr, "- Peer has closed the TLS connection\n");
          goto end;
        }
      else if (ret < 0 && gnutls_error_is_fatal (ret) == 0)
        {
          fprintf (stderr, "*** Warning: %s\n", gnutls_strerror (ret));
        }
      else if (ret < 0)
        {
          fprintf (stderr, "*** Error: %s\n", gnutls_strerror (ret));
          goto end;
        }

      if (ret > 0)
        {
          fprintf (stderr, "- Received %d bytes: ", ret);
          for (ii = 0; ii < ret; ii++)
            {
              fputc (buffer[ii], stderr);
            }
          fputs ("\n", stderr);
        }
    }

  CHECK (gnutls_bye (session, GNUTLS_SHUT_RDWR));

end:

  tcp_close (sd);

  gnutls_deinit (session);

  gnutls_psk_free_client_credentials (pskcred);

  gnutls_global_deinit ();

  return 0;

bad_usage:
  fprintf (stderr, "Usage: %s <address> <port>\n", argv[0]);
  return -1;
}
