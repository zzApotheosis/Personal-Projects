#include <openssl/bio.h>
#include <openssl/crypto.h>
#include <openssl/prov_ssl.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/socket.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <openssl/ssl.h>
#include <netinet/in.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "foo.h"

/* Buffer size to be used for transfers */
#define BUFSIZE 4096

/*
 * Prepare a SSL context for use by the server
 */
static SSL_CTX * get_server_context(const char * chain_pem,
                                    const char * key_pem)
{
  SSL_CTX * ctx;
  uint64_t op = 0;

  /* Get a default context */
  if (!(ctx = SSL_CTX_new(TLS_server_method())))
    {
      fprintf(stderr, "SSL_CTX_new failed\n");
      return NULL;
    }

  /* Set minimum TLS version allowed */
  if (!SSL_CTX_set_min_proto_version(ctx, TLS1_3_VERSION))
    {
      fprintf(stderr, "SSL_CTX_set_min_proto_version failed\n");
      goto fail;
    }

  /* Configure option flags */
  op |= SSL_OP_IGNORE_UNEXPECTED_EOF;
  op |= SSL_OP_NO_RENEGOTIATION;
  op |= SSL_OP_CIPHER_SERVER_PREFERENCE;
  SSL_CTX_set_options(ctx, op);

  /* Set the CA file location for the server */
  //if (SSL_CTX_load_verify_locations(ctx, ca_pem, NULL) != 1)
  //  {
  //    fprintf(stderr, "Could not set the CA file location\n");
  //    goto fail;
  //  }

  /* Load the client's CA file location as well */
  //SSL_CTX_set_client_CA_list(ctx, SSL_load_client_CA_file(ca_pem));

  /* Set the server's certificate signed by the CA */
  //if (SSL_CTX_use_certificate_file(ctx, cert_pem, SSL_FILETYPE_PEM) != 1)
  //  {
  //    fprintf(stderr, "Could not set the server's certificate\n");
  //    goto fail;
  //  }

  /* Load the server's certificate chain file */
  if (SSL_CTX_use_certificate_chain_file(ctx, chain_pem) <= 0)
    {
      fprintf(stderr, "SSL_CTX_use certificate_chain_file failed\n");
      goto fail;
    }

  /* Set the server's key for the above certificate chain */
  if (SSL_CTX_use_PrivateKey_file(ctx, key_pem, SSL_FILETYPE_PEM) <= 0)
    {
      fprintf(stderr, "SSL_CTX_use_PrivateKey_file failed\n");
      goto fail;
    }

  /* We've loaded both certificate and the key, check if they match (NOTE: This is probably unneeded. See https://docs.openssl.org/master/man7/ossl-guide-tls-server-block/#creating-the-ssl_ctx-and-ssl-objects) */
  if (SSL_CTX_check_private_key(ctx) != 1)
    {
      fprintf(stderr, "Server's certificate and the key don't match\n");
      goto fail;
    }

  /* We won't handle incomplete read/writes due to renegotiation */
  SSL_CTX_set_mode(ctx, SSL_MODE_AUTO_RETRY);

  /* Specify that we need to verify the client as well */
  //SSL_CTX_set_verify(ctx,
  //                   SSL_VERIFY_PEER | SSL_VERIFY_FAIL_IF_NO_PEER_CERT,
  //                   NULL);

  /* Do not verify the client */
  SSL_CTX_set_verify(ctx, SSL_VERIFY_NONE, NULL);

  /* We accept only certificates signed only by the CA himself */
  //SSL_CTX_set_verify_depth(ctx, 1);

  /* Done, return the context */
  return ctx;

 fail:
  SSL_CTX_free(ctx);
  return NULL;
}

static BIO * get_socket(const char * const port_str)
{
  /* Create BIO object */
  BIO * const bio = BIO_new_accept(port_str);
  if (bio == NULL)
    {
      fprintf(stderr, "BIO_new_accept failed\n");
      return NULL;
    }

  BIO_set_bind_mode(bio, BIO_BIND_REUSEADDR);
  if (BIO_do_accept(bio) <= 0)
    {
      fprintf(stderr, "BIO_do_accept failed\n");
      goto fail;
    }

  /* Done */
  return bio;
  
 fail:
  BIO_free(bio);
  return NULL;
}

int server(const char * port_str,
           const char * chain_pem,
           const char * key_pem)
{
  unsigned char buffer[BUFSIZE];
  memset(buffer, 0, BUFSIZE);
  SSL_CTX * ctx;
  size_t nread = 0, nwritten = 0, total = 0;

  /* Get a server context for our use */
  if (!(ctx = get_server_context(chain_pem, key_pem)))
    return -1;

  /* Get a socket which is ready to listen on the server's port number */
  BIO * const bio = get_socket(port_str);

  /* Server loop */
  fprintf(stderr, "Listening for connections...\n");
  for (;;)
    {
      if (BIO_do_accept(bio) <= 0)
        continue;

      nread = 0;
      nwritten = 0;
      total = 0;
      BIO * const client = BIO_pop(bio);
      fprintf(stderr, "New client connected.\n");

      SSL * const ssl = SSL_new(ctx);
      if (ctx == NULL)
        {
          fprintf(stderr, "SSL_new failed\n");
          BIO_free(client);
          continue;
        }
      SSL_set_bio(ssl, client, client);

      /* Attempt handshake with client */
      if (SSL_accept(ssl) <= 0)
        {
          fprintf(stderr, "SSL_accept failed\n");
          SSL_free(ssl);
          continue;
        }

      /* Echo client input */
      for (;SSL_read_ex(ssl, buffer, sizeof(buffer), &nread) > 0;)
        {
          fprintf(stderr, "Client said: %s", buffer);
          if (SSL_write_ex(ssl, buffer, nread, &nwritten) > 0 && nwritten == nread)
            {
              total += nwritten;
              memset(buffer, 0, BUFSIZE);
              continue;
            }
          fprintf(stderr, "Error echoing client input.\n");
          break;
        }

      /* Close connection and free memory */
      fprintf(stderr, "Client connection closed, %zu bytes sent\n", total);
      SSL_free(ssl);
    }
  
  SSL_CTX_free(ctx);
  return 0;
}

int client(const char * const server_addr, const char * const port_str)
{
  int result = EXIT_SUCCESS;
  
  /*
   * Create an SSL_CTX which we can use to create SSL objects from. We
   * want an SSL_CTX for creating clients so we use TLS_client_method()
   * here.
   */
  SSL_CTX * const ctx = SSL_CTX_new(TLS_client_method());
  if (ctx == NULL)
    {
      fprintf(stderr, "Failed to create the SSL_CTX\n");
      goto end;
    }

  /*
   * Configure the client to abort the handshake if certificate
   * verification fails. Virtually all clients should do this unless you
   * really know what you are doing.
   */
  //SSL_CTX_set_verify(ctx, SSL_VERIFY_PEER, NULL);
  SSL_CTX_set_verify(ctx, SSL_VERIFY_NONE, NULL);

  /* Use the default trusted certificate store */
  if (!SSL_CTX_set_default_verify_paths(ctx))
    {
      fprintf(stderr, "Failed to set the default trusted certificate store\n");
      goto end;
    } 

  /* Use a custom trusted certificate store */
  //if (SSL_CTX_load_verify_locations(ctx, cert_pem, NULL) <= 0)
  //  {
  //    fprintf(stderr, "SSL_CTX_load_verify_locations failed\n");
  //    goto end;
  //  }

  /*
   * TLSv1.1 or earlier are deprecated by IETF and are generally to be
   * avoided if possible. We require a minimum TLS version of TLSv1.2.
   */
  if (!SSL_CTX_set_min_proto_version(ctx, TLS1_2_VERSION))
    {
      fprintf(stderr, "Failed to set the minimum TLS protocol version\n");
      goto end;
    }

  /* Create an SSL object to represent the TLS connection */
  SSL * const ssl = SSL_new(ctx);
  if (ssl == NULL)
    {
      fprintf(stderr, "Failed to create the SSL object\n");
      goto end;
    }

  int sock = -1;
  BIO_ADDRINFO * res = NULL;
  const BIO_ADDRINFO * ai = NULL;

  /*
   * Lookup IP address info for the server.
   */
  if (!BIO_lookup_ex(server_addr, port_str, BIO_LOOKUP_CLIENT, AF_INET, SOCK_STREAM, 0,
                     &res))
    {
      fprintf(stderr, "BIO_lookup_ex failed\n");
      return 0;
    }
  
  /*
   * Loop through all the possible addresses for the server and find one
   * we can connect to.
   */
  for (ai = res; ai != NULL; ai = BIO_ADDRINFO_next(ai)) {
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
    if (!BIO_connect(sock, BIO_ADDRINFO_address(ai), BIO_SOCK_NODELAY)) {
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
      return 0;
    }
  
  /*
   * Associate the newly created BIO with the underlying socket. By
   * passing BIO_CLOSE here the socket will be automatically closed when
   * the BIO is freed. Alternatively you can use BIO_NOCLOSE, in which
   * case you must close the socket explicitly when it is no longer
   * needed.
   */
  BIO_set_fd(bio, sock, BIO_CLOSE);

  SSL_set_bio(ssl, bio, bio);

  /*
   * Tell the server during the handshake which hostname we are attempting
   * to connect to in case the server supports multiple hosts.
   */
  //if (!SSL_set_tlsext_host_name(ssl, server_addr))
  //  {
  //    fprintf(stderr, "Failed to set the SNI hostname\n");
  //    goto end;
  //  }

  /*
   * Ensure we check during certificate verification that the server has
   * supplied a certificate for the hostname that we were expecting.
   * Virtually all clients should do this unless you really know what you
   * are doing.
   */
  //if (!SSL_set1_host(ssl, server_addr))
  //  {
  //    fprintf(stderr, "Failed to set the certificate verification hostname");
  //    goto end;
  //  }

  /* Do the handshake with the server */
  if (SSL_connect(ssl) < 1)
    {
      fprintf(stderr, "Failed to connect to the server\n");

      /*
       * If the failure is due to a verification error we can get more
       * information about it from SSL_get_verify_result().
       */
      if (SSL_get_verify_result(ssl) != X509_V_OK)
        fprintf(stderr, "Verify error: %s\n",
                X509_verify_cert_error_string(SSL_get_verify_result(ssl)));
      goto end;
    }

  size_t written = 0;
  char buf[BUFSIZE];
  memset(buf, 0, BUFSIZE);
  snprintf(buf, BUFSIZE, "Hello, OpenSSL example!\n");
  const size_t expected_response_bytes = strnlen(buf, BUFSIZE);
  //const char * request_start = "GET / HTTP/1.0\r\nConnection: close\r\nHost: ";
  //const char * request_end = "\r\n\r\n";
  
  /* Write an HTTP GET request to the peer */
  //if (!SSL_write_ex(ssl, request_start, strlen(request_start), &written))
  //  {
  //    fprintf(stderr, "Failed to write start of HTTP request\n");
  //    goto end;
  //  }
  //if (!SSL_write_ex(ssl, server_addr, strlen(server_addr), &written))
  //  {
  //    fprintf(stderr, "Failed to write hostname in HTTP request\n");
  //    goto end;
  //  }
  //if (!SSL_write_ex(ssl, request_end, strlen(request_end), &written))
  //  {
  //    fprintf(stderr, "Failed to write end of HTTP request\n");
  //    goto end;
  //  }
  if (!SSL_write_ex(ssl, buf, strnlen(buf, BUFSIZE), &written))
    {
      fprintf(stderr, "Failed to write to SSL socket\n");
      goto end;
    }
  memset(buf, 0, BUFSIZE);

  size_t readbytes = 0, total_response_bytes = 0;
  
  /*
   * Get up to sizeof(buf) bytes of the response. We keep reading until the
   * server closes the connection.
   */
  for (;SSL_read_ex(ssl, buf, sizeof(buf), &readbytes);) {
    /*
     * OpenSSL does not guarantee that the returned data is a string or
     * that it is NUL terminated so we use fwrite() to write the exact
     * number of bytes that we read. The data could be non-printable or
     * have NUL characters in the middle of it. For this simple example
     * we're going to print it to stdout anyway.
     */
    fwrite(buf, 1, readbytes, stdout);
    total_response_bytes += readbytes;
    if (total_response_bytes >= expected_response_bytes)
      break;
  }
  /* In case the response didn't finish with a newline we add one now */
  fprintf(stdout, "\n");

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
      //fflush(stderr);
      fgets(buf, BUFSIZE, stdin);

      if (feof(stdin))
        {
          fprintf(stderr, "\n");
          break;
        }

      /* Send the data over the wire */
      SSL_write_ex(ssl, buf, strnlen(buf, BUFSIZE), &written);

      /* Receive response */
      memset(buf, 0, BUFSIZE);
    read_more:
      SSL_read_ex(ssl, &buf[readbytes], sizeof(buf) - readbytes, &readbytes);
      if (readbytes != written)
        goto read_more;

      /* Output response */
      fwrite(buf, 1, strnlen(buf, BUFSIZE), stdout);
    }

  /* Close the connection */
  //if (SSL_shutdown(ssl) == 0)
  //  {
  //    //SSL_read(ssl, buf, sizeof(buf));
  //    SSL_shutdown(ssl);
  //  }

  /*
   * Check whether we finished the while loop above normally or as the
   * result of an error. The 0 argument to SSL_get_error() is the return
   * code we received from the SSL_read_ex() call. It must be 0 in order
   * to get here. Normal completion is indicated by SSL_ERROR_ZERO_RETURN.
   */
  //if (SSL_get_error(ssl, 0) != SSL_ERROR_ZERO_RETURN)
  //  {
  //    /*
  //     * Some error occurred other than a graceful close down by the
  //     * peer
  //     */
  //    fprintf(stderr, "Failed reading remaining data\n");
  //    goto end;
  //  }

  /*
   * The peer already shutdown gracefully (we know this because of the
   * SSL_ERROR_ZERO_RETURN above). We should do the same back.
   */
  int ret = SSL_shutdown(ssl);
  if (ret == 0)
    {
      SSL_read(ssl, buf, sizeof(buf));
      //ret = SSL_shutdown(ssl);
      //if (ret < 1)
      //  {
      //    /*
      //     * ret < 0 indicates an error. ret == 0 would be unexpected here
      //     * because that means "we've sent a close_notify and we're waiting
      //     * for one back". But we already know we got one from the peer
      //     * because of the SSL_ERROR_ZERO_RETURN above.
      //     */
      //    fprintf(stderr, "Error shutting down\n");
      //    goto end;
      //  }
    }
  
  /*
   * Free the resources we allocated. We do not free the BIO object here
   * because ownership of it was immediately transferred to the SSL object
   * via SSL_set_bio(). The BIO will be freed when we free the SSL object.
   */
  SSL_free(ssl);
  SSL_CTX_free(ctx);
  
 end:
  /*
   * If something bad happened then we will dump the contents of the
   * OpenSSL error stack to stderr. There might be some useful diagnostic
   * information there.
   */
  if (result == EXIT_FAILURE)
    fprintf(stderr, "Bad exit result\n");
  
  return result;
}

void do_server(int argc, char ** argv)
{
  fprintf(stderr, "This is the server!\n");
  for (int i = 0; i < argc; i++)
    fprintf(stderr, "%s\n", argv[i]);
  fprintf(stderr, "\n");

  if (argc < 4)
    {
      fprintf(stderr, "Usage: %s <port> <certificate chain> <private key>\n", argv[0]);
      return;
    }

  server(argv[1], argv[2], argv[3]);
}

void do_client(int argc, char ** argv)
{
  fprintf(stderr, "This is the client!\n");
  for (int i = 0; i < argc; i++)
    fprintf(stderr, "%s\n", argv[i]);

  if (argc < 3)
    {
      fprintf(stderr, "Usage: %s <server address> <port>\n", argv[0]);
      return;
    }

  client(argv[1], argv[2]);
}
