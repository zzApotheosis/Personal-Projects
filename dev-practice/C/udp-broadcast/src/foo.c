#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <arpa/inet.h>
#include <sys/socket.h>

#include "foo.h"

#define BUFFER_SIZE 1024
#define BROADCAST_PORT 12345

#define DIE(x) \
  do {         \
    perror(x); \
    abort();   \
  } while (0);

static void parse_address_port(const char * const s, char * const a, int * const p)
{
  const char * const colon_pos = strchr(s, ':');

  if (colon_pos)
    {
      const size_t address_len = colon_pos - s;
      strncpy(a, s, address_len);
      a[address_len] = '\0';
      *p = (int) strtol(colon_pos + 1, NULL, 10);
    }
  else
    strcpy(a, s);
}

void do_udp_broadcast_client(int argc, char ** argv)
{
  if (argc < 2)
    {
      fprintf(stderr, "Usage: %s <broadcast address>[:port]\n", argv[0]);
      return;
    }

  int sock = 0;
  struct sockaddr_in broadcast_address;
  char message[] = "Hello, world!";
  int broadcast_enable = 1;
  int port = BROADCAST_PORT;
  char address_buffer[64];
  memset(address_buffer, 0, sizeof(address_buffer));

  /* Parse address */
  parse_address_port(argv[1], address_buffer, &port);

  /* Create UDP socket */
  sock = socket(AF_INET, SOCK_DGRAM, 0);
  if (sock < 0)
    DIE("socket()");

  /* Enable broadcast socket option */
  if (setsockopt(sock, SOL_SOCKET, SO_BROADCAST, &broadcast_enable, sizeof(broadcast_enable)) < 0)
    DIE("setsockopt()");

  /* Configure broadcast address and port */
  memset(&broadcast_address, 0, sizeof(struct sockaddr_in));
  broadcast_address.sin_family = AF_INET;
  broadcast_address.sin_port = htons(port);
  if (inet_pton(AF_INET, address_buffer, &broadcast_address.sin_addr) <= 0)
    DIE("inet_pton()");

  /* Send the broadcast message */
  if (sendto(sock, message, sizeof(message), 0, (struct sockaddr *) &broadcast_address, sizeof(struct sockaddr_in)) < 0)
    DIE("sendto()");

  /* Close socket */
  close(sock);
}

void do_udp_broadcast_server(int argc, char ** argv)
{
  int sock = 0;
  struct sockaddr_in server_address, client_address;
  char buffer[BUFFER_SIZE];
  socklen_t client_len = sizeof(struct sockaddr_in);
  int port = BROADCAST_PORT;
  if (argc > 1)
    port = atoi(argv[1]);

  /* Create UDP socket */
  sock = socket(AF_INET, SOCK_DGRAM, 0);
  if (sock < 0)
    DIE("socket()");

  /* Configure server address and port */
  memset(&server_address, 0, sizeof(struct sockaddr_in));
  server_address.sin_family = AF_INET;
  server_address.sin_addr.s_addr = INADDR_ANY;
  server_address.sin_port = htons(port);

  /* Bind to the socket */
  if (bind(sock, (struct sockaddr *) &server_address, sizeof(struct sockaddr_in)) < 0)
    DIE("bind()");

  /* Listen for incoming broadcast messages */
  for (;;)
    {
      ssize_t bytes_received = recvfrom(sock, buffer, BUFFER_SIZE - 1, 0, (struct sockaddr *) &client_address, &client_len);
      if (bytes_received < 0)
        DIE("recvfrom()");
      buffer[bytes_received] = '\0';
      fprintf(stderr, "[%s:%d] %s\n", inet_ntoa(client_address.sin_addr), ntohs(client_address.sin_port), buffer);
    }
}
