#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <signal.h>

/*
 * Generally, the sigaction() function is preferred over the signal() function.
 *
 * See the sigaction(3P) man page for more information.
 */

static void signal_handler(int s) {
  fprintf(stdout, "\n");
  switch(s) {
    case SIGINT:
      fprintf(stdout, "Caught SIGINT!\n");
      break;
    case SIGTERM:
      fprintf(stdout, "Caught SIGTERM!\n");
      break;
    default:
      fprintf(stdout, "Caught some signal (%d)\n", s);
      break;
  }
  exit(EXIT_SUCCESS);
}

int main(void) {
  struct sigaction s;
  s.sa_handler = signal_handler;
  sigemptyset(&(s.sa_mask));
  //sigemptyset(&s.sa_mask); // This is valid too
  s.sa_flags = SA_RESTART;

  if (sigaction(SIGINT, &s, NULL) != 0) {
    fprintf(stderr, "ERROR: Unable to assign SIGINT handler\n");
  }
  if (sigaction(SIGTERM, &s, NULL) != 0) {
    fprintf(stderr, "ERROR: Unable to assign SIGTERM handler\n");
  }
  if (sigaction(SIGQUIT, &s, NULL) != 0) {
    fprintf(stderr, "ERROR: Unable to assign SIGQUIT handler\n");
  }

  /*
   * Note: Try stopping this process by sending a SIGQUIT signal
   */

  fprintf(stdout, "Press Ctrl-C to quit\n");
  while (1) {
    usleep(1000000);
  }

  return EXIT_SUCCESS;
}
