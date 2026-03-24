#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <signal.h>

/*
 * Generally, the sigaction() function is preferred over the signal() function.
 *
 * See the sigaction(3P) man page for more information.
 */

static void signal_handler(int signo, siginfo_t * info, void * context) {
  (void) info;
  (void) context;
  switch(signo) {
    case SIGINT:
      write(STDOUT_FILENO, "Caught SIGINT!\n", 15);
      break;
    case SIGTERM:
      write(STDOUT_FILENO, "Caught SIGTERM!\n", 16);
      break;
    default:
      write(STDOUT_FILENO, "Caught some signal\n", 19);
      break;
  }
  _exit(EXIT_SUCCESS);
}

int main(void) {
  struct sigaction s;
  sigemptyset(&s.sa_mask);
  s.sa_sigaction = signal_handler;
  s.sa_flags = SA_SIGINFO;

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
