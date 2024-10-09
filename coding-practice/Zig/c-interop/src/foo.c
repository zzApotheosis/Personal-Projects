#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <time.h>

void c_foo(const unsigned int x) {
  fprintf(stdout, "Hello from C!\n");
  const char * msg = NULL;
  if (x < 60) {
    msg = "No offense, but you suck.";
  } else if (x < 70) {
    msg = "Do better next time.";
  } else if (x < 80) {
    msg = "Not bad, but could be better.";
  } else if (x < 90) {
    msg = "Good job!";
  } else if (x < 100) {
    msg = "Outstanding!";
  } else if (x < 110) {
    msg = "Okay, now you're just showing off.";
  } else if (x == 110) {
    msg = "You really had to prove something, didn't you... freaking nerd.";
  }
  fprintf(stdout, "%u? %s\n", x, msg);
}
