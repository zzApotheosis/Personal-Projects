#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "foo.h"

void library_function(const int x) {
  fprintf(stderr, "Library function received integer: %d\n", x);
}

int do_app(int argc, char *argv[]) {
  if (argc < 3) {
    fprintf(stderr, "Usage: %s <message> files...\n", argv[0]);
    return -1;
  }

  for (int i = 2; i < argc; i++) {
    FILE *fp = fopen(argv[i], "w");
    if (fp == NULL) {
      fprintf(stderr, "Failed to open file: %s, %s\n", argv[i],
              strerror(errno));
      continue;
    }

    fwrite(argv[1], 1, strlen(argv[1]),
           fp); // Don't care about errors here (but probably should)

    fclose(fp);
  }

  return 0;
}
