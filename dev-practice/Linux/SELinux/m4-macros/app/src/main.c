#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

static int do_app(int, char * []);

int main(int argc, char *argv[]) {
  fprintf(stderr, "This is project %s.\n", PROJECT_NAME);
  return do_app(argc, argv);
}

static int do_app(int argc, char * argv[]) {
  if (argc < 3) {
    fprintf(stderr, "Usage: %s <message> files...\n", argv[0]);
    return -1;
  }

  for (int i = 2; i < argc; i++) {
    FILE *fp = fopen(argv[i], "a+");
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
