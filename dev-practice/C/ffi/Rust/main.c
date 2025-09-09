#include <stdlib.h>
#include <stdio.h>

// Function prototype tells the C compiler that the rust_func exists "somewhere", just not in this file!
void rust_func(void);

int main(int argc, char * argv[]) {
  fprintf(stderr, "Main function in C!\n");
  rust_func();
}
