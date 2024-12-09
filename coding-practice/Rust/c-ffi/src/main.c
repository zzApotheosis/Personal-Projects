#include <stdlib.h>
#include <stdio.h>

/* Implemented in Rust! :) */
int add(const int, const int);

int char_valid(const char c) {
  if (c >= '0' && c <= '9')
    return 1;
  return 0;
}

int parse_int(const char * const s) {
  if (s == NULL)
    abort();
  int is_negative = 0;
  int previous = 0;
  int current = 0;
  const char * index = s;
  if (*index == '-') {
    is_negative = 1;
    index += 1;
  }
  while (*index != '\0') {
    if (!char_valid(*index))
      abort(); /* digits only */
    previous = current;
    current *= 10; /* shift decimal digits 1 to the left */
    current += *index - '0';
    if (previous > current)
      abort(); /* buffer overflow */
    index += 1;
  }
  if (is_negative)
    current *= -1;
  return current;
}

void usage(const char * const arg0) {
  fprintf(stderr, "Usage: %s <num> <num>\n", arg0);
}

void format_expression(const int a, const int b, const int n) {
  char operator = '+';
  int format_a = a;
  int format_b = b;
  if (b < 0) {
    operator = '-';
    format_b *= -1;
  }
  fprintf(stderr, "%d %c %d = %d\n", format_a, operator, format_b, n);
}

int main(const int argc, const char * const * const argv) {
  if (argc < 3) {
    usage(argv[0]);
    return EXIT_FAILURE;
  }

  int a = parse_int(argv[1]);
  int b = parse_int(argv[2]);

  format_expression(a, b, add(a, b));
  
  return EXIT_SUCCESS;
}
