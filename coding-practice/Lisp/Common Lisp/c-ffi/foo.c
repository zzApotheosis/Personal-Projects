/*
 * Try modifying these functions to see how they affect the Lisp implementation :)
 */

int my_add(const int a, const int b) {
  return a + b;
}

static int data = 0;

void set_data(const int a) {
  data = a;
}

int get_data(void) {
  return data;
}
