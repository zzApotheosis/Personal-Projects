#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdint.h>

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

void _execvp(void)
{
  char * cmd = "sbcl";
  char * args[] = {cmd, NULL};
  int result = execvp(cmd, args);
  if (result != 0)
    {
      fprintf(stderr, "Error occurred during execvp(): %s\n", strerror(errno));
    }
}

typedef int32_t T;

static T (* invoke_f)(const T, const T) = NULL;

static T add(const T a, const T b)
{
  return a + b;
}

static T sub(const T a, const T b)
{
  return a - b;
}

static T mul(const T a, const T b)
{
  return a * b;
}

static T div(const T a, const T b)
{
  return a / b;
}

static T mod(const T a, const T b)
{
  return a % b;
}

/* Do NOT use "select" for this function name. Apparently the "select" symbol exists somewhere in the compiled code and SBCL finds that symbol before this one. */
void _select(const uint32_t f)
{
  switch (f)
    {
    case 0:
      invoke_f = add;
      break;
    case 1:
      invoke_f = sub;
      break;
    case 2:
      invoke_f = mul;
      break;
    case 3:
      invoke_f = div;
      break;
    case 4:
      invoke_f = mod;
      break;
    default:
      fprintf(stderr, "Invalid selection: %u\n", f);
      break;
    }
}

T invoke(const T a, const T b)
{
  T t = invoke_f(a, b);
  return t;
}

void process_void_ptr(void * const p)
{
  fprintf(stderr, "p = %p\n", p);
  fprintf(stderr, "*p = %u\n", * (uint8_t * const) p);

  // Obviously this is terrible code. The function takes a void constant pointer and casts it to uint8_t constant pointer before dereferencing, all with zero error handling and checks
  // This is just an example to show that memory can be passed between SBCL (ANSI Common Lisp) and C code with no issues
  // This opens an entire world of possibilities with Common Lisp code
}
