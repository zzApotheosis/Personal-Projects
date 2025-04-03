#include <stdio.h>
#include <stdint.h>

int main(void)
{
  // Declare and initialize an integer within its valid range.
  int32_t my_integer = 10;

  // Declare and initialize a constant string literal.
  const char * const my_string = "Hello, safe world!";

  // Check if the integer is positive before performing an operation.
  if (my_integer > 0)
    {
      int32_t result = my_integer * 2;
      printf("Result: %d\n", result);
    }
  else
    {
      printf("Integer is not positive.\n");
    }

  // Print the string.
  printf("%s\n", my_string);

  // Return success.
  return 0;
}
