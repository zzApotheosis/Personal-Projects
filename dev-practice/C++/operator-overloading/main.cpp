#include <iostream>

#include "foo.hpp"

#define MY_COND(c)					\
  do {							\
    if (c)						\
      {							\
	std::cerr << #c << " is true" << std::endl;	\
      }							\
    else						\
      {							\
	std::cerr << #c << " is false" << std::endl;	\
      }							\
  } while (0)

int main(void)
{
  Foo::MyClass a("Steven", 94);
  Foo::MyClass b("Ligma", 42);

  std::cerr << "a = Foo::MyClass(\"Steven\", 94)" << std::endl;
  std::cerr << "b = Foo::MyClass(\"Ligma\", 42)" << std::endl;

  MY_COND(a > b);
  MY_COND(a < b);
  MY_COND(a >= b);
  MY_COND(a <= b);
  MY_COND(a == b);
  MY_COND(a != b);
  
  return 0;
}
