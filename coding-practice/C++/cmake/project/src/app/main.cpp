#include <iostream>

#include <foo.hpp>

using namespace std;

int main(const int argc, const char * const * const argv)
{
  for (int i = 0; i < argc; i++)
    {
      cerr << "argv[" << i << "] = " << argv[i] << endl;
    }

  Foo<unsigned long long int> foo;
  for (unsigned long long int i = 0; i < 100; i++)
    {
      const int j = foo.foo();
      std::cerr << "foo.foo() = " << j << std::endl;
    }

  std::cerr << "Resetting foo..." << std::endl;
  foo.reset();
  std::cerr << "foo.foo() = " << foo.foo() << std::endl;
}
