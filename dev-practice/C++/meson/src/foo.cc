#include <iostream>

#include "foo.h"

void
library_function (const int x)
{
  std::cerr << "Library function received integer: " << x << std::endl;

  Foo foo;
  foo.do_something();
}

Foo::Foo (void) { std::cerr << "Creating new Foo object" << std::endl; }

Foo::~Foo (void) { std::cerr << "Destroying Foo object" << std::endl; }

void
Foo::do_something (void)
{
  std::cerr << "This is where we do something!" << std::endl;
}
