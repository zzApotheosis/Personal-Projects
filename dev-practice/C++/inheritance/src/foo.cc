#include <iostream>

#include "foo.h"

void
library_function (const int x)
{
  fprintf (stderr, "Library function received integer: %d\n", x);
}

template <typename D> Foo<D>::Foo (void) : data (0)
{
  std::cerr << "Creating new Foo object" << std::endl;
}

template <typename D> Foo<D>::~Foo (void)
{
  std::cerr << "Destroying Foo object" << std::endl;
}

template <typename D> void Foo<D>::set_data(D const & data) {
  this->data = data;
}

template class Foo<int>;
