//#include <bar.hpp>

#include "foo.hpp"

template <typename D>
Foo<D>::Foo(void)
{
  this->data = 0;
}

template <typename D>
Foo<D>::~Foo(void)
{
  // Destructor
}

template <typename D>
void Foo<D>::reset(void)
{
  this->data = 0;
}

template <typename D>
D Foo<D>::foo(void)
{
  this->data += 1;
  return this->data;
}

// Explicit instantiation (Usually, template classes are implemented entirely in header files)
template class Foo<unsigned long long int>;
