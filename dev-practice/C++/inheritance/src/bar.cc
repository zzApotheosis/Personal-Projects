#include <iostream>

#include "bar.h"

template <typename D> Bar<D>::Bar (void) : more_data (0)
{
  std::cerr << "Creating new Bar object" << std::endl;
}

template <typename D> Bar<D>::~Bar (void)
{
  std::cerr << "Destroying Bar object" << std::endl;
}

template <typename D>
void
Bar<D>::do_something (void)
{
  std::cerr << "This is where we do something with the Bar object!"
            << std::endl;
  std::cerr << "We did something! " << this->more_data << std::endl;
}

template <typename D>
void
Bar<D>::set_more_data (const D d)
{
  this->more_data = d;
}

template class Bar<int>;
