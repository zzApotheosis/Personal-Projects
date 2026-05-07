#include <iostream>

#include "foo.h"

int main(void)
{
  std::cerr << "This is project " << PROJECT_NAME << std::endl;

  library_function(42);
}
