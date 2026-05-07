#include <iostream>

#include "bar.h"

int main(void)
{
  std::cerr << "This is project " << PROJECT_NAME << std::endl;

  Bar<int> bar;
  bar.do_something();
  bar.set_more_data(1);
  bar.do_something();
}
