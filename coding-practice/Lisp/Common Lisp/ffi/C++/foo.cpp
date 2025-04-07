#include <cstdlib>
#include <iostream>

#include "foo.hpp"

namespace Foo {
  MyClass::MyClass() :
    ligma_count(0)
  {
    std::cerr << "Making new MyClass!" << std::endl;
  }

  MyClass::~MyClass()
  {
    std::cerr << "Fricking killing MyClass :( You bastard" << std::endl;
  }

  void MyClass::ligma()
  {
    std::cerr << "Ligma balls lmao" << std::endl;
    ligma_count++;
    std::cerr << "You liggedma balls " << ligma_count << " times" << std::endl;
  }

  int MyClass::bad_add(const int a, const int b)
  {
    return a + corrupt(b);
  }

  int MyClass::corrupt(const int n)
  {
    return n + 1;
  }
}

extern "C" {
  void cpp_ffi(void)
  {
    Foo::MyClass my_class;

    my_class.ligma();
    my_class.ligma();
    my_class.ligma();
    std::cerr << "What's 60 + 9? Let's ask MyClass: " << my_class.bad_add(60, 9) << std::endl;
    std::cerr << "Wait what? That's not correct? Fricking bastards" << std::endl;
  }
}
