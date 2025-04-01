#include <iostream>

#include "foo.hpp"

#include <string>

namespace Foo {

  template<typename T>
  class MyClass {
  private:
    T data;
    std::string name;

  public:
    MyClass(const T data, const std::string name) :
      data(data),
      name(name)
    {
      std::cout << "Entering constructor for MyClass!" << std::endl;
    }
    virtual ~MyClass(void)
    {
      std::cout << "Entering destructor for MyClass!" << std::endl;
    }

    T & get_data(void)
    {
      return this->data;
    }

    std::string & get_name(void)
    {
      return this->name;
    }
  };

}

void cpp_func(void)
{
  Foo::MyClass<int> obj = Foo::MyClass(42, "Alice");
  std::cout << "obj.get_data() = " << obj.get_data() << std::endl;
  std::cout << "obj.get_name() = " << obj.get_name() << std::endl;

  Foo::MyClass<std::string> obj2 = Foo::MyClass(std::string("Template data!"), "Bob");
  std::cout << "obj2.get_data() = " << obj2.get_data() << std::endl;
  std::cout << "obj2.get_name() = " << obj2.get_name() << std::endl;
}
