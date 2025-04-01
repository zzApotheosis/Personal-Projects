#include <iostream>

#include "foo.hpp"

#include <string>

namespace Foo {

  template<typename D, typename N>
  class MyClass {
  private:
    D data;
    std::string name;

  public:
    MyClass(const D d, const N n) :
      data(d),
      name(n)
    {
      std::cout << "Entering constructor for MyClass!" << std::endl;
    }
    
    virtual ~MyClass(void)
    {
      std::cout << "Entering destructor for MyClass!" << std::endl;
    }

    D & get_data(void)
    {
      return this->data;
    }

    N & get_name(void)
    {
      return this->name;
    }
  };

}

using namespace std;

void cpp_func(void)
{
  Foo::MyClass<int, string> obj = Foo::MyClass(42, string("Alice"));
  cout << "obj.get_data() = " << obj.get_data() << endl;
  cout << "obj.get_name() = " << obj.get_name() << endl;

  obj.get_data() += 10;
  cout << "obj.get_data() = " << obj.get_data() << endl;
  cout << "obj.get_name() = " << obj.get_name() << endl;

  Foo::MyClass<string, string> obj2 = Foo::MyClass(string("Template data!"), string("Bob"));
  cout << "obj2.get_data() = " << obj2.get_data() << endl;
  cout << "obj2.get_name() = " << obj2.get_name() << endl;

  obj2.get_data() = "Updated template data!";
  cout << "obj2.get_data() = " << obj2.get_data() << endl;
  cout << "obj2.get_name() = " << obj2.get_name() << endl;
}
