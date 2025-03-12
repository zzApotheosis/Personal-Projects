#include "foo.hpp"

namespace Foo {
  MyClass::MyClass() :
    name(""),
    data(0),
    mut_data(0)
  {
    // Constructor
  }

  MyClass::MyClass(const std::string name, const int data) :
    name(name),
    data(data),
    mut_data(0)
  {}
  
  MyClass::~MyClass()
  {
    // Destructor
  }

  bool MyClass::operator >(const MyClass & other) const
  {
    mut_data = 1;
    return this->data > other.data;
  }

  bool MyClass::operator <(const MyClass & other) const
  {
    mut_data = 2;
    return this->data < other.data;
  }

  bool MyClass::operator >=(const MyClass & other) const
  {
    mut_data = 3;
    return this->data >= other.data;
  }

  bool MyClass::operator <=(const MyClass & other) const
  {
    mut_data = 4;
    return this->data <= other.data;
  }

  bool MyClass::operator ==(const MyClass & other) const
  {
    mut_data = 5;
    return this->data == other.data;
  }

  bool MyClass::operator !=(const MyClass & other) const
  {
    mut_data = 6;
    return this->data != other.data;
  }
}
