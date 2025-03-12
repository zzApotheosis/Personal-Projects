#ifndef FOO_HPP
#define FOO_HPP

#include <string>

namespace Foo {
  class MyClass {
  public:
    MyClass(void);
    MyClass(const std::string, const int);
    virtual ~MyClass(void);

    /* Operators */

    /*
     * NOTE: Something I just learned is that methods themselves can be marked "const".
     * That's what the const keywords below are for. The first "const" on each line
     * marks the MyClass references as immutable (constant), and the second "const"
     * marks the methods themselves as constant, which basically means that the methods
     * are not allowed to modify instance data, but are allowed to read instance data.
     * It's the C++ way to commmunicate to the compiler and other developers that this
     * method is read-only.
     *
     * On the flipside, the "mutable" keyword in instance data communicates to the
     * compiler that the marked data is allowed to be modified by "const"-marked
     * methods. It gets confusing at first, which is why one of the biggest
     * criticisms of C++ is how complicated its syntax/semantics are.
     */
    bool operator >(const MyClass &) const;
    bool operator <(const MyClass &) const;
    bool operator >=(const MyClass &) const;
    bool operator <=(const MyClass &) const;
    bool operator ==(const MyClass &) const;
    bool operator !=(const MyClass &) const;

  private:
    std::string name;
    int data;
    mutable int mut_data;
  };
}

#endif
