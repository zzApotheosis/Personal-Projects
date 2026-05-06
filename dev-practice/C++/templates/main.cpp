#include <iostream>
#include <vector>

/* Template functions */
template <typename T>
T
max (const T &a, const T &b)
{
  return (a > b) ? a : b;
}

/* Template class */
template <typename T> class Stack
{
private:
  std::vector<T> stack;

public:
  void
  push (T const &e)
  {
    stack.push_back (e);
  }

  T
  pop (void)
  {
    if (isEmpty ())
      throw std::out_of_range ("Stack is empty");
    T e = stack.back ();
    stack.pop_back ();
    return e;
  }

  bool
  isEmpty (void) const
  {
    return stack.empty ();
  }

  T
  top () const
  {
    if (isEmpty ())
      throw std::out_of_range ("Stack is empty");
    return stack.back ();
  }
};

int
main (void)
{
  /* Utilize the template function */
  std::cerr << "max(5, 10) = " << max (5, 10) << std::endl;
  std::cerr << "max(4.2, 3.14) = " << max (4.2, 3.14) << std::endl;
  std::cerr << "max('b', 'B') = " << max ('b', 'B') << std::endl;

  /* Utilize the template class */
  Stack<int> int_stack;
  int_stack.push (42);
  int_stack.push (17);
  std::cerr << "Top of int_stack: " << int_stack.top () << std::endl;
  std::cerr << "Popped from int_stack: " << int_stack.pop () << std::endl;
  std::cerr << "Top of int_stack after pop: " << int_stack.top () << std::endl;

  Stack<std::string> string_stack;
  string_stack.push ("Hello");
  string_stack.push ("World");
  std::cerr << "Top of string_stack: " << string_stack.top () << std::endl;
  std::cerr << "Popped from string_stack: " << string_stack.pop ()
            << std::endl;
  std::cerr << "Top of string_stack after pop: " << string_stack.top ()
            << std::endl;

  return 0;
}
