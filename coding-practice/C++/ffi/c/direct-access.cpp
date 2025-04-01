#include <iostream>
#include <string>

#include "foo.h"

using namespace std;

int main(void)
{
  int a = 10, b = 32;
  cout << a << " + " << b << " = " << add(a, b) << endl;

  std::string data = "That's some nice code you got there. Would be a shame if I SIGSEGV.\n";
  out(data.c_str(), data.length());
}
