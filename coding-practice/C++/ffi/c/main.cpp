#include <iostream>
#include <dlfcn.h>

int main(void)
{
  void * dl_handle = dlopen("./libfoo.so", RTLD_LAZY);
  if (!dl_handle)
    {
      std::cerr << "Error: " << dlerror() << std::endl;
      return 1;
    }

  int (* const add)(const int, const int) = (int (* const)(const int, const int)) dlsym(dl_handle, "add");
  char * error = dlerror();
  if (error != NULL)
    {
      std::cerr << "Error: " << error << std::endl;
      dlclose(dl_handle);
      return 1;
    }
  
  void (* const out)(const char * const, const unsigned long) = (void (* const)(const char * const, const unsigned long)) dlsym(dl_handle, "out");
  error = dlerror();
  if (error != NULL)
    {
      std::cerr << "Error: " << error << std::endl;
      dlclose(dl_handle);
      return 1;
    }

  int a = 1, b = 2;
  std::cout << a << " + " << b << " = " << add(a, b) << std::endl;

  std::string data = "This is one coooooooooool string, bro.\n";
  out(data.c_str(), data.length());

  dlclose(dl_handle);
}
