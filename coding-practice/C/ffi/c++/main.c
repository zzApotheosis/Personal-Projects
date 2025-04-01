#include <stdio.h>
#include <dlfcn.h>

int main(void)
{
  void * dl_handle = dlopen("./libfoo.so", RTLD_LAZY);
  if (!dl_handle)
    {
      fprintf(stderr, "Error: %s\n", dlerror());
      return 1;
    }

  dlerror();

  void (* cpp_func)(void) = dlsym(dl_handle, "cpp_func");
  char * error = dlerror();
  if (error != NULL)
    {
      fprintf(stderr, "Error: %s\n", error);
      return 1;
    }
  
  cpp_func();

  dlclose(dl_handle);
}
