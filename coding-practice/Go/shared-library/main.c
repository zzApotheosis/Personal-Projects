#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>

int main(void)
{
  void * handle = NULL;
  int (* Add)(int, int) = NULL;
  char * (* Greet)(char *) = NULL;
  char * error = NULL;

  handle = dlopen("./libfoo.so", RTLD_LAZY);
  if (!handle)
    {
      fprintf(stderr, "%s\n", dlerror());
    }

  // See if "Add" exists in the shared object
  Add = (int (*)(int, int)) dlsym(handle, "Add");
  error = dlerror();
  if (error != NULL)
    {
      fprintf(stderr, "%s\n", error);
      dlclose(handle);
      return 1;
    }

  // See if "Greet" exists in the shared object
  Greet = (char * (*)(char *)) dlsym(handle, "Greet");
  error = dlerror();
  if (error != NULL)
    {
      fprintf(stderr, "%s\n", error);
      dlclose(handle);
      return 1;
    }

  // Call the functions
  int a = 60, b = 9;
  int result = Add(a, b);
  fprintf(stdout, "%d + %d = %d\n", a, b, result);

  char name[] = "Steven";
  char * greeting = Greet(name);
  fprintf(stdout, "%s\n", greeting);
  free(greeting);

  dlclose(handle);
  return 0;
}
