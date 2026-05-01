#include <Python.h>

int main(void)
{
  // Initialize the Python interpreter
  Py_Initialize();

  if (!Py_IsInitialized())
    goto bad_py_initialize;

  FILE * fp = NULL;
  fp = fopen("test-file.enc", "r");
  if (fp == NULL)
    goto bad_fopen;
      
  // Execute a Python statement
  PyRun_SimpleString("print('Hello from Python!')");

  // Start giving the interpreter keyboard input!
  // PyRun_SimpleString("print('The Python interpreter is now reading for input. Press Enter, Ctrl-D when finished.')");
  // PyRun_SimpleFile(stdin, "stdin");
  PyRun_SimpleFile(fp, "cipher-prog");

  // Clean up and finalize the Python interpreter
  Py_Finalize();

  return 0;
      
 bad_fopen:
  Py_Finalize();
 bad_py_initialize:
  return 1;
}
