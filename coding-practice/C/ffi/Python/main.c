#include <Python.h>

int main(void)
{
  // Initialize the Python interpreter
  Py_Initialize();

  if (Py_IsInitialized())
    {
      // Execute a Python statement
      PyRun_SimpleString("print('Hello from Python!')");

      // Clean up and finalize the Python interpreter
      Py_Finalize();
    }
  else
    {
      return 1;
    }
}
