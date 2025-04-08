#include <Python.h>

int main(void)
{
  // Initialize the Python interpreter
  Py_Initialize();

  if (Py_IsInitialized())
    {
      // Execute a Python statement
      PyRun_SimpleString("print('Hello from Python!')");

      // Start giving the interpreter keyboard input!
      PyRun_SimpleString("print('The Python interpreter is now reading for input. Press Enter, Ctrl-D when finished.')");
      PyRun_SimpleFile(stdin, "stdin");

      // Clean up and finalize the Python interpreter
      Py_Finalize();
    }
  else
    {
      return 1;
    }
}
