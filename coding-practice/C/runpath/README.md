# RUNPATH, RPATH example

On Unix-like systems, executables that are built with modern linkers such as GNU's `ld` and LLVM's `lld` support the option to specify a custom RPATH or RUNPATH to allow the executable file to convey special information to the dynamic loader advising how it should search for its required runtime libraries. This is a powerful mechanism for binary distribution and dynamic library loading.

## $ORIGIN

$ORIGIN is a special variable in the RPATH/RUNPATH that resolves to the directory that the executable file is located in when the dynamic loader searches the RPATH/RUNPATH.

## Building and running this example

This is an example to build the code so that it only depends on RPATH/RUNPATH.

> $ `clang foo.c -o libfoo.so -shared -fPIC`  
> $ `clang main.c -o main -L. -lfoo -Wl,-rpath='$ORIGIN:$ORIGIN/../lib:/tmp:/other/custom/path'`  
> $ `objdump -x main | grep -i path`  
> $ `./main`

This is an example to build the code so that it requires LD\_LIBRARY\_PATH to run.

> $ `clang foo.c -o libfoo.so -shared -fPIC`  
> $ `clang main.c -o main -L. -lfoo`  
> $ `objdump -x main | grep -i path`  
> There should be neither RPATH nor RUNPATH!  
> $ `./main`  
> `./main` should fail because the dynamic loader can't find its required libraries to run. We can fix it by specifying LD\_LIBRARY\_PATH.  
> $ `LD\_LIBRARY\_PATH=. ./main`
