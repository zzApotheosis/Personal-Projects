# C++ foreign function interface

C++ - being a sophisticated extension of C - is capable of exporting compatible functions with a C-style ABI. This example shows how to compile code written in C++ and use that code in C.

# Building this example

> $ `clang++ foo.cpp -o libfoo.so -shared -fPIC`  
> $ `clang main.c -o main`  
> $ `clang direct-access.c -o direct-access -L. -lfoo -Wl,-rpath='$ORIGIN:$ORIGIN/../lib'`  
> $ `./main`  
> $ `./direct-access`
