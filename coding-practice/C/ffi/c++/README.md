# C++ foreign function interface

C++ - being a sophisticated extension of C - is capable of exporting compatible functions with a C-style ABI. This example shows how to compile code written in C++ and use that code in C.

# Building this example

> $ clang++ -shared -fPIC -o libfoo.so foo.cpp  
> $ clang -o main main.c  
> $ clang -L. -lfoo -o direct-access direct-access.c  
> $ ./main  
> $ LD_LIBRARY_PATH=. ./direct-access