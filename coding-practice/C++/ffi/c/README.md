# C++ foreign function interface

C++ - being a sophisticated extension of C - is capable of using compatible functions with a C-style ABI. This example shows how to compile code written in C and use that code in C++.

# Building this example

> $ clang -shared -fPIC -o libfoo.so foo.c
> $ clang++ -o main main.cpp
> $ ./main