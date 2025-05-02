# Shared Libraries in Go
While this isn't a primary feature of Go, the Go compiler supports building shared libraries.

# Building and running this example
> $ go build -buildmode=c-shared -o libfoo.so foo.go
> $ chmod +x libfoo.so
> $ clang -o main main.c
> $ ./main

Note: This example code is intended to work on Unix-like systems only. This will not work on Windows.

You may notice that this example could automatically produce a C header file after invoking the Go compiler to build this shared object. This can be used in C programs to invoke the functions exported by the Go shared object.
However, in this example, we use the dlfcn.h standard header to dynamically load the shared object by path.