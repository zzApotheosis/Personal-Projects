# Python foreign function interface

## Building this example

> $ clang -o main $(pkg-config --cflags python) main.c $(pkg-config --libs python3-embed)  
> $ ./main
