# README

It is possible to compile and link this code by hand. In a real-world project, it would be "better" to automate the compilation and linking of code, but it is very useful and powerful to understand
how to build software by hand with given tools such as a compiler, archiver, and linker.

## How to build this example by hand

Note: This example is for Linux systems.

> $ gcc -c -fPIC cool.c  
> $ ar rcs libcool.a cool.o  
> $ rustc -L. -lstatic=cool -lgmp -orust-calls-c main.rs  
> $ ./rust-calls-c

Additionally, we can build the C example by hand.

> $ rustc --crate-type staticlib cool.rs
> $ gcc -oc-calls-rust main.c libcool.a
> $ ./c-calls-rust
