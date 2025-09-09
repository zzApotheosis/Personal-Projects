# Rust foreign function interface

Rust is capable of exporting functions with a C-style ABI. This example shows how to compile code written in Rust and use that code in C.

# Building this example

## Static Linking
> $ `rustc --crate-type staticlib foo.rs`  
> $ `clang -omain main.c libfoo.a`
> $ `./main`

## Dynamic Linking
> $ `rustc --crate-type dylib foo.rs`
> $ `clang main.c -L. -lfoo -rpath '$ORIGIN' -o main`
> $ `./main`
