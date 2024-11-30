# README

While it's important to understand how a build system works (such as Zig's Zig Build System,
Rust's Cargo, CMake, Meson, Gradle, etc.), it's equally as important to understand how to build
something from scratch.

Similar to the multi-file example projects under the Rust directory, these Zig projects can be
built by hand.

> $ $CC -c -fPIC foo.c  
> $ $AR rcs libfoo.a foo.o  
> $ zig build-exe -I. -L. -lfoo -lc main.zig  
> $ zig build-exe -I. -L. -lfoo -lc c-structs.zig

Note: the `$CC` and `$AR` variables can be replaced by any C compiler and archiver, respectively.
It's possible to use toolchains from different vendors/projects. For instance, we could use
the built-in C compiler in the Zig compiler to achieve C compilation, and we could use GNU
Archiver to create the static library, and vice versa.

In this example, we compile with GCC, archive with LLVM, and link with Zig.

> $ gcc -c fPIC foo.c  
> $ llvm-ar rcs libfoo.a foo.o  
> $ zig build-exe -I. -L. -lfoo -lc main.zig  
> $ zig build-exe -I. -L. -lfoo -lc c-structs.zig
