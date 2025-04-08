# Building C Programs By Hand

While the responsibility of translating human-readable source code to machine code is almost entirely handled by your chosen compiler (`gcc`, `clang`, or other), it is important to know and understand each of the smaller steps that the compiler actually performs to convert your code to a format that the machine understands. In this context, what "the machine understands" is different depending on the instruction set that the CPU provides and supports, and its memory addressing scheme (32-bit vs 64-bit vs other architectures). This tiny example project is intended to show how a compiler toolchain such as `GCC` or `LLVM` process source code into useful machine instructions.

## Basic Principles

There are a few details that this example illustrates.

1. The Preprocessor
2. The Compiler
3. The Assembler
4. The Linker

### Preprocessor

The job of the preprocessor is to analyze the source code and output an intermediary representation of the source code that replaces compiler directives with appropriate substitutions. This includes things like `#define`, `#include`, `#ifdef`, and `#endif`. It also includes "built-in" macros such as `__func__`, `__file__`, and `__line__`.

### Compiler

In this context, the job of the compiler is to convert the intermediary representation of the source code to a representation of the code that an assembler can process. Most of the time, software engineers would prefer to just skip this step and have the compiler perform preprocessing, compiling, and assembling into machine code all in one convenient step. But this is intended to show what the compiler is actually doing.

### Assembler

The job of the assembler is to convert source code in its final human-readable form to Assembly for a given CPU instruction set. Arguably, Assembly language is the last "human-readable" representation of instructions before it is finally converted to machine code, but most people would prefer to not have to bother with Assembly. That's why compilers exist.

### Linker

The job of the linker is to analyze compiled object files for the binary symbols that they contain (and export), and determine how to "map" or "link" the addresses contained in each binary object file to each other such that the final output is a file that is ready to load into memory for execution. This final representation of the source code can be in two commonly used forms: executables and libraries.

## LLVM Toolchain

This shows how to use the LLVM toolchain to preprocess, compile, assemble, and link this example project.

### Preprocessing

This converts the original source code into preprocessed code.

- `clang -E main.c -o main.i`
- `clang -E foo.c -o foo.i`

### Compiling

These next steps convert the preprocessed code into Assembly.

- `clang -S main.i -o main.s`
- `clang -S foo.i -o foo.s`

### Assembling

The LLVM assembler converts the Assembly source text into machine code.

- `llvm-mc -filetype=obj main.s -o main.o`
- `llvm-mc -filetype=obj foo.s -o foo.o`

### Linking

Finally, the linker maps memory spaces together so the final output is ready to load into memory for execution.

- `ld.lld main.o foo.o -o main`

## Closing Statement

I hope this was helpful to understand all the abstractions and convenience that technological pioneers have done for us over the past several decades.

For a final challenge, can you figure out how to build `foo.c` into a shared object instead of an executable with `main.c`?
