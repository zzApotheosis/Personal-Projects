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

These next steps convert the preprocessed code into LLVM's Intermediary Representation (IR) format.

- `clang -S -emit-llvm main.i -o main.ll`
- `clang -S -emit-llvm foo.i -o foo.ll`

Next, these steps convert LLVM IR to Assembly.

- `llc main.ll -o main.s`
- `llc foo.ll -o foo.s`

### Assembling

The LLVM assembler converts the Assembly source text into machine code.

- `llvm-mc -filetype=obj main.s -o main.o`
- `llvm-mc -filetype=obj foo.s -o foo.o`

### Linking

Finally, the linker maps memory spaces together so the final output is ready to load into memory for execution. This final step is very complicated because it is highly dependent on the system it runs on. It will be much easier to ask the compiler `clang` what it "would" invoke in order to finally link the compiled object files into a ready-to-execute format.

- `clang -### main.o foo.o -o main`

Then copy the command it produces and replace the first token of the command with `ld.lld`. This is the LLVM linker. It's possible that Clang would invoke another linker on the system, especially if GCC is also installed. Both approaches would work just fine, but this example is strictly adhering to LLVM technologies.

Invoking the long command should yield a `main` binary executable file.

Or, it may be easier to just invoke `clang` to finish the job for us.

- `clang main.o foo.o -o main`

### Execution

With the final product of the linker, we can now load it into memory for execution.

- `./main`

Expected:
> `5^2 = 25`  
> `Hello, Manual Build!`

## GCC Toolchain

This shows how to use the GCC toolchain to preprocess, compile, assemble, and link this example project.

### Preprocessing

This converts the original source code into preprocessed code.

- `gcc -E main.c -o main.i`
- `gcc -E foo.c -o foo.i`

### Compiling

These next steps convert the preprocessed code directly to Assembly.

- `gcc -S main.i -o main.s`
- `gcc -S foo.i -o foo.s`

### Assembling

The GNU Assembler converts the Assembly source text into machine code.

- `as main.s -o main.o`
- `as foo.s -o foo.o`

### Linking

Finally, the linker maps memory spaces together so the final output is ready to load into memory for execution. This final step is very complicated because it is highly dependent on the system it runs on. It will be much easier to ask the compiler `gcc` what it "would" invoke in order to finally link the compiled object files into a ready-to-execute format.

- `gcc -### main.o foo.o -o main`

Then copy the command it produces and replace the first token of the command with `ld`. It's possible it will already be an `ld` command. This is the GNU Linker. It's possible that GCC would invoke another linker on the system. In theory, the linker shouldn't matter too much, so long as the final binary output is able to load into memory and execute instructions. At the time of writing this, my GCC setup showed a `collect2` command of some sort, which also worked, but I found that replacing `collect2` with `ld` worked just fine.

Or, it may be easier to just invoke `gcc` to finish the job for us, since the linker of choice doesn't matter that much, so long as `gcc` invokes one of them.

- `gcc main.o foo.o -o main`

### Execution

With the final product of the linker, we can now load it into memory for execution.

- `./main`

Expected:
> `5^2 = 25`  
> `Hello, Manual Build!`

## Closing Statement

I hope this was helpful to understand all the abstractions and convenience that technological pioneers have done for us over the past several decades.

For a final challenge, can you figure out how to build `foo.c` into a shared object instead of linking with `main.c`?
