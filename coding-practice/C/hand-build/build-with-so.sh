#!/bin/sh

# Source -> Preprocessed Source
clang -E main.c -o main.i
clang -E foo.c -o foo.i

# Preprocessed Source -> LLVM IR
clang -S main.i -emit-llvm -o main.ll
clang -S foo.i -emit-llvm -o foo.ll

# LLVM IR -> Assembly
llc main.ll -o main.s
llc foo.ll --relocation-model=pic -o foo.s

# Assembly -> Machine Code
llvm-mc main.s -filetype=obj -o main.o
llvm-mc foo.s -filetype=obj --position-independent -o foo.o

# Machine Code -> Shared Object
clang foo.o -shared -o libfoo.so

# Machine Code -> Linked Executable
clang main.o -L. -lfoo -Wl,-rpath='$ORIGIN' -o main
