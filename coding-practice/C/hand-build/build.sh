#!/bin/sh

# Source -> Preprocessed Source
clang -E main.c -o main.i
clang -E foo.c -o foo.i

# Preprocessed Source -> LLVM IR
clang -S main.i -emit-llvm -o main.ll
clang -S foo.i -emit-llvm -o foo.ll

# LLVM IR -> Assembly
llc main.ll -o main.s
llc foo.ll -o foo.s

# Assembly -> Machine Code
llvm-mc main.s -filetype=obj -o main.o
llvm-mc foo.s -filetype=obj -o foo.o

# Machine Code -> Linked Executable
clang main.o foo.o -o main

# Strip unnecessary symbols from final output
strip -s main
