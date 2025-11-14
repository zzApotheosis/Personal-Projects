# Static Libraries

Use these instructions to build static libraries by hand.

- `$ clang -c -o foo.o foo.c`  
- `$ llvm-ar rcs libfoo.a foo.o`  
- `$ clang -g -o main main.c -L. -lfoo`  
- `$ ./main`

