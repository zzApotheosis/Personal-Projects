# Shared Objects in Nim
It is possible to use Nim to implement functions and export them into usable shared libraries that other languages can leverage.

> $ nim c --app:lib --noMain --out:libfoo.so foo.nim  
> $ clang -Wl,-rpath='$ORIGIN' -L. -lfoo -o main main.c  
> $ ./main
