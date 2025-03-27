# Shared Libraries in Go
While this isn't a primary feature of Go, the Go compiler supports building shared libraries.

# Building and running this example
> $ go build -buildmode=c-shared -o libfoo.so foo.go
> $ chmod +x libfoo.so
> $ clang -o main main.c
> $ ./main

Note: This example code is intended to work on Unix-like systems only. This will not work on Windows.