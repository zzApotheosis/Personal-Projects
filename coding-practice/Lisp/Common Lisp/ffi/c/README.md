# Foreign Function Interface
SBCL natively supports a foreign function interface to load shared objects at runtime and execute compiled code. Obviously this functionality is not defined in the ANSI Common Lisp standard; it is a fully featured extension of SBCL.

# Building and Running this Example
Use these steps to build and run this FFI example on Linux systems. This will also probably work on Darwin and BSD systems as well.

> $ gcc -shared -fPIC -o libfoo.so foo.c  
> $ ./ffi.sh