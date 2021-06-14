# One method
- Create sources (main.c)
- Create configure.ac
- Command: libtoolize
- Command: aclocal
- Command: autoconf
- Create Makefile.am
- Command: automake --add-missing --foreign
- Command: ./configure
- Command: make

# Another method
- Create sources
- Create configure.ac
- Command: libtoolize
- Command: aclocal
- Command: autoreconf --install
- Create Makefile.am files
- Command: automake
- Command ./configure
- Command make

