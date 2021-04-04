ACLOCAL_AMFLAGS = -I m4

lib_LTLIBRARIES = libfoo.la
libfoo_la_SOURCES = foo.c
libfoo_la_LDFLAGS = -shared

bin_PROGRAMS = tut_prog
tut_prog_SOURCES = main.c foo.c
tut_prog_LDADD = libfoo.a
