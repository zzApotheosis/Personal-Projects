# Project structure
project('linkedlist', 'c')
libutil_proj = subproject('util')
libutil_dep = libutil_proj.get_variable('libutil_dep')

# Define project structure
source_files = [
  'main.c',
  'main.h',
  'linkedlist.c',
  'linkedlist.h']

executable('a.out', source_files, dependencies : libutil_dep)

