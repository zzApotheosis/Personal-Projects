# Project structure
project('linkedlist', 'c')
libutil_proj = subproject('util')
libutil_dep = libutil_proj.get_variable('libutil_dep')

# Define project structure
linkedlist_source_files = [
  'linkedlist/main.c',
  'linkedlist/main.h',
  'linkedlist/linkedlist.c',
  'linkedlist/linkedlist.h']

executable('a.out', source_files, dependencies : libutil_dep)

