# Project declaration
project('libutil', 'c')

# Define project structure
source_files = [
  'util.c',
  'util.h']

libutil_static = static_library('util', source_files)

libutil_lt_c=1
libutil_lt_r=0
libutil_lt_a=0

libutil_so_version = '@0@.@1@.@2@'.format((libutil_lt_c - libutil_lt_a), libutil_lt_a, libutil_lt_r)

libutil_shared = shared_library('util', source_files, version : libutil_so_version)

libutil_dep = declare_dependency(link_with : libutil_static)

