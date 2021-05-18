# Project declaration
project(
  'util',
  'c'
)

# Define project structure
project_source_files = [
  'util.c'
]

project_headers = [
  'include/util.h'
]

public_headers = include_directories(
  'include'
)

libutil_static = static_library(
  meson.project_name(),
  project_source_files,
  gnu_symbol_visibility : 'hidden',
  include_directories : public_headers
)

libutil_lt_c=1
libutil_lt_r=0
libutil_lt_a=0

libutil_so_version = '@0@.@1@.@2@'.format((libutil_lt_c - libutil_lt_a), libutil_lt_a, libutil_lt_r)

libutil_shared = shared_library(
  meson.project_name(),
  project_source_files + project_headers,
  version : libutil_so_version
)

libutil_dep = declare_dependency(
  include_directories : public_headers,
  link_with : libutil_static
)

