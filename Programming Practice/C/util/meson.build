# Project declaration
project(
  'util',
  'c'
)

# Define project structure
project_source_files = [
  'src/stacktoheap.c',
  'src/string.c',
  'src/node.c',
  'src/linkedlist.c'
]

project_headers = [
  'include/util.h'
]

public_headers = include_directories(
  'include'
)

util_static = static_library(
  meson.project_name(),
  project_source_files + project_headers,
  gnu_symbol_visibility : 'hidden',
  include_directories : public_headers
)

util_lt_c=1
util_lt_r=0
util_lt_a=0

util_so_version = '@0@.@1@.@2@'.format((util_lt_c - util_lt_a), util_lt_a, util_lt_r)

util_shared = shared_library(
  meson.project_name(),
  project_source_files + project_headers,
  version : util_so_version
)

util_dep = declare_dependency(
  include_directories : public_headers,
  link_with : util_static
)

