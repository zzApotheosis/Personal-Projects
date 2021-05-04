# Project structure
project(
  'journaldlistener',
  'c'
)

# Define project structure
project_source_files = [
  'src/main.c',
  'src/journald.c',
  'src/unixsocket.c'
]

project_header_files = [
  'src/main.h',
  'src/journald.h',
  'src/unixsocket.h'
]

libsystemd_dep = dependency('libsystemd', method : 'pkg-config')

project_dependencies = [
  libsystemd_dep
]

build_args = [
]

# Target
project_target = executable(
  meson.project_name(),
  project_source_files + project_header_files,
  dependencies : project_dependencies
)
