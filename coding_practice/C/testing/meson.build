# Project structure
project(
  'mytest',
  'c'
)

# Define project structure
project_source_files = [
  'src/main.c',
  'src/main.h',
]

project_dependencies = [
  dependency('util', fallback : ['util', 'util_dep'])
]

build_args = [
]

# Target
project_target = executable(
  meson.project_name(),
  project_source_files,
  dependencies : project_dependencies
)

test(
  'mytest',
  project_target
)

