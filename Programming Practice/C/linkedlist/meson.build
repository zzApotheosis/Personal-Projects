# Project structure
project(
  'linkedlist',
  'c'
)

# Define project structure
project_source_files = [
  'src/main.c',
  'src/main.h',
  'src/linkedlist.c',
  'src/linkedlist.h',
  'src/node.c',
  'src/node.h'
]

project_dependencies = [
  dependency('libutil', fallback : ['libutil', 'libutil_dep'])
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
  'basic',
  project_target
)

