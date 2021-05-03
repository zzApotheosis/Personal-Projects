# Project structure
project(
  'keygen',
  'c'
)

# Define project structure
project_source_files = [
  'src/main.c',
  'src/charset.c'
]

project_header_files = [
  'src/main.h',
  'src/charset.h'
]

project_dependencies = [
]

build_args = [
]

# Target
project_target = executable(
  meson.project_name(),
  project_source_files + project_header_files,
  dependencies : project_dependencies
)
