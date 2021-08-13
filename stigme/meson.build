# Project structure
project(
  'stigme',
  'c'
)

# Define project structure
project_source_files = [
  'src/stigme.c'
]

test_source_files = [
  'src/test/all.c'
]

project_header_files = [
  'src/stigme.h'
]

# Dependencies
gtkdep = dependency('gtk4')
project_dependencies = [
  gtkdep
]

build_args = [
]

# Project Target
project_target = executable(
  'stigme',
  project_source_files + project_header_files,
  dependencies : project_dependencies
)

# Test Target
test_target = executable(
  'Test',
  test_source_files + project_header_files,
  dependencies : project_dependencies
)

