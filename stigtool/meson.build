# Project structure
project(
  'stigtool',
  'c'
)

# Define project structure
project_source_files = [
  'src/stigtool.c'
]

test_source_files = [
  'src/test/all.c'
]

project_header_files = [
  'src/stigtool.h'
]

project_dependencies = [
]

build_args = [
]

# Project Target
project_target = executable(
  'stigtool',
  project_source_files + project_header_files,
  dependencies : project_dependencies
)

# Test Target
test_target = executable(
  'Test',
  test_source_files + project_header_files,
  dependencies : project_dependencies
)

