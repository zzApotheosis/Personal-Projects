# Project structure
project(
  'sysloglistener',
  'c'
)

# Define project structure
server_source_files = [
  'src/project.c',
  'src/server.c',
  'src/logger.c',
  'src/unixsocket.c',
  'src/slutil.c'
]

client_source_files = [
  'src/project.c',
  'src/client.c',
  'src/logger.c',
  'src/unixsocket.c',
  'src/slutil.c'
]

test_source_files = [
  'src/test.c',
  'src/slutil.c'
]

project_header_files = [
  'src/project.h',
  'src/server.h',
  'src/server.h',
  'src/logger.h',
  'src/unixsocket.h',
  'src/slutil.h'
]

project_dependencies = [
]

build_args = [
]

# Server Target
server_target = executable(
  'server',
  server_source_files + project_header_files,
  dependencies : project_dependencies
)

# Client Target
client_target = executable(
  'client',
  client_source_files + project_header_files,
  dependencies : project_dependencies
)

# Test Target
test_target = executable(
  'Test',
  test_source_files + project_header_files,
  dependencies : project_dependencies
)

