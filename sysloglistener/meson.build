# Project structure
project(
  'sysloglistener',
  'c'
)

# Define project structure
server_source_files = [
  'src/server.c',
  'src/logger.c',
  'src/unixsocket.c',
  'src/jdlutil.c'
]

client_source_files = [
  'src/client.c',
  'src/logger.c',
  'src/unixsocket.c',
  'src/jdlutil.c'
]

project_header_files = [
  'src/server.h',
  'src/server.h',
  'src/logger.h',
  'src/unixsocket.h',
  'src/jdlutil.h'
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

