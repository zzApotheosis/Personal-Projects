# Project structure
project(
  'journaldlistener',
  'c'
)

# Define project structure
server_source_files = [
  'src/server.c',
  'src/logger.c',
  'src/journald.c',
  'src/unixsocket.c'
]

client_source_files = [
  'src/client.c',
  'src/logger.c',
  'src/unixsocket.c'
]

project_header_files = [
  'src/server.h',
  'src/server.h',
  'src/logger.h',
  'src/journald.h',
  'src/unixsocket.h'
]

libsystemd_dep = dependency('libsystemd', method : 'pkg-config')

project_dependencies = [
  libsystemd_dep
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

