# Project Definitions
cmake_minimum_required(VERSION 3.23.2)
project(gnutls_example)

# Check for required packages, libraries, items, etc.
#find_package(vlc REQUIRED)

# Define main executable
add_executable(client)
add_executable(server)

# Define all source files used in building
target_sources(client PRIVATE
    ${CMAKE_CURRENT_LIST_DIR}/client.c
    ${CMAKE_CURRENT_LIST_DIR}/udp.c
)
target_sources(server PRIVATE
    ${CMAKE_CURRENT_LIST_DIR}/server.c
    ${CMAKE_CURRENT_LIST_DIR}/udp.c
)

# Define libraries to link
target_link_libraries(client
    gnutls
)
target_link_libraries(server
    gnutls
)

# Copy files to build dir
configure_file(${CMAKE_CURRENT_LIST_DIR}/root.key ${CMAKE_BINARY_DIR}/root.key COPYONLY)
configure_file(${CMAKE_CURRENT_LIST_DIR}/root.crt ${CMAKE_BINARY_DIR}/root.crt COPYONLY)
configure_file(${CMAKE_CURRENT_LIST_DIR}/server.key ${CMAKE_BINARY_DIR}/server.key COPYONLY)
configure_file(${CMAKE_CURRENT_LIST_DIR}/server.req ${CMAKE_BINARY_DIR}/server.req COPYONLY)
configure_file(${CMAKE_CURRENT_LIST_DIR}/server.crt ${CMAKE_BINARY_DIR}/server.crt COPYONLY)
configure_file(${CMAKE_CURRENT_LIST_DIR}/client.key ${CMAKE_BINARY_DIR}/client.key COPYONLY)
configure_file(${CMAKE_CURRENT_LIST_DIR}/client.req ${CMAKE_BINARY_DIR}/client.req COPYONLY)
configure_file(${CMAKE_CURRENT_LIST_DIR}/client.crt ${CMAKE_BINARY_DIR}/client.crt COPYONLY)

# Set compile options
target_compile_options(client PRIVATE -fPIC)
target_compile_options(server PRIVATE -fPIC)

