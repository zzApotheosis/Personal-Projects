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

# Set compile options
target_compile_options(client PRIVATE -fPIC)
target_compile_options(server PRIVATE -fPIC)

