# Project Definitions
cmake_minimum_required(VERSION 3.23.2)
project(udp_example)

# Define main executable
add_executable(server)
add_executable(client)

# Define all source files used in building
target_sources(server PRIVATE
    ${CMAKE_CURRENT_LIST_DIR}/server.c
)
target_sources(client PRIVATE
    ${CMAKE_CURRENT_LIST_DIR}/client.c
)

# Set compile options
target_compile_options(server PRIVATE -fPIC)
target_compile_options(client PRIVATE -fPIC)

