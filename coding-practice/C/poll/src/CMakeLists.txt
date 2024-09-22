# Project Definitions
cmake_minimum_required(VERSION 3.23.2)
project(poll_example)

# Check for required packages, libraries, items, etc.
#find_package(vlc REQUIRED)

# Define main executable
add_executable(poll)

# Define all source files used in building
target_sources(poll PRIVATE
    ${CMAKE_CURRENT_LIST_DIR}/main.c
)

# Set compile options
target_compile_options(poll PRIVATE -fPIC)

