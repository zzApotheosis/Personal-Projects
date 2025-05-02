# Project Definitions
cmake_minimum_required(VERSION 3.23.2)
project(eventfd_example)

# Check for required packages, libraries, items, etc.
#find_package(vlc REQUIRED)

# Define main executable
add_executable(forked)
add_executable(single_thread)

# Define all source files used in building
target_sources(forked PRIVATE
    ${CMAKE_CURRENT_LIST_DIR}/forked.c
)
target_sources(single_thread PRIVATE
    ${CMAKE_CURRENT_LIST_DIR}/single_thread.c
)

# Set compile options
target_compile_options(forked PRIVATE -fPIC)
target_compile_options(single_thread PRIVATE -fPIC)

