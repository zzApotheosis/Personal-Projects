# Project Definitions
cmake_minimum_required(VERSION 3.23.2)
project(const_example)

# Define main executable
add_executable(${CMAKE_PROJECT_NAME})

# Define all source files used in building
target_sources(${CMAKE_PROJECT_NAME} PRIVATE
    ${CMAKE_CURRENT_LIST_DIR}/main.c
)

# Set compile options
target_compile_options(${CMAKE_PROJECT_NAME} PRIVATE -fPIC)

