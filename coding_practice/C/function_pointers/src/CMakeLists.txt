# Project Definitions
cmake_minimum_required(VERSION 3.23.2)
project(function_pointer_example)

# Define main executable
add_executable(${CMAKE_PROJECT_NAME})
add_executable(function_pointer_array_example)

# Define all source files used in building
target_sources(${CMAKE_PROJECT_NAME} PRIVATE
    ${CMAKE_CURRENT_LIST_DIR}/main.c
)
target_sources(function_pointer_array_example PRIVATE
    ${CMAKE_CURRENT_LIST_DIR}/array.c
)

# Set compile options
target_compile_options(${CMAKE_PROJECT_NAME} PRIVATE -fPIC)
target_compile_options(function_pointer_array_example PRIVATE -fPIC)

