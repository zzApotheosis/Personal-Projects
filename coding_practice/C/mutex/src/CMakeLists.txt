# Project Definitions
cmake_minimum_required(VERSION 3.23.2)
project(mutex_example)

# Find packages
find_package(Threads)

# Define main executable
add_executable(${CMAKE_PROJECT_NAME})

# Define all source files used in building
target_sources(${CMAKE_PROJECT_NAME} PRIVATE
    ${CMAKE_CURRENT_LIST_DIR}/main.c
)

# Define libraries to link
target_link_libraries(${CMAKE_PROJECT_NAME}
    #${CMAKE_THREAD_LIBS_INIT}
    Threads::Threads
)

# Set compile options
target_compile_options(${CMAKE_PROJECT_NAME} PRIVATE -fPIC)

