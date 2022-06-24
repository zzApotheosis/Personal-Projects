# Project Definitions
cmake_minimum_required(VERSION 3.23.2)
project(syslog_example)

# Check for required packages, libraries, items, etc.
#find_package(vlc REQUIRED)

# Define main executable
add_executable(${CMAKE_PROJECT_NAME})

# Define all source files used in building
target_sources(${CMAKE_PROJECT_NAME} PRIVATE
    ${CMAKE_CURRENT_LIST_DIR}/main.c
)

# Define paths which include needed header files
#target_include_directories(vlc_server PRIVATE
#    # Put paths here
#)

# Define libraries to link
#target_link_libraries(client
#    gcrypt
#    gpg-error
#)

# Set compile options
target_compile_options(${CMAKE_PROJECT_NAME} PRIVATE -fPIC)

