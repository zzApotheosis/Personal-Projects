# Project Definitions
cmake_minimum_required(VERSION 3.23.2)
project(shm_example)

# Check for required packages, libraries, items, etc.
#find_package(vlc REQUIRED)

# Define main executable
add_executable(shm_send)
add_executable(shm_receive)

# Define all source files used in building
target_sources(shm_send PRIVATE
    ${CMAKE_CURRENT_LIST_DIR}/shm_send.c
)
target_sources(shm_receive PRIVATE
    ${CMAKE_CURRENT_LIST_DIR}/shm_receive.c
)

# Define paths which include needed header files
#target_include_directories(vlc_server PRIVATE
#    # Put paths here
#)

# Define libraries to link
#target_link_libraries(server
#    gcrypt
#    gpg-error
#)
#target_link_libraries(client
#    gcrypt
#    gpg-error
#)

# Set compile options
target_compile_options(shm_send PRIVATE -fPIC)
target_compile_options(shm_receive PRIVATE -fPIC)

