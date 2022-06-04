# Project Definitions
cmake_minimum_required(VERSION 3.23.2)
project(challenge_response_hmac)

# Check for required packages, libraries, items, etc.
#find_package(vlc REQUIRED)

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

# Define paths which include needed header files
#target_include_directories(vlc_server PRIVATE
#    # Put paths here
#)

# Define libraries to link
target_link_libraries(server
    gcrypt
    gpg-error
)
target_link_libraries(client
    gcrypt
    gpg-error
)

configure_file(${CMAKE_CURRENT_LIST_DIR}/test.key ${CMAKE_BINARY_DIR}/test.key COPYONLY)

# Set compile options
target_compile_options(server PRIVATE -fPIC)
target_compile_options(client PRIVATE -fPIC)

