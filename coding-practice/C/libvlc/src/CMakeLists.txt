# Project Definitions
cmake_minimum_required(VERSION 3.23.2)
project(vlc_test)
add_definitions(-DDEBUG_LEVEL=3)

# Check for required packages, libraries, items, etc.
#find_package(vlc REQUIRED)

# Define main executable
add_executable(vlc_server)
add_executable(vlc_client)

# Define all source files used in building
target_sources(vlc_server PRIVATE
    ${CMAKE_CURRENT_LIST_DIR}/server.c
)
target_sources(vlc_client PRIVATE
    ${CMAKE_CURRENT_LIST_DIR}/client.c
)

# Define paths which include needed header files
#target_include_directories(vlc_server PRIVATE
#    # Put paths here
#)

# Define libraries to link
target_link_libraries(vlc_server
    vlc
)
target_link_libraries(vlc_client
    vlc
)

# Set compile options
target_compile_options(vlc_server PRIVATE -fPIC)
target_compile_options(vlc_client PRIVATE -fPIC)

