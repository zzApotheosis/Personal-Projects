# Project Definitions
cmake_minimum_required(VERSION 3.23.2)
project(gnutls_example)

# Check for required packages, libraries, items, etc.
#find_package(vlc REQUIRED)

# Define main executable
add_executable(dtls_client)
add_executable(dtls_server)
add_executable(tls_client)
add_executable(tls_server)

# Define all source files used in building
target_sources(dtls_client PRIVATE
    ${CMAKE_CURRENT_LIST_DIR}/dtls_client.c
    ${CMAKE_CURRENT_LIST_DIR}/udp.c
)
target_sources(dtls_server PRIVATE
    ${CMAKE_CURRENT_LIST_DIR}/dtls_server.c
    ${CMAKE_CURRENT_LIST_DIR}/udp.c
)
target_sources(tls_client PRIVATE
    ${CMAKE_CURRENT_LIST_DIR}/tls_client.c
    ${CMAKE_CURRENT_LIST_DIR}/tcp.c
)
target_sources(tls_server PRIVATE
    ${CMAKE_CURRENT_LIST_DIR}/tls_server.c
    ${CMAKE_CURRENT_LIST_DIR}/tcp.c
)

# Define libraries to link
target_link_libraries(dtls_client
    gnutls
)
target_link_libraries(dtls_server
    gnutls
)
target_link_libraries(tls_client
    gnutls
)
target_link_libraries(tls_server
    gnutls
)

# Copy files to build dir
configure_file(${CMAKE_CURRENT_LIST_DIR}/root.key ${CMAKE_BINARY_DIR}/root.key COPYONLY)
configure_file(${CMAKE_CURRENT_LIST_DIR}/root.crt ${CMAKE_BINARY_DIR}/root.crt COPYONLY)
configure_file(${CMAKE_CURRENT_LIST_DIR}/server.key ${CMAKE_BINARY_DIR}/server.key COPYONLY)
configure_file(${CMAKE_CURRENT_LIST_DIR}/server.req ${CMAKE_BINARY_DIR}/server.req COPYONLY)
configure_file(${CMAKE_CURRENT_LIST_DIR}/server.crt ${CMAKE_BINARY_DIR}/server.crt COPYONLY)
configure_file(${CMAKE_CURRENT_LIST_DIR}/client.key ${CMAKE_BINARY_DIR}/client.key COPYONLY)
configure_file(${CMAKE_CURRENT_LIST_DIR}/client.req ${CMAKE_BINARY_DIR}/client.req COPYONLY)
configure_file(${CMAKE_CURRENT_LIST_DIR}/client.crt ${CMAKE_BINARY_DIR}/client.crt COPYONLY)

# Set compile options
target_compile_options(dtls_client PRIVATE -fPIC)
target_compile_options(dtls_server PRIVATE -fPIC)
target_compile_options(tls_client PRIVATE -fPIC)
target_compile_options(tls_server PRIVATE -fPIC)

