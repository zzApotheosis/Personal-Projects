cmake_minimum_required(VERSION 3.23.1)
project(gtk_practice)

set(EXAMPLE0 "example0")
set(EXAMPLE1 "example1")
set(TEST_BIN "testbin")
set(TEST_LIB "testlib")

add_executable(${EXAMPLE0})
add_executable(${EXAMPLE1})
add_executable(${TEST_BIN})
add_library(${TEST_LIB} SHARED)

target_sources(${EXAMPLE0} PRIVATE example-0.c)
target_sources(${EXAMPLE1} PRIVATE example-1.c)
target_sources(${TEST_BIN} PRIVATE test-bin.c)
target_sources(${TEST_LIB} PRIVATE test-lib.c)

find_package(PkgConfig REQUIRED)
pkg_check_modules(GTK4 REQUIRED gtk4)

include_directories(${GTK4_INCLUDE_DIRS})

target_link_libraries(${EXAMPLE0} ${GTK4_LIBRARIES})
target_link_libraries(${EXAMPLE1} ${GTK4_LIBRARIES})
target_link_libraries(${TEST_LIB} PRIVATE ${GTK4_LIBRARIES})
target_link_libraries(${TEST_BIN} PRIVATE ${TEST_LIB})
