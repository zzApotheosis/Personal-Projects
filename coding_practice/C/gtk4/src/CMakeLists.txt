add_executable(my_test)

target_sources(my_test PRIVATE basic.c)

find_package(PkgConfig REQUIRED)
pkg_check_modules(GTK4 REQUIRED gtk4)

include_directories(${GTK4_INCLUDE_DIRS})
link_directories(${GTK4_LIBRARY_DIRS})

add_definitions(${GTK4_CFLAGS_OTHER})

target_link_libraries(my_test ${GTK4_LIBRARIES})
