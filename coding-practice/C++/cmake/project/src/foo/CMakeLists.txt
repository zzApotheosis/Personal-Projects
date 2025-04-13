# Define the CMake target
add_library(
  foo
  SHARED
)

# Define sources used to build this target
target_sources(
  foo
  PRIVATE
  "${CMAKE_CURRENT_LIST_DIR}/foo.cpp"
)

# Define dependencies needed for this target (include directories, libraries, etc.)
target_include_directories(
  foo
  PRIVATE
  $<TARGET_PROPERTY:bar,INTERFACE_INCLUDE_DIRECTORIES>
)
target_link_libraries(
  foo
  PRIVATE
  bar
)

# Define directories that the rest of this project can include for compile-time dependencies
set(
  foo_INCLUDE_DIRS
  "${CMAKE_CURRENT_LIST_DIR}"
)
set_property(TARGET foo PROPERTY INTERFACE_INCLUDE_DIRECTORIES ${foo_INCLUDE_DIRS})
