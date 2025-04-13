# Define the CMake target
add_library(
  bar
  SHARED
)

# Define sources used to build this target
target_sources(
  bar
  PRIVATE
  "${CMAKE_CURRENT_LIST_DIR}/bar.cpp"
)

# Define dependencies needed for this target (include directories, libraries, etc.)
# N/A

# Define directories that the rest of this project can include for compile-time dependencies
set(
  bar_INCLUDE_DIRS
  "${CMAKE_CURRENT_LIST_DIR}"
)
set_property(TARGET bar PROPERTY INTERFACE_INCLUDE_DIRECTORIES ${bar_INCLUDE_DIRS})

# Define installation path relative to CMAKE_INSTALL_PREFIX
install(
  TARGETS bar
  DESTINATION "lib64"
)
