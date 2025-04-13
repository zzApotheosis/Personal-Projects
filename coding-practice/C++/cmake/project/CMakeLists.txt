# Project settings
cmake_minimum_required(VERSION 3.22.0)
project(
  cmake-example
  VERSION 0.0.0.1
  DESCRIPTION "A very basic CMake example project"
  LANGUAGES C CXX
)

# Include project-specific CMake configuration options
add_subdirectory(cmake)

# Recurse into project directories
add_subdirectory(src)
