# Project settings
cmake_minimum_required(VERSION 3.22.0)
project(
  cmake-example
  VERSION 0.0.0.1
  DESCRIPTION "A very basic CMake example project"
  LANGUAGES C CXX
)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON) # For LSP
set(CMAKE_BUILD_TYPE Debug) # Change to MinSizeRel during CMake setup for release builds, e.g., -DCMAKE_BUILD_TYPE=MinSizeRel (-DCMAKE_BUILD_TYPE=Release also works)

# Import custom CMake utilities
add_subdirectory(cmake-utils)

# Recurse into project directories
add_subdirectory(src)

# Build top-level targets (dependencies will be automatically built)

