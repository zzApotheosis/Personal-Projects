# CMake configuration options
set(CMAKE_EXPORT_COMPILE_COMMANDS ON) # For LSP

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE "Debug" CACHE STRING "Choose the type of build, options are: Debug Release RelWithDebInfo MinSizeRel." FORCE)
  message(STATUS "CMAKE_BUILD_TYPE not specified, defaulting to Debug")
endif()

# Include CPack related configuration
include(cpack_configuration.cmake)
