# Define the CMake target
add_executable(
  MyApp_deadbeef
)
set_property(TARGET MyApp_deadbeef PROPERTY OUTPUT_NAME "app")

# Define sources used to build this target
target_sources(
  MyApp_deadbeef
  PRIVATE
  "${CMAKE_CURRENT_LIST_DIR}/main.cpp"
)

# Define dependencies needed for this target (include directories, libraries, etc.)
target_include_directories(
  MyApp_deadbeef
  PRIVATE
  $<TARGET_PROPERTY:foo,INTERFACE_INCLUDE_DIRECTORIES>
)
target_link_libraries(
  MyApp_deadbeef
  PRIVATE
  foo
)

# Define installation path relative to CMAKE_INSTALL_PREFIX
install(
  TARGETS MyApp_deadbeef
  DESTINATION "bin"
)
