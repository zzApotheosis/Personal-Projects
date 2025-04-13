add_executable(
  MyApp_deadbeef
)
set_property(TARGET MyApp_deadbeef PROPERTY OUTPUT_NAME "app")

target_sources(
  MyApp_deadbeef
  PRIVATE
  "${CMAKE_CURRENT_LIST_DIR}/main.cpp"
)

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
