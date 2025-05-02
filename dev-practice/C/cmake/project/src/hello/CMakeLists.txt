add_executable (hello)

target_sources(hello PRIVATE
  main.c
)

target_include_directories(hello PRIVATE
  "../mylib"
)

target_link_libraries(hello mylib)

target_compile_options(hello PRIVATE -fPIC)
