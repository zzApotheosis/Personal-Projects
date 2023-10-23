add_library(mylib SHARED)

target_sources(mylib PRIVATE
  mylib.c
)

target_compile_options(mylib PRIVATE -fPIC)
