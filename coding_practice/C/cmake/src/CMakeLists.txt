cmake_minimum_required (VERSION 3.21.3)
project (HELLO)

add_executable (helloDemo)

target_sources(helloDemo PRIVATE
    main.c
)

target_compile_options(helloDemo PRIVATE -fPIC)

