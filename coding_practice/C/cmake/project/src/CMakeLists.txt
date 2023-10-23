add_executable (helloDemo)

target_sources(helloDemo PRIVATE
    main.c
)

target_compile_options(helloDemo PRIVATE -fPIC)
