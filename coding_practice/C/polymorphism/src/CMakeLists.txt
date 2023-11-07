add_executable (polymorphism)

target_sources(polymorphism PRIVATE
        main.c
)

target_compile_options(polymorphism PRIVATE -fPIC)
