cmake_minimum_required (VERSION 3.21.3)
project (sudoku-project)

add_executable(sudoku-solver)

target_sources(sudoku-solver PRIVATE
    main.c
    cell.c
    cellgroup.c
    board.c
    validity.c
    cellvector.c
    solution.c
)

target_compile_options(sudoku-solver PRIVATE -fPIC)

