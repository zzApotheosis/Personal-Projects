cmake_minimum_required (VERSION 3.21.3)
project (sudoku_project)

add_executable(sudoku_solver)

target_sources(sudoku_solver PRIVATE
    main.c
    cell.c
    cellgroup.c
    board.c
    validity.c
    cellvector.c
    solution.c
)

target_compile_options(sudoku_solver PRIVATE -fPIC)

