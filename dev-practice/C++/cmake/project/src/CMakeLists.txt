# Libraries
set_rpath("\$ORIGIN:\$ORIGIN/../lib")
add_subdirectory(bar)
add_subdirectory(foo)

# Executables
set_rpath("\$ORIGIN")
add_subdirectory(app)

# Custom function defined in cmake directory
create_install_symlinks()
