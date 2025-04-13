set(_PROJECT_TOPLEVEL_TARGETS_)

function(add_toplevel_target target)
  list(APPEND _PROJECT_TOPLEVEL_TARGETS_ target)
endfunction()

function(run_post_install_code code)
  install(
    CODE ${code}
  )
endfunction()

function(create_install_symlinks)
  run_post_install_code(
    "execute_process(
      COMMAND ${CMAKE_COMMAND} -E create_symlink lib64 \"${CMAKE_INSTALL_PREFIX}/lib\"
    )"
  )
endfunction()


