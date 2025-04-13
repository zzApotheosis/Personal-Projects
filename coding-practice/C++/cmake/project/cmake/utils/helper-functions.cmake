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

function(set_rpath path)
  set(CMAKE_INSTALL_RPATH "${path}" CACHE STRING "Global RPATH for installed targets")
endfunction()
