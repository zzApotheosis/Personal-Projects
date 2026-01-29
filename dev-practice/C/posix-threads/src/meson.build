project('posix-threads-example', 'c',
  version : '0.1',
  default_options : ['warning_level=3']
)

thread_dep = dependency('threads')

executable(meson.project_name(), ['main.c'], dependencies : [thread_dep])
