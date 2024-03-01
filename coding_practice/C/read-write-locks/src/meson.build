project('read-write-locks', 'c',
  version : '0.1',
  default_options : ['warning_level=3']
)

project_name = 'read-write-locks'

thread_dep = dependency('threads')

counter_sources = files('counter.c')

counter_lib = static_library('counter', counter_sources, include_directories : ['../../common-macros'])

executable('read-write-locks', ['main.c'], include_directories : ['../../common-macros'], dependencies : [thread_dep], link_with : counter_lib)
