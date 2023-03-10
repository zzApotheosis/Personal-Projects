project('pthreads', 'c',
  version : '0.1',
  default_options : ['warning_level=3']
)

thread_dep = dependency('threads')

executable('pthreads', ['main.c'], dependencies : [thread_dep])
