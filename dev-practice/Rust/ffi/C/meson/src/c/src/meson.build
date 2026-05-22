# See https://mesonbuild.com/index.html

project('rust-mesonexample', 'c',
  version: '0.1',
  default_options: ['warning_level=3']
)

# lib = library('foo', ['foo.c'], version: '0.0.1', soversion: '0', install: true)
lib = static_library('foo', ['foo.c'], install: true)
