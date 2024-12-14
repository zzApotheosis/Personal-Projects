# See https://mesonbuild.com/index.html

project('hello-world', 'c',
  version : '0.1',
  default_options : ['warning_level=3']
)

lib = library('foo', ['foo.c'], version : '0.0.1', soversion : '0')

executable('hello-world', ['main.c'], link_with : lib)
