# See https://mesonbuild.com/index.html

project('meson', 'c',
  version : '0.1',
  default_options : ['warning_level=3']
)

lib = library('foo', ['foo.c'], version : '0.0.1', soversion : '0')

executable('meson_example', ['main.c'], link_with : lib)
