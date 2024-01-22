# See https://mesonbuild.com/index.html

project('unions', 'c',
  version : '0.1',
  default_options : ['warning_level=3']
)

lib = library('unions', ['unions.c'], version : '0.0.1', soversion : '0')

executable('onions', ['onions.c'], link_with : lib)
