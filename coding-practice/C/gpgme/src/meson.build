# See https://mesonbuild.com/index.html

project('gpgme-example', 'c',
  version : '0.1',
  default_options : ['warning_level=3']
)

include_directories = include_directories('../../common-macros')

gpgme_lib = dependency('gpgme', version : '>=1.15.1')

lib = library('gpgme-example',
  sources : ['gpgme-example.c',
             'funcnode.c',
             'functionlist.c'],
  version : '0.0.1',
  soversion : '0',
  include_directories : include_directories,
  dependencies : [gpgme_lib]
)

executable('gpgme-example',
  sources : ['main.c'],
  link_with : lib,
  include_directories : include_directories
)
