project('libgmp-example', 'c', version : '0.1', default_options :
  ['warning_level=3'])

project_sources = files('main.c')

#weechat_dep = dependency('weechat')
libgmp_dep = dependency('gmp')

include_common_macros = include_directories('../../common-macros')

executable(meson.project_name(), project_sources, include_directories :
  [include_common_macros], dependencies : [libgmp_dep])
