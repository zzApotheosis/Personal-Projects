project('weechat-plugin-example', 'c',
  version : '0.1',
  default_options : ['warning_level=3']
  )

project_sources = files('main.c')

weechat_dep = dependency('weechat')

include_common_macros = include_directories('../../common-macros')

shared_library(meson.project_name(), project_sources, include_directories : [include_common_macros], dependencies : [weechat_dep])
