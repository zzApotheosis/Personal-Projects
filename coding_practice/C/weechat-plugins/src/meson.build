project('weechat-plugin-example', 'c',
  version : '0.1',
  default_options : ['warning_level=3']
  )

project_sources = files('main.c')

weechat_dep = dependency('weechat')

shared_library(meson.project_name(), project_sources, dependencies : [weechat_dep])
