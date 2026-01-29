# See https://mesonbuild.com/index.html

project('posix-threads-example', 'c',
  version: '0.1',
  default_options: ['warning_level=3']
)

executable(meson.project_name(), ['main.c'], dependencies: [dependency('threads')], install: true)
