# See https://mesonbuild.com/index.html

project('musl-libc', 'c',
  version : '0.1',
  default_options : ['warning_level=3']
)

lib = static_library('foo', ['foo.c'])

executable(meson.project_name(), ['main.c'], link_with: lib, link_args: ['-static', '-L/usr/x86_64-linux-musl/lib64'])
