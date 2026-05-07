# See https://mesonbuild.com/index.html

project('mesonexample', 'cpp',
  version: '0.1',
  default_options: ['warning_level=3']
)

lib = library('foo', ['foo.cc'], version: '0.0.1', soversion: '0', install: true)

exe = executable(meson.project_name(), ['main.cc'], link_with: lib, cpp_args: ['-DPROJECT_NAME="' + meson.project_name() + '"'], install: true, install_rpath: '$ORIGIN:$ORIGIN/../lib64:$ORIGIN/../lib')
