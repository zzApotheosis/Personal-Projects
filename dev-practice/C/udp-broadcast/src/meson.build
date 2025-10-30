# See https://mesonbuild.com/index.html

project('udp-broadcast', 'c',
  version: '0.1',
  default_options: ['warning_level=3']
)

lib = library('foo', ['foo.c'], version: '0.0.1', soversion: '0', install: true)

client_exe = executable(meson.project_name() + '-client', ['main.c'], link_with: lib, c_args: ['-DPROJECT_NAME="' + meson.project_name() + '"', '-DROLE=0'], install: true, install_rpath: '$ORIGIN:$ORIGIN/../lib64:$ORIGIN/../lib')
server_exe = executable(meson.project_name() + '-server', ['main.c'], link_with: lib, c_args: ['-DPROJECT_NAME="' + meson.project_name() + '"', '-DROLE=1'], install: true, install_rpath: '$ORIGIN:$ORIGIN/../lib64:$ORIGIN/../lib')
