# See https://mesonbuild.com/index.html

project('mesonexample', 'c',
  version: '0.1',
  default_options: ['warning_level=3']
)

gnutls_dep = dependency('gnutls')

lib = library('foo', ['foo.c'], dependencies: [gnutls_dep], version: '0.0.1', soversion: '0', install: true)

server_exe = executable(meson.project_name() + '-server', ['main.c'], link_with: lib, c_args: ['-DPROJECT_NAME="' + meson.project_name() + '"', '-DSERVER'], install: true, install_rpath: '$ORIGIN:$ORIGIN/../lib64:$ORIGIN/../lib')
client_exe = executable(meson.project_name() + '-client', ['main.c'], link_with: lib, c_args: ['-DPROJECT_NAME="' + meson.project_name() + '"', '-DCLIENT'], install: true, install_rpath: '$ORIGIN:$ORIGIN/../lib64:$ORIGIN/../lib')
